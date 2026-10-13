open! Core
module Amount = Money.Amount
module Account_id = Ledger.Account_id

module Chart = struct
  type t =
    { settlement : Rail.t -> Money.Currency.t -> Account_id.t
    ; fee_income : Money.Currency.t -> Account_id.t
    }

  (* Rail.to_string has a ':' for on-chain rails ("onchain:bitcoin"); ids allow it. *)
  let default =
    { settlement =
        (fun rail currency ->
          Account_id.of_string
            (sprintf "settlement:%s/%s" (Rail.to_string rail) (Money.Currency.to_string currency)))
    ; fee_income =
        (fun currency ->
          Account_id.of_string (sprintf "income:fees/%s" (Money.Currency.to_string currency)))
    }
  ;;

  let accounts t ~rails ~currencies =
    List.concat_map currencies ~f:(fun currency ->
      Ledger.Account.create (t.fee_income currency) Income
      :: List.map rails ~f:(fun rail ->
        Ledger.Account.create ~allow_negative:true (t.settlement rail currency) Asset))
    |> List.dedup_and_sort ~compare:(fun a b ->
      Account_id.compare a.Ledger.Account.id b.Ledger.Account.id)
  ;;
end

let hold_id (t : Transfer.t) =
  Ledger.Hold_id.of_string ("transfer:" ^ Transfer.Id.to_string t.id)
;;

let apply balances entries =
  List.fold_result entries ~init:balances ~f:(fun b entry ->
    Ledger.Balances.apply b { seq = Ledger.Balances.next_seq b; event = Posted entry })
;;

(* Builds legs one after the other against a scratch copy of the balances, because a
   later leg (the fee capture) depends on the state the earlier one leaves. *)
let build balances legs =
  let open Or_error.Let_syntax in
  let%map _, rev =
    List.fold_result legs ~init:(balances, []) ~f:(fun (b, acc) leg ->
      let%bind entry = leg b in
      let%map b = apply b [ entry ] in
      b, entry :: acc)
  in
  List.rev rev
;;

let key ~event_id leg = Ledger.Idempotency_key.of_string_or_error (event_id ^ "/" ^ leg)

let transfer_entry ~tag ~event_id ~(t : Transfer.t) ~at ~debit ~credit amount _balances =
  let open Or_error.Let_syntax in
  let%bind idempotency_key = key ~event_id tag in
  Ledger.Entry.create
    ~idempotency_key
    ~kind:(Transfer tag)
    ~reference:(Transfer.Id.to_string t.id)
    ~effective_at:at
    ~recorded_at:at
    [ Ledger.Posting.debit debit amount; Ledger.Posting.credit credit amount ]
;;

let place ~event_id ~(t : Transfer.t) ~at balances =
  let open Or_error.Let_syntax in
  let%bind idempotency_key = key ~event_id "hold" in
  Ledger.Hold.place
    balances
    ~hold_id:(hold_id t)
    ~account:t.account
    ~amount:(Transfer.total_debit t)
    ~idempotency_key
    ~reference:(Transfer.Id.to_string t.id)
    ~effective_at:at
    ~recorded_at:at
    ()
;;

let capture ~leg ~to_ ~amount ~event_id ~(t : Transfer.t) ~at balances =
  let open Or_error.Let_syntax in
  let%bind idempotency_key = key ~event_id leg in
  Ledger.Hold.capture
    balances
    ~hold_id:(hold_id t)
    ~amount
    ~to_
    ~idempotency_key
    ~reference:(Transfer.Id.to_string t.id)
    ~effective_at:at
    ~recorded_at:at
    ()
;;

let release ~event_id ~(t : Transfer.t) ~at balances =
  let open Or_error.Let_syntax in
  let%bind idempotency_key = key ~event_id "release" in
  Ledger.Hold.release
    balances
    ~hold_id:(hold_id t)
    ~idempotency_key
    ~reference:(Transfer.Id.to_string t.id)
    ~effective_at:at
    ~recorded_at:at
    ()
;;

let currency (t : Transfer.t) = Amount.currency t.amount
let settlement (chart : Chart.t) (t : Transfer.t) = chart.settlement t.rail (currency t)

let fee_legs (t : Transfer.t) leg = if Amount.is_zero t.fee then [] else [ leg ]

let deposit_legs chart ~event_id ~(t : Transfer.t) ~at =
  transfer_entry
    ~tag:"deposit"
    ~event_id
    ~t
    ~at
    ~debit:(settlement chart t)
    ~credit:t.account
    t.amount
  :: fee_legs
       t
       (transfer_entry
          ~tag:"fee"
          ~event_id
          ~t
          ~at
          ~debit:t.account
          ~credit:(chart.fee_income (currency t))
          t.fee)
;;

let legs (chart : Chart.t) ~(t : Transfer.t) ~(to_ : Transfer.Status.t) ~event_id ~at =
  match t.direction, t.status, to_ with
  | Out, (Draft | Awaiting_sca), Submitted -> [ place ~event_id ~t ~at ]
  | Out, (Submitted | Pending), Settled ->
    capture ~leg:"capture" ~to_:(settlement chart t) ~amount:t.amount ~event_id ~t ~at
    :: fee_legs
         t
         (capture
            ~leg:"fee"
            ~to_:(chart.fee_income (currency t))
            ~amount:t.fee
            ~event_id
            ~t
            ~at)
  | Out, (Submitted | Pending), Failed _ -> [ release ~event_id ~t ~at ]
  | Out, Settled, Returned ->
    [ transfer_entry
        ~tag:"return"
        ~event_id
        ~t
        ~at
        ~debit:(settlement chart t)
        ~credit:t.account
        t.amount
    ]
  | In, Pending, Settled -> deposit_legs chart ~event_id ~t ~at
  | In, Settled, Returned ->
    [ transfer_entry
        ~tag:"return"
        ~event_id
        ~t
        ~at
        ~debit:t.account
        ~credit:(settlement chart t)
        t.amount
    ]
  (* Every other allowed transition moves no money: entering Awaiting_sca or Pending,
     or giving up before the hold was placed. *)
  | _ -> []
;;

let transition chart balances (t : Transfer.t) ~to_ ~event_id ~at =
  if not (Transfer.can_transition t to_)
  then
    Or_error.error_s
      [%message
        "transition not allowed"
          ~transfer:(t.id : Transfer.Id.t)
          ~from:(t.status : Transfer.Status.t)
          (to_ : Transfer.Status.t)]
  else build balances (legs chart ~t ~to_ ~event_id ~at)
;;

let created chart balances (t : Transfer.t) ~event_id ~at =
  match t.direction, t.status with
  | In, Settled -> build balances (deposit_legs chart ~event_id ~t ~at)
  | _ -> Ok []
;;

(* Stops at the first failure: later legs were built assuming the earlier ones booked. *)
let post journal entries =
  List.fold_result entries ~init:[] ~f:(fun acc entry ->
    Or_error.map (Ledger.Journal.append_entry journal entry) ~f:(fun r -> r :: acc))
  |> Or_error.map ~f:List.rev
;;
