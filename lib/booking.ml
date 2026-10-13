open! Core
module Amount = Money.Amount
module Account_id = Ledger.Account_id

module Chart = struct
  type t =
    { settlement : Rail.t -> Money.Currency.t -> Account_id.t
    ; fee_income : Money.Currency.t -> Account_id.t
    ; fx_position : Money.Currency.t -> Account_id.t
    }

  (* Rail.to_string has a ':' for on-chain rails ("onchain:bitcoin"); ids allow it. *)
  let default =
    { settlement =
        (fun rail currency ->
          Account_id.of_string
            (sprintf
               "settlement:%s/%s"
               (Rail.to_string rail)
               (Money.Currency.to_string currency)))
    ; fee_income =
        (fun currency ->
          Account_id.of_string
            (sprintf "income:fees/%s" (Money.Currency.to_string currency)))
    ; fx_position =
        (fun currency ->
          Account_id.of_string
            (sprintf "fx:position/%s" (Money.Currency.to_string currency)))
    }
  ;;

  let accounts t ~rails ~currencies =
    List.concat_map currencies ~f:(fun currency ->
      Ledger.Account.create (t.fee_income currency) Income
      :: Ledger.Account.create ~allow_negative:true (t.fx_position currency) Asset
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

(* Builds legs one after the other against a scratch copy of the balances, because a later
   leg (the fee capture) depends on the state the earlier one leaves. *)
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

(* How the payment was authorised, kept on the entry for the PSD2 art. 74(2) trail. *)
let sca_evidence (t : Transfer.t) : Ledger.Sca_evidence.t option =
  match t.authorization with
  | None -> None
  | Some (Sca id) -> Some (Challenge (Sca.Id.to_string id))
  | Some (Exempt e) ->
    Some
      (Exemption
         (sprintf
            "%s %s"
            (Sca.Exemption.article e)
            (Sexp.to_string [%sexp (e : Sca.Exemption.t)])))
;;

let transfer_entry
  ~tag
  ~code
  ~event_id
  ~(t : Transfer.t)
  ~at
  ~debit
  ~credit
  amount
  _balances
  =
  let open Or_error.Let_syntax in
  let%bind idempotency_key = key ~event_id tag in
  Ledger.Entry.create
    ~idempotency_key
    ~kind:(Transfer tag)
    ~code
    ?sca:(sca_evidence t)
    ~reference:(Transfer.Id.to_string t.id)
    ~effective_at:at
    ~recorded_at:at
    [ Ledger.Posting.debit debit amount; Ledger.Posting.credit credit amount ]
;;

let withdrawal_code (t : Transfer.t) : Ledger.Entry.Code.t =
  match t.rail with
  | Intraledger -> Transfer_intraledger
  | _ -> Withdrawal
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
    ~code:(withdrawal_code t)
    ~reference:(Transfer.Id.to_string t.id)
    ~effective_at:at
    ~recorded_at:at
    ()
;;

(* One entry pays the network and the fee account from the hold, so the two can never be
   half-booked; any remainder (none for a full settlement) is released with it. *)
let capture_split ~payees ~event_id ~(t : Transfer.t) ~at balances =
  let open Or_error.Let_syntax in
  let%bind idempotency_key = key ~event_id "capture" in
  Ledger.Hold.capture_split
    balances
    ~hold_id:(hold_id t)
    ~release_remainder:true
    payees
    ~idempotency_key
    ~code:(withdrawal_code t)
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
    ~code:Deposit
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
          ~code:Fee
          ~event_id
          ~t
          ~at
          ~debit:t.account
          ~credit:(chart.fee_income (currency t))
          t.fee)
;;

let return_code : Transfer.Status.t -> Ledger.Entry.Code.t = function
  | Recalled -> Recall
  | _ -> Return
;;

let legs (chart : Chart.t) ~(t : Transfer.t) ~(to_ : Transfer.Status.t) ~event_id ~at =
  match t.direction, t.status, to_ with
  | Out, (Draft | Awaiting_sca), Submitted -> [ place ~event_id ~t ~at ]
  | Out, (Submitted | Pending), Settled ->
    [ capture_split
        ~payees:
          ((settlement chart t, t.amount)
           :: fee_legs t (chart.fee_income (currency t), t.fee))
        ~event_id
        ~t
        ~at
    ]
  | Out, (Submitted | Pending), (Failed _ | Rejected _ | Timed_out) ->
    [ release ~event_id ~t ~at ]
  (* A settlement confirmed after the SCT Inst time-out: the hold is gone, so the payment
     is booked as a fresh debit. It can fail if the customer has spent the released funds
     meanwhile; the caller then has a case to open, not an entry. *)
  | Out, Timed_out, Settled ->
    transfer_entry
      ~tag:"late-settlement"
      ~code:(withdrawal_code t)
      ~event_id
      ~t
      ~at
      ~debit:t.account
      ~credit:(settlement chart t)
      t.amount
    :: fee_legs
         t
         (transfer_entry
            ~tag:"fee"
            ~code:Fee
            ~event_id
            ~t
            ~at
            ~debit:t.account
            ~credit:(chart.fee_income (currency t))
            t.fee)
  | Out, (Settled | Recall_requested _), (Returned _ | Recalled) ->
    [ transfer_entry
        ~tag:"return"
        ~code:(return_code to_)
        ~event_id
        ~t
        ~at
        ~debit:(settlement chart t)
        ~credit:t.account
        t.amount
    ]
  | In, Pending, Settled -> deposit_legs chart ~event_id ~t ~at
  (* A credited deposit whose block was orphaned: take the credit (and the fee) back until
     the transaction confirms again. *)
  | In, Settled, Reorged ->
    fee_legs
      t
      (transfer_entry
         ~tag:"reorg-fee-refund"
         ~code:Refund
         ~event_id
         ~t
         ~at
         ~debit:(chart.fee_income (currency t))
         ~credit:t.account
         t.fee)
    @ [ transfer_entry
          ~tag:"reorg"
          ~code:Reversal
          ~event_id
          ~t
          ~at
          ~debit:t.account
          ~credit:(settlement chart t)
          t.amount
      ]
  | In, (Settled | Recall_requested _), (Returned _ | Recalled) ->
    (* Sending an incoming payment back undoes it, so the fee charged on arrival is
       refunded first; otherwise a customer who received exactly the amount could not
       cover the return. *)
    fee_legs
      t
      (transfer_entry
         ~tag:"fee-refund"
         ~code:Refund
         ~event_id
         ~t
         ~at
         ~debit:(chart.fee_income (currency t))
         ~credit:t.account
         t.fee)
    @ [ transfer_entry
          ~tag:"return"
          ~code:(return_code to_)
          ~event_id
          ~t
          ~at
          ~debit:t.account
          ~credit:(settlement chart t)
          t.amount
      ]
  (* Every other allowed transition moves no money: entering Awaiting_sca or Pending,
     giving up before the hold was placed, or a recall being requested or refused. *)
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

(* One linked record: the legs of a transition book together or not at all. *)
let post journal entries = Ledger.Journal.append_linked journal entries
