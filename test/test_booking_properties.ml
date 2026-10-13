open! Core
open Payments_core
open Helpers

(* Random walks through the transfer state machine, booking every step. *)

let candidates : Transfer.Status.t list =
  [ Awaiting_sca
  ; Submitted
  ; Pending
  ; Settled
  ; Rejected AC01
  ; Failed Insufficient_funds
  ; Returned AC04
  ; Returned FOCR
  ; Recall_requested DUPL
  ; Timed_out
  ; Reorged
  ; Recall_refused NOAS
  ; Recalled
  ; Cancelled
  ]
;;

type case =
  { direction : Transfer.Direction.t
  ; rail : Rail.t
  ; amount_cents : int
  ; fee_cents : int
  ; funding_cents : int
  ; start_settled : bool
  ; choices : int list
  }
[@@deriving sexp_of]

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%bind direction = Quickcheck.Generator.of_list [ Transfer.Direction.In; Out ] in
  let%bind rail =
    Quickcheck.Generator.of_list
      [ Rail.Sepa_credit; Sepa_instant; Intraledger; Onchain Bitcoin ]
  in
  let%bind amount_cents = Int.gen_incl 1 10_000 in
  let%bind fee_cents =
    Quickcheck.Generator.weighted_union [ 1., return 0; 2., Int.gen_incl 1 (min 200 amount_cents) ]
  in
  let%bind funding_cents = Int.gen_incl 0 12_000 in
  let%bind start_settled = Bool.quickcheck_generator in
  let%map choices = List.gen_with_length 8 (Int.gen_incl 0 100) in
  { direction; rail; amount_cents; fee_cents; funding_cents; start_settled; choices }
;;

let cents n = Money.Amount.of_minor_units_int Money.Currency.eur n

let travel_rule : Travel_rule.t =
  { originator =
      { name = "Customer One"
      ; account = "customer:1"
      ; identification = Some (Customer_number "c-1")
      ; lei = None
      }
  ; beneficiary =
      { name = "Ada Example"; account = "bc1q-example"; identification = None; lei = None }
  ; counterparty_wallet = Hosted { provider_name = "Example Exchange"; provider_lei = None }
  }
;;

let details_for (rail : Rail.t) : Transfer.Details.t option =
  match rail with
  | Sepa_credit | Sepa_instant -> Some (sepa_details ())
  | Onchain _ ->
    Some
      (Onchain
         { travel_rule = Some travel_rule
         ; tx_hash = None
         ; block_hash = None
         ; confirmations = 0
         ; finalized = false
         })
  | _ -> None
;;

let fund b ~rail cents_ =
  if cents_ = 0
  then b
  else (
    let t =
      Transfer.incoming
        ?details:(details_for rail)
        ~id:(Transfer.Id.of_string "funding")
        ~rail
        ~account:customer
        ~from:(External { name = "Funding"; detail = "test" })
        ~amount:(cents cents_)
        ~status:Settled
        ~now:t0
        ()
      |> Or_error.ok_exn
    in
    Booking.created chart b t ~event_id:"funding" ~at:t0
    |> Or_error.bind ~f:(Booking.apply b)
    |> Or_error.ok_exn)
;;

let start case =
  let amount = cents case.amount_cents in
  let fee = cents case.fee_cents in
  match case.direction with
  | Out ->
    let raw =
      match case.rail with
      | Intraledger -> "customer:2/eur"
      | Onchain _ -> "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
      | _ -> "DE89370400440532013000"
    in
    Transfer.draft
      ?details:(details_for case.rail)
      ~id:(Transfer.Id.of_string "tr")
      ~rail:case.rail
      ~account:customer
      ~beneficiary:(beneficiary ~rail:case.rail ~raw ())
      ~amount
      ~fee
      ~now:t0
      ()
    |> Or_error.ok_exn
  | In ->
    Transfer.incoming
      ?details:(details_for case.rail)
      ~id:(Transfer.Id.of_string "tr")
      ~rail:case.rail
      ~account:customer
      ~from:(External { name = "Payer"; detail = "test" })
      ~amount
      ~fee
      ~status:(if case.start_settled then Settled else Pending)
      ~now:t0
      ()
    |> Or_error.ok_exn
;;

let held b (t : Transfer.t) =
  match Ledger.Balances.find_hold b (Booking.hold_id t) with
  | None -> Money.Amount.zero Money.Currency.eur
  | Some h -> h.remaining
;;

let check_invariants b (t : Transfer.t) =
  Ledger.Trial_balance.of_balances b |> Ledger.Trial_balance.check |> Or_error.ok_exn;
  match t.direction, t.status with
  | Out, (Submitted | Pending | Reorged) ->
    [%test_result: Money.Amount.t] (held b t) ~expect:(Transfer.total_debit t)
  | _ -> [%test_result: Money.Amount.t] (held b t) ~expect:(cents 0)
;;

let run case =
  let b = fund (ledger ()) ~rail:case.rail case.funding_cents in
  let t = start case in
  let b =
    Booking.created chart b t ~event_id:"created" ~at:t0
    |> Or_error.bind ~f:(Booking.apply b)
    |> Or_error.ok_exn
  in
  check_invariants b t;
  List.foldi case.choices ~init:(b, t) ~f:(fun i (b, t) choice ->
    (* Candidates the state machine accepts here, reason codes included. *)
    let next =
      List.filter candidates ~f:(fun to_ -> Result.is_ok (Transfer.transition t ~to_ ~at:t0))
    in
    if List.is_empty next
    then b, t
    else (
      let to_ = List.nth_exn next (choice % List.length next) in
      let at = at (i + 1) in
      let event_id = sprintf "evt-%d" i in
      let booked =
        match Booking.transition chart b t ~to_ ~event_id ~at, to_ with
        | Ok entries, _ -> Some (to_, entries)
        (* A hold over the available balance is refused; the transfer fails instead. *)
        | Error _, Submitted ->
          let to_ = Transfer.Status.Failed Insufficient_funds in
          Some (to_, Booking.transition chart b t ~to_ ~event_id ~at |> Or_error.ok_exn)
        (* Returning an incoming payment the customer already spent is refused too. *)
        | Error _, (Returned _ | Recalled) -> None
        | Error e, _ -> Error.raise e
      in
      match booked with
      | None -> b, t
      | Some (to_, entries) ->
        let b = Booking.apply b entries |> Or_error.ok_exn in
        let t = Transfer.transition t ~to_ ~at |> Or_error.ok_exn in
        check_invariants b t;
        b, t))
;;

let%expect_test "random lifecycles keep the books balanced and close every hold" =
  let terminal = ref 0 in
  Quickcheck.test gen ~trials:2_000 ~sexp_of:sexp_of_case ~f:(fun case ->
    let b, t = run case in
    if Transfer.is_terminal t then incr terminal;
    check_invariants b t);
  (* Most walks end in a terminal state within eight steps. *)
  print_s [%message (!terminal > 1_000 : bool)];
  [%expect {| ("(!terminal) > 1_000" true) |}]
;;
