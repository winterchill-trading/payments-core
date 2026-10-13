open! Core
open Payments_core
open Helpers

let settlement = chart.settlement Sepa_credit Money.Currency.eur

let ledger_with_pending_payment () =
  let b = ledger () in
  let deposit =
    Transfer.incoming
      ~id:(Transfer.Id.of_string "in-1")
      ~rail:Sepa_credit
      ~details:(sepa_details ())
      ~account:customer
      ~from:(External { name = "Payer"; detail = "test" })
      ~amount:(eur "100.00")
      ~status:Settled
      ~now:t0
      ()
    |> Or_error.ok_exn
  in
  let book b entries = Booking.apply b (Or_error.ok_exn entries) |> Or_error.ok_exn in
  let b = book b (Booking.created chart b deposit ~event_id:"e0" ~at:t0) in
  let t = draft () in
  let b = book b (Booking.transition chart b t ~to_:Submitted ~event_id:"e1" ~at:(at 1)) in
  b
;;

let%expect_test "balances" =
  let b = ledger_with_pending_payment () in
  let reported : Reconciliation.Reported_balance.t list =
    [ { account = customer; amount = eur "100.00"; basis = Posted }
    ; { account = customer; amount = eur "75.00"; basis = Available }
    ; { account = account "customer:9/eur"; amount = eur "1.00"; basis = Posted }
    ]
  in
  print_s
    [%sexp
      (Reconciliation.balances
         ~scope:[ customer, Money.Currency.eur; settlement, Money.Currency.eur ]
         b
         reported
       : Reconciliation.Diff.t list)];
  [%expect {|
    ((Balance_mismatch (account customer:1/eur) (basis Available)
      (ledger "74.50 EUR") (provider "75.00 EUR") (delta "0.50 EUR"))
     (Unknown_account (account customer:9/eur))
     (Unreported_balance (account settlement:sepa_credit/EUR)
      (ledger "100.00 EUR")))
    |}]
;;

let%expect_test "transfers" =
  let submitted ref_ id =
    { (Transfer.submit
         (draft ())
         ~provider_ref:ref_
         ~at:(at 1)
         ()
       |> Or_error.ok_exn)
      with
      id = Transfer.Id.of_string id
    }
  in
  let settled = Transfer.transition (submitted "p-2" "tr-2") ~to_:Settled ~at:(at 2) |> Or_error.ok_exn in
  let ours = [ submitted "p-1" "tr-1"; settled; submitted "p-3" "tr-3"; draft () ] in
  let theirs : Reconciliation.Reported_transfer.t list =
    [ { provider_ref = "p-1"; status = Settled }
    ; { provider_ref = "p-2"; status = Settled }
    ; { provider_ref = "p-9"; status = Pending }
    ]
  in
  print_s [%sexp (Reconciliation.transfers ours theirs : Reconciliation.Diff.t list)];
  [%expect {|
    ((Status_mismatch (transfer tr-1) (provider_ref p-1) (ledger Submitted)
      (provider Settled))
     (Unknown_transfer (provider_ref p-9) (provider Pending))
     (Missing_at_provider (transfer tr-3) (provider_ref p-3)))
    |}]
;;

let%expect_test "breaks, carry-forward, ageing and maker-checker resolution" =
  let d = Date.of_string in
  let ours : Reconciliation.Internal_tx.t list =
    [ { provider_ref = "p-1"; amount = eur "10.00"; booked_on = d "2026-10-12" }
    ; { provider_ref = "p-2"; amount = eur "20.00"; booked_on = d "2026-10-12" }
    ; { provider_ref = "p-3"; amount = eur "30.00"; booked_on = d "2026-10-13" }
    ; { provider_ref = "p-4"; amount = eur "40.00"; booked_on = d "2026-10-07" }
    ; { provider_ref = "p-5"; amount = eur "5.00"; booked_on = d "2026-10-12" }
    ; { provider_ref = "p-5"; amount = eur "5.00"; booked_on = d "2026-10-12" }
    ]
  in
  let theirs : Reconciliation.External_tx.t list =
    [ { provider_ref = "p-1"; amount = eur "10.00"; value_date = d "2026-10-12" }
    ; { provider_ref = "p-2"; amount = eur "19.50"; value_date = d "2026-10-12" }
    ; { provider_ref = "p-5"; amount = eur "5.00"; value_date = d "2026-10-12" }
    ; { provider_ref = "p-9"; amount = eur "9.00"; value_date = d "2026-10-05" }
    ]
  in
  let day1 = Reconciliation.match_transactions ~business_date:(d "2026-10-13") ours theirs in
  List.iter day1 ~f:(fun b -> print_s [%sexp (b.id : string), (b.first_seen : Date.t)]);
  (* next day: p-3 still missing at the provider, now past the timing window *)
  let day2 =
    Reconciliation.match_transactions ~business_date:(d "2026-10-15") ours theirs
    |> fun current -> Reconciliation.carry_forward ~previous:day1 ~current
  in
  print_endline "-- day 2";
  List.iter day2 ~f:(fun b -> print_s [%sexp (b.id : string), (b.first_seen : Date.t)]);
  print_s
    [%sexp
      (Reconciliation.ageing day2 ~today:(d "2026-10-23") : (Reconciliation.Break.Age.t * int) list)];
  let mismatch = List.find_exn day2 ~f:(fun b -> String.equal b.id "Amount_mismatch:p-2") in
  let adjustment =
    Ledger.Entry.create
      ~idempotency_key:(Ledger.Idempotency_key.of_string "adj-1")
      ~kind:(Transfer "adjustment")
      ~reference:"Amount_mismatch:p-2"
      ~effective_at:t0
      ~recorded_at:t0
      [ Ledger.Posting.debit (account "expense:rounding/eur") (eur "0.50")
      ; Ledger.Posting.credit (account "settlement:sepa_credit/EUR") (eur "0.50")
      ]
    |> Or_error.ok_exn
  in
  let show r = print_s [%sexp (Result.map r ~f:(fun (b : Reconciliation.Break.t) -> b.status) : Reconciliation.Break.Status.t Or_error.t)] in
  show (Reconciliation.Break.resolve mismatch ~adjustment ~maker:"op-1" ~checker:"op-1");
  show (Reconciliation.Break.resolve mismatch ~adjustment ~maker:"op-1" ~checker:"op-2");
  [%expect {|
    (Amount_mismatch:p-2 2026-10-13)
    (Timing:p-3 2026-10-13)
    (Missing_external:p-4 2026-10-13)
    (Duplicate:p-5 2026-10-13)
    (Missing_internal:p-9 2026-10-13)
    -- day 2
    (Amount_mismatch:p-2 2026-10-13)
    (Missing_external:p-3 2026-10-13)
    (Missing_external:p-4 2026-10-13)
    (Duplicate:p-5 2026-10-13)
    (Missing_internal:p-9 2026-10-13)
    ((Days_0_1 0) (Days_2_5 0) (Over_5 5))
    (Error ("maker and checker must differ" op-1))
    (Ok (Resolved (adjustment adj-1) (maker op-1) (checker op-2)))
    |}]
;;
