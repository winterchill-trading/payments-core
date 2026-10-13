open! Core
open Payments_core
open Helpers

let settlement = chart.settlement Sepa_credit Money.Currency.eur
let fees = chart.fee_income Money.Currency.eur

(* Funds the customer with an incoming SEPA credit. *)
let funded amount =
  let deposit =
    Transfer.incoming
      ~id:(Transfer.Id.of_string "in-1")
      ~rail:Sepa_credit
      ~details:(sepa_details ())
      ~account:customer
      ~from:(External { name = "Employer"; detail = "DE89370400440532013000" })
      ~amount:(eur amount)
      ~status:Settled
      ~now:t0
      ()
    |> Or_error.ok_exn
  in
  let entries = Booking.created chart (ledger ()) deposit ~event_id:"evt-in-1" ~at:t0 |> Or_error.ok_exn in
  print_entries entries;
  Booking.apply (ledger ()) entries |> Or_error.ok_exn
;;

(* Applies one transition: books its entries, prints them and the balances. *)
let step to_ ~event_id ~sec (b, t) =
  let at = at sec in
  match Booking.transition chart b t ~to_ ~event_id ~at with
  | Error e ->
    print_s [%message "refused" ~_:(e : Error.t)];
    b, t
  | Ok entries ->
    printf "-- %s\n" (Transfer.Status.name to_);
    print_entries entries;
    let b = Booking.apply b entries |> Or_error.ok_exn in
    let t = Transfer.transition t ~to_ ~at |> Or_error.ok_exn in
    b, t
;;

let finish (b, _) =
  print_balances b [ customer; settlement; fees ];
  Ledger.Trial_balance.of_balances b |> Ledger.Trial_balance.check |> Or_error.ok_exn
;;

let%expect_test "outgoing SEPA: hold, settle with a fee, then returned" =
  let b = funded "100.00" in
  (b, draft ())
  |> step ~event_id:"evt-1" ~sec:1 Submitted
  |> step ~event_id:"evt-2" ~sec:2 Pending
  |> step ~event_id:"evt-3" ~sec:3 Settled
  |> step ~event_id:"evt-4" ~sec:4 (Returned AC04)
  |> finish;
  [%expect {|
    evt-in-1/deposit  (Transfer deposit)  ref=in-1
      debit  settlement:sepa_credit/EUR               100.00 EUR
      credit customer:1/eur                           100.00 EUR
    -- Submitted
    evt-1/hold  (Hold_place transfer:tr-1)  ref=tr-1
      debit  customer:1/eur                           25.50 EUR
      credit customer:1/eur/pending                   25.50 EUR
    -- Pending
    -- Settled
    evt-3/capture  (Hold_capture transfer:tr-1)  ref=tr-1
      debit  customer:1/eur/pending                   25.00 EUR
      credit settlement:sepa_credit/EUR               25.00 EUR
    evt-3/fee  (Hold_capture transfer:tr-1)  ref=tr-1
      debit  customer:1/eur/pending                   0.50 EUR
      credit income:fees/EUR                          0.50 EUR
    -- Returned
    evt-4/return  (Transfer return)  ref=tr-1
      debit  settlement:sepa_credit/EUR               25.00 EUR
      credit customer:1/eur                           25.00 EUR
    customer:1/eur                           available=99.50 held=0.00
    settlement:sepa_credit/EUR               available=100.00 held=0.00
    income:fees/EUR                          available=0.50 held=0.00
    |}]
;;

let%expect_test "outgoing SEPA: failed after submission releases the hold" =
  let b = funded "100.00" in
  (b, draft ())
  |> step ~event_id:"evt-1" ~sec:1 Submitted
  |> step ~event_id:"evt-2" ~sec:2 (Rejected AC01)
  |> finish;
  [%expect {|
    evt-in-1/deposit  (Transfer deposit)  ref=in-1
      debit  settlement:sepa_credit/EUR               100.00 EUR
      credit customer:1/eur                           100.00 EUR
    -- Submitted
    evt-1/hold  (Hold_place transfer:tr-1)  ref=tr-1
      debit  customer:1/eur                           25.50 EUR
      credit customer:1/eur/pending                   25.50 EUR
    -- Rejected
    evt-2/release  (Hold_release transfer:tr-1)  ref=tr-1
      debit  customer:1/eur/pending                   25.50 EUR
      credit customer:1/eur                           25.50 EUR
    customer:1/eur                           available=100.00 held=0.00
    settlement:sepa_credit/EUR               available=100.00 held=0.00
    income:fees/EUR                          available=0.00 held=0.00
    |}]
;;

let%expect_test "outgoing over the available balance is refused at submission" =
  let b = funded "20.00" in
  (b, draft ())
  |> step ~event_id:"evt-1" ~sec:1 Submitted
  |> step ~event_id:"evt-2" ~sec:2 (Failed Insufficient_funds)
  |> finish;
  [%expect {|
    evt-in-1/deposit  (Transfer deposit)  ref=in-1
      debit  settlement:sepa_credit/EUR               20.00 EUR
      credit customer:1/eur                           20.00 EUR
    (refused
     ("entry would take an account below zero" (id customer:1/eur)
      (balance_after "-5.50 EUR") (idempotency_key evt-1/hold)))
    -- Failed
    customer:1/eur                           available=20.00 held=0.00
    settlement:sepa_credit/EUR               available=20.00 held=0.00
    income:fees/EUR                          available=0.00 held=0.00
    |}]
;;

let%expect_test "incoming pending then settled with a fee, then returned" =
  let t =
    Transfer.incoming
      ~id:(Transfer.Id.of_string "in-2")
      ~rail:Sepa_credit
      ~details:(sepa_details ())
      ~account:customer
      ~from:(External { name = "Payer"; detail = "DE89370400440532013000" })
      ~amount:(eur "40.00")
      ~fee:(eur "0.20")
      ~status:Pending
      ~now:t0
      ()
    |> Or_error.ok_exn
  in
  print_entries (Booking.created chart (ledger ()) t ~event_id:"evt-0" ~at:t0 |> Or_error.ok_exn);
  (ledger (), t)
  |> step ~event_id:"evt-1" ~sec:1 Settled
  |> step ~event_id:"evt-2" ~sec:2 (Returned AC04)
  |> finish;
  [%expect {|
    -- Settled
    evt-1/deposit  (Transfer deposit)  ref=in-2
      debit  settlement:sepa_credit/EUR               40.00 EUR
      credit customer:1/eur                           40.00 EUR
    evt-1/fee  (Transfer fee)  ref=in-2
      debit  customer:1/eur                           0.20 EUR
      credit income:fees/EUR                          0.20 EUR
    -- Returned
    evt-2/fee-refund  (Transfer fee-refund)  ref=in-2
      debit  income:fees/EUR                          0.20 EUR
      credit customer:1/eur                           0.20 EUR
    evt-2/return  (Transfer return)  ref=in-2
      debit  customer:1/eur                           40.00 EUR
      credit settlement:sepa_credit/EUR               40.00 EUR
    customer:1/eur                           available=0.00 held=0.00
    settlement:sepa_credit/EUR               available=0.00 held=0.00
    income:fees/EUR                          available=0.00 held=0.00
    |}]
;;
