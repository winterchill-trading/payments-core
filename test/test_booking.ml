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
  let entries =
    Booking.created chart (ledger ()) deposit ~event_id:"evt-in-1" ~at:t0
    |> Or_error.ok_exn
  in
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
  [%expect
    {|
    evt-in-1/deposit  DEPOSIT  (Transfer deposit)  ref=in-1
      debit  settlement:sepa_credit/EUR               100.00 EUR
      credit customer:1/eur                           100.00 EUR
    -- Submitted
    evt-1/hold  WITHDRAWAL  (Hold_place transfer:tr-1)  ref=tr-1
      debit  customer:1/eur                           25.50 EUR
      credit customer:1/eur/pending                   25.50 EUR
    -- Pending
    -- Settled
    evt-3/capture  WITHDRAWAL  (Hold_capture transfer:tr-1)  ref=tr-1
      debit  customer:1/eur/pending                   25.50 EUR
      credit settlement:sepa_credit/EUR               25.00 EUR
      credit income:fees/EUR                          0.50 EUR
    -- Returned
    evt-4/return  RETURN  (Transfer return)  ref=tr-1  sca=(Exemption"Art 16 Low_value_remote")
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
  [%expect
    {|
    evt-in-1/deposit  DEPOSIT  (Transfer deposit)  ref=in-1
      debit  settlement:sepa_credit/EUR               100.00 EUR
      credit customer:1/eur                           100.00 EUR
    -- Submitted
    evt-1/hold  WITHDRAWAL  (Hold_place transfer:tr-1)  ref=tr-1
      debit  customer:1/eur                           25.50 EUR
      credit customer:1/eur/pending                   25.50 EUR
    -- Rejected
    evt-2/release  HOLD  (Hold_release transfer:tr-1)  ref=tr-1
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
  [%expect
    {|
    evt-in-1/deposit  DEPOSIT  (Transfer deposit)  ref=in-1
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
  print_entries
    (Booking.created chart (ledger ()) t ~event_id:"evt-0" ~at:t0 |> Or_error.ok_exn);
  (ledger (), t)
  |> step ~event_id:"evt-1" ~sec:1 Settled
  |> step ~event_id:"evt-2" ~sec:2 (Returned AC04)
  |> finish;
  [%expect
    {|
    -- Settled
    evt-1/deposit  DEPOSIT  (Transfer deposit)  ref=in-2
      debit  settlement:sepa_credit/EUR               40.00 EUR
      credit customer:1/eur                           40.00 EUR
    evt-1/fee  FEE  (Transfer fee)  ref=in-2
      debit  customer:1/eur                           0.20 EUR
      credit income:fees/EUR                          0.20 EUR
    -- Returned
    evt-2/fee-refund  REFUND  (Transfer fee-refund)  ref=in-2
      debit  income:fees/EUR                          0.20 EUR
      credit customer:1/eur                           0.20 EUR
    evt-2/return  RETURN  (Transfer return)  ref=in-2
      debit  customer:1/eur                           40.00 EUR
      credit settlement:sepa_credit/EUR               40.00 EUR
    customer:1/eur                           available=0.00 held=0.00
    settlement:sepa_credit/EUR               available=0.00 held=0.00
    income:fees/EUR                          available=0.00 held=0.00
    |}]
;;

let%expect_test "a transition posts to a journal as one linked record, once" =
  let dir = Core_unix.mkdtemp (Filename.concat Filename.temp_dir_name "payments-core") in
  let path = Filename.concat dir "journal" in
  let clock = Async_kernel.Time_source.create ~now:t0 () in
  let time_source = Async_kernel.Time_source.read_only clock in
  let j = Ledger.Journal.create ~time_source ~fsync:false path |> Or_error.ok_exn in
  (* The journal stamps a linked group now, now+1ns, ...; with a frozen clock the next
     group would start before the last one, so move the clock on between posts. *)
  let tick = ref 0 in
  let advance () =
    incr tick;
    Async_kernel.Time_source.advance_directly clock ~to_:(at !tick)
  in
  List.iter
    (Ledger.Account.create customer Liability
     :: Booking.Chart.accounts
          chart
          ~rails:[ Sepa_credit ]
          ~currencies:[ Money.Currency.eur ])
    ~f:(fun a ->
      ignore
        (Ledger.Journal.open_account j a |> Or_error.ok_exn
         : Ledger.Journal.Append_result.t));
  let book t ~to_ ~event_id =
    advance ();
    let entries =
      Booking.transition chart (Ledger.Journal.balances j) t ~to_ ~event_id ~at:(at !tick)
      |> Or_error.ok_exn
    in
    let r1 = Booking.post j entries |> Or_error.ok_exn in
    let r2 = Booking.post j entries |> Or_error.ok_exn in
    let dup = function
      | Ledger.Journal.Append_result.Duplicate _ -> "duplicate"
      | Appended _ -> "appended"
    in
    printf
      "%s: %d entries, first %s, replay %s\n"
      event_id
      (List.length entries)
      (dup r1)
      (dup r2)
  in
  let deposit =
    Transfer.incoming
      ~id:(Transfer.Id.of_string "in-1")
      ~rail:Sepa_credit
      ~account:customer
      ~details:(sepa_details ())
      ~from:(External { name = "Payer"; detail = "x" })
      ~amount:(eur "100.00")
      ~status:Pending
      ~now:t0
      ()
    |> Or_error.ok_exn
  in
  book deposit ~to_:Settled ~event_id:"evt-in";
  let t = draft () in
  book t ~to_:Submitted ~event_id:"evt-1";
  let t = Transfer.submit t ~at:t0 () |> Or_error.ok_exn in
  book t ~to_:Settled ~event_id:"evt-2";
  printf
    "available %s\n"
    (Money.Amount.to_string
       (Ledger.Balances.available (Ledger.Journal.balances j) customer Money.Currency.eur));
  Ledger.Journal.close j;
  [%expect
    {|
    evt-in: 1 entries, first appended, replay duplicate
    evt-1: 1 entries, first appended, replay duplicate
    evt-2: 1 entries, first appended, replay duplicate
    available 74.50 EUR
    |}]
;;
