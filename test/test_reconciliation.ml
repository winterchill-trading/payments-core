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
