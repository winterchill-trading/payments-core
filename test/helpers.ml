open! Core
open Payments_core

let t0 = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"
let at sec = Time_ns.add t0 (Time_ns.Span.of_int_sec sec)
let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn
let account = Ledger.Account_id.of_string

let beneficiary ?(rail = Rail.Sepa_credit) ?(raw = "DE89370400440532013000") () =
  Beneficiary.create
    ~id:(Beneficiary.Id.of_string "ben-1")
    ~owner:"customer-1"
    ~name:"Ada Example"
    ~rail
    ~now:t0
    raw
  |> Or_error.ok_exn
;;

let draft ?(rail = Rail.Sepa_credit) ?(amount = "25.00") ?(fee = "0.50") () =
  Transfer.draft
    ~id:(Transfer.Id.of_string "tr-1")
    ~rail
    ~account:(account "customer:1/eur")
    ~beneficiary:(beneficiary ~rail ())
    ~amount:(eur amount)
    ~fee:(eur fee)
    ~reference:"invoice 42"
    ~now:t0
    ()
  |> Or_error.ok_exn
;;
