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

let customer = account "customer:1/eur"
let chart = Booking.Chart.default

let open_accounts accounts =
  List.fold accounts ~init:Ledger.Balances.empty ~f:(fun b a ->
    Ledger.Balances.apply b { seq = Ledger.Balances.next_seq b; event = Account_opened a }
    |> Or_error.ok_exn)
;;

let ledger ?(currencies = [ Money.Currency.eur ]) ?(customers = [ customer ]) () =
  open_accounts
    (List.map customers ~f:(fun c -> Ledger.Account.create c Liability)
     @ Booking.Chart.accounts chart ~rails:Rail.all ~currencies)
;;

let print_entries entries =
  List.iter entries ~f:(fun (e : Ledger.Entry.t) ->
    printf
      "%s  %s  ref=%s\n"
      (Ledger.Idempotency_key.to_string e.idempotency_key)
      (Sexp.to_string [%sexp (e.kind : Ledger.Entry.Kind.t)])
      e.reference;
    List.iter e.postings ~f:(fun p ->
      printf
        "  %-6s %-40s %s\n"
        (match p.side with
         | Debit -> "debit"
         | Credit -> "credit")
        (Ledger.Account_id.to_string p.account)
        (Money.Amount.to_string p.amount)))
;;

let print_balances b accounts =
  List.iter accounts ~f:(fun a ->
    printf
      "%-40s available=%s held=%s\n"
      (Ledger.Account_id.to_string a)
      (Money.Amount.to_decimal_string (Ledger.Balances.available b a Money.Currency.eur))
      (Money.Amount.to_decimal_string (Ledger.Balances.held b a Money.Currency.eur)))
;;
