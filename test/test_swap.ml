open! Core
open Payments_core
open Helpers

let btc_account = account "customer:1/btc"

let funded_ledger eur_amount =
  let b =
    ledger ~currencies:[ Money.Currency.eur; Money.Currency.btc ] ~customers:[ customer; btc_account ] ()
  in
  let deposit =
    Transfer.incoming
      ~id:(Transfer.Id.of_string "in-1")
      ~rail:Sepa_instant
      ~details:(sepa_details ())
      ~account:customer
      ~from:(External { name = "Payer"; detail = "test" })
      ~amount:(eur eur_amount)
      ~status:Settled
      ~now:t0
      ()
    |> Or_error.ok_exn
  in
  Booking.created chart b deposit ~event_id:"in-1" ~at:t0
  |> Or_error.bind ~f:(Booking.apply b)
  |> Or_error.ok_exn
;;

(* Quoted the usual way round: one BTC costs 60000 EUR. *)
let market =
  Money.Rate.create
    ~base:Money.Currency.btc
    ~quote:Money.Currency.eur
    ~price:(Q.of_int 60_000)
    ~at:t0
    ~source:"test"
  |> Or_error.ok_exn
;;

let fee = Money.Fee.create ~bps:50 Money.Currency.eur |> Or_error.ok_exn

let quote ?(sell = "100.00") () =
  Swap.Quote.create
    ~id:(Swap.Id.of_string "q-1")
    ~market
    ~sell:(eur sell)
    ~buy:Money.Currency.btc
    ~spread_bps:100
    ~fee
    ~now:t0
    ()
;;

let%expect_test "EUR to BTC with spread and fee" =
  let q = quote () |> Or_error.ok_exn in
  print_s
    [%message
      "quote"
        ~sell:(q.sell : Money.Amount.t)
        ~fee:(q.fee : Money.Amount.t)
        ~buy:(q.buy : Money.Amount.t)
        ~effective_price:(Money.Rate.price_to_string (Swap.Quote.effective_price q))];
  let b = funded_ledger "150.00" in
  let entries =
    Swap.execute chart b q ~from_:customer ~to_:btc_account ~event_id:"swap-evt-1" ~now:(at 10)
    |> Or_error.ok_exn
  in
  print_entries entries;
  let b = Booking.apply b entries |> Or_error.ok_exn in
  print_s
    [%message
      "after"
        ~eur:(Ledger.Balances.available b customer Money.Currency.eur : Money.Amount.t)
        ~btc:(Ledger.Balances.available b btc_account Money.Currency.btc : Money.Amount.t)];
  Ledger.Trial_balance.(check (of_balances b)) |> Or_error.ok_exn;
  [%expect {|
    (quote (sell "100.00 EUR") (fee "0.50 EUR") (buy "0.00164175 BTC")
     (effective_price 0.0000164175))
    swap-evt-1/swap-sell  (Transfer swap-sell)  ref=q-1
      debit  customer:1/eur                           99.50 EUR
      credit fx:position/EUR                          99.50 EUR
    swap-evt-1/swap-fee  (Transfer swap-fee)  ref=q-1
      debit  customer:1/eur                           0.50 EUR
      credit income:fees/EUR                          0.50 EUR
    swap-evt-1/swap-buy  (Transfer swap-buy)  ref=q-1
      debit  fx:position/BTC                          0.00164175 BTC
      credit customer:1/btc                           0.00164175 BTC
    (after (eur "50.00 EUR") (btc "0.00164175 BTC"))
    |}]
;;

let%expect_test "refusals" =
  let q = quote () |> Or_error.ok_exn in
  let show r = print_s [%sexp (r |> Result.map ~f:(fun _ -> ()) : unit Or_error.t)] in
  show (Swap.execute chart (funded_ledger "150.00") q ~from_:customer ~to_:btc_account ~event_id:"e" ~now:(at 30));
  show (Swap.execute chart (funded_ledger "50.00") q ~from_:customer ~to_:btc_account ~event_id:"e" ~now:(at 1));
  show (quote ~sell:"0.01" ());
  show
    (Swap.Quote.create
       ~id:(Swap.Id.of_string "q-2")
       ~market
       ~sell:(eur "10.00")
       ~buy:Money.Currency.eth
       ~now:t0
       ());
  [%expect {|
    (Error ("quote expired" (id q-1) (expires_at "2026-10-13 09:00:30Z")))
    (Error
     ("entry would take an account below zero" (id customer:1/eur)
      (balance_after "-49.50 EUR") (idempotency_key e/swap-sell)))
    (Error ("fee exceeds the amount" (sell "0.01 EUR") (fee "0.01 EUR")))
    (Error
     ("rate does not connect the currencies"
      (market
       ((base BTC) (quote EUR) (price 60000) (at "2026-10-13 09:00:00Z")
        (source test)))
      (sell_currency EUR) (buy_currency ETH)))
    |}]
;;

let%expect_test "the customer never gets more than the quoted price allows" =
  Quickcheck.test
    (Quickcheck.Generator.both (Int.gen_incl 1_000 10_000_000) (Int.gen_incl 0 500))
    ~sexp_of:[%sexp_of: int * int]
    ~f:(fun (cents, spread_bps) ->
      match
        Swap.Quote.create
          ~id:(Swap.Id.of_string "q")
          ~market
          ~sell:(Money.Amount.of_minor_units_int Money.Currency.eur cents)
          ~buy:Money.Currency.btc
          ~spread_bps
          ~fee
          ~now:t0
          ()
      with
      | Error _ -> ()
      | Ok q ->
        let net = Money.Amount.(to_q (sub_exn q.sell q.fee)) in
        let exact = Q.(net / of_int 60_000 * (one - (of_int spread_bps / of_int 10_000))) in
        let got = Money.Amount.to_q q.buy in
        let one_sat = Q.of_ints 1 100_000_000 in
        assert (Q.leq got exact);
        assert (Q.lt Q.(exact - got) one_sat))
;;
