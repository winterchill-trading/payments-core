open! Core
open Payments_core
open Helpers

let btc s = Money.Amount.of_string (s ^ " BTC")

let deposit () =
  Transfer.incoming
    ~id:(Transfer.Id.of_string "dep-1")
    ~rail:(Onchain Bitcoin)
    ~account:(account "customer:1/btc")
    ~from:(External { name = "unknown"; detail = "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4" })
    ~amount:(btc "0.01000000")
    ~fee:(btc "0.00001000")
    ~status:Pending
    ~now:t0
    ()
  |> Or_error.ok_exn
;;

let%expect_test "confirmation policy" =
  List.iter
    [ Network.Bitcoin, Some (eur "500.00"); Bitcoin, Some (eur "20000.00"); Bitcoin, None; Ethereum, None; Tron, None ]
    ~f:(fun (n, eur_value) ->
      print_s
        [%message
          (n : Network.t)
            (eur_value : Money.Amount.t option)
            ~_:(Confirmation_policy.required n ~eur_value : Confirmation_policy.t)]);
  [%expect {|
    ((n Bitcoin) (eur_value ("500.00 EUR")) (Confirmations 3))
    ((n Bitcoin) (eur_value ("20000.00 EUR")) (Confirmations 6))
    ((n Bitcoin) (eur_value ()) (Confirmations 6))
    ((n Ethereum) (eur_value ()) Finalized)
    ((n Tron) (eur_value ()) (Confirmations 19))
    |}]
;;

let%expect_test "a deposit settles at 3 confirmations, is taken back on a reorg and credited again" =
  let b =
    ledger ~currencies:[ Money.Currency.btc ] ~customers:[ account "customer:1/btc" ] ()
  in
  let step (b, t) f =
    let t' = f t |> Or_error.ok_exn in
    let entries =
      if Transfer.Status.equal t.Transfer.status t'.Transfer.status
      then []
      else
        Booking.transition chart b t ~to_:t'.status ~event_id:(sprintf "ev-%d" (List.length t'.history)) ~at:t0
        |> Or_error.ok_exn
    in
    print_s [%message "" ~status:(t'.status : Transfer.Status.t)];
    print_entries entries;
    Booking.apply b entries |> Or_error.ok_exn, t'
  in
  let confirm n ~block t =
    Transfer.record_confirmations t ~tx_hash:"tx-1" ~block_hash:block ~confirmations:n ~finalized:false
      ~eur_value:(eur "600.00") ~at:t0 ()
  in
  let _ =
    (b, deposit ())
    |> fun s -> step s (confirm 1 ~block:"blk-a")
    |> fun s -> step s (confirm 3 ~block:"blk-a")
    |> fun s -> step s (fun t -> Transfer.transition t ~to_:Reorged ~at:t0)
    |> fun s -> step s (fun t -> Transfer.transition t ~to_:Pending ~at:t0)
    |> fun s -> step s (confirm 3 ~block:"blk-b")
  in
  [%expect {|
    (status Pending)
    (status Settled)
    ev-2/deposit  (Transfer deposit)  ref=dep-1
      debit  settlement:onchain:bitcoin/BTC           0.01000000 BTC
      credit customer:1/btc                           0.01000000 BTC
    ev-2/fee  (Transfer fee)  ref=dep-1
      debit  customer:1/btc                           0.00001000 BTC
      credit income:fees/BTC                          0.00001000 BTC
    (status Reorged)
    ev-3/reorg-fee-refund  (Transfer reorg-fee-refund)  ref=dep-1
      debit  income:fees/BTC                          0.00001000 BTC
      credit customer:1/btc                           0.00001000 BTC
    ev-3/reorg  (Transfer reorg)  ref=dep-1
      debit  customer:1/btc                           0.01000000 BTC
      credit settlement:onchain:bitcoin/BTC           0.01000000 BTC
    (status Pending)
    (status Settled)
    ev-5/deposit  (Transfer deposit)  ref=dep-1
      debit  settlement:onchain:bitcoin/BTC           0.01000000 BTC
      credit customer:1/btc                           0.01000000 BTC
    ev-5/fee  (Transfer fee)  ref=dep-1
      debit  customer:1/btc                           0.00001000 BTC
      credit income:fees/BTC                          0.00001000 BTC
    |}]
;;

let%expect_test "Lightning settles only on the matching preimage" =
  (* preimage 32 zero bytes; its SHA-256 is the invoice's payment hash *)
  let preimage = String.make 64 '0' in
  let hash = "66687aadf862bd776c8fc18b8e9f8e20089714856ee233b3902a591d0d5f2925" in
  let b = beneficiary ~rail:Lightning ~raw:"satoshi@example.com" () in
  let t =
    Transfer.draft
      ~id:(Transfer.Id.of_string "ln-1")
      ~rail:Lightning
      ~account:(account "customer:1/btc")
      ~beneficiary:b
      ~amount:(btc "0.00010000")
      ~details:(Lightning { payment_hash = Some hash; preimage = None })
      ~now:t0
      ()
    |> Or_error.ok_exn
  in
  let t = Transfer.submit t ~at:(at 1) () |> Or_error.ok_exn in
  print_s [%sexp (Transfer.settle_lightning t ~preimage:(String.make 64 '1') ~at:(at 2) |> Result.map ~f:(fun t -> t.details) : Transfer.Details.t Or_error.t)];
  print_s [%sexp (Transfer.settle_lightning t ~preimage ~at:(at 2) |> Result.map ~f:(fun t -> t.status, t.details) : (Transfer.Status.t * Transfer.Details.t) Or_error.t)];
  [%expect {|
    (Error
     ("preimage does not match the payment hash"
      66687aadf862bd776c8fc18b8e9f8e20089714856ee233b3902a591d0d5f2925))
    (Ok
     (Settled
      (Lightning
       (payment_hash
        (66687aadf862bd776c8fc18b8e9f8e20089714856ee233b3902a591d0d5f2925))
       (preimage
        (0000000000000000000000000000000000000000000000000000000000000000)))))
    |}]
;;
