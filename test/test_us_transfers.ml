open! Core
open Payments_core
open Helpers

let usd s = Money.Amount.of_decimal_string Money.Currency.usd s |> Or_error.ok_exn
let usd_account = account "customer:1/usd"
let ach = Rail.Ach { sec = Ppd; same_day = false }
let same_day = Rail.Ach { sec = Ppd; same_day = true }

let us_beneficiary =
  Beneficiary.create
    ~id:(Beneficiary.Id.of_string "ben-us")
    ~owner:"customer-1"
    ~name:"Ada Example"
    ~rail:ach
    ~now:t0
    "021000021 123456789 savings"
  |> Or_error.ok_exn
;;

let us_draft ?(rail = ach) ?us_rules ?(reference = "rent") amount =
  Transfer.draft
    ~id:(Transfer.Id.of_string "tr-us")
    ~rail
    ~account:usd_account
    ~beneficiary:us_beneficiary
    ~amount
    ~reference
    ?us_rules
    ~now:t0
    ()
;;

let show r =
  print_s
    [%sexp
      (Or_error.map r ~f:(fun (t : Transfer.t) -> Rail.to_string t.rail, t.details)
       : (string * Transfer.Details.t) Or_error.t)]
;;

let%expect_test "rails and destinations" =
  print_s
    [%sexp
      (Beneficiary.Destination.rails us_beneficiary.destination
       |> List.map ~f:Rail.to_string
       : string list)];
  List.iter [ "ach:ppd"; "ach:ccd:same_day"; "rtp"; "fedwire" ] ~f:(fun s ->
    let rail = Rail.of_string s in
    print_s
      [%sexp
        (s : string)
        , (Rail.is_instant rail : bool)
        , (Rail.is_returnable rail : bool)
        , (Rail.is_us rail : bool)]);
  [%expect
    {|
    (ach:ppd ach:ppd:same_day ach:ccd ach:ccd:same_day ach:web ach:web:same_day
     ach:tel ach:tel:same_day ach:ctx ach:ctx:same_day rtp fedwire)
    (ach:ppd false true true)
    (ach:ccd:same_day false true true)
    (rtp true false true)
    (fedwire false false true)
    |}]
;;

let%expect_test "drafts on the US rails" =
  show (us_draft (usd "2500.00"));
  show (us_draft ~rail:same_day (usd "1000000.00"));
  show (us_draft ~rail:same_day (usd "1000000.01"));
  show (us_draft ~rail:Rtp (usd "10000000.01"));
  let us_rules = Us_scheme.Rules.create ~rtp_max_amount:(usd "25000.00") () |> ok_exn in
  show (us_draft ~rail:Rtp ~us_rules (usd "25000.01"));
  show (us_draft ~rail:Fedwire (usd "25000000.00"));
  show (us_draft (eur "10.00"));
  show (us_draft ~reference:(String.make 81 'x') (usd "1.00"));
  show (us_draft ~rail:Sepa_credit (usd "1.00"));
  [%expect
    {|
    (Ok (ach:ppd (Us_domestic ((scheme_ref ()) (effective_date ())))))
    (Ok (ach:ppd:same_day (Us_domestic ((scheme_ref ()) (effective_date ())))))
    (Error
     ("over the Same Day ACH limit" (amount "1000000.01 USD")
      (max "1000000.00 USD")))
    (Error
     ("over the RTP limit" (amount "10000000.01 USD") (max "10000000.00 USD")))
    (Error ("over the RTP limit" (amount "25000.01 USD") (max "25000.00 USD")))
    (Ok (fedwire (Us_domestic ((scheme_ref ()) (effective_date ())))))
    (Error ("US domestic payments are in USD" (amount "10.00 EUR")))
    (Error
     ("reference too long" (scheme Ach) (max_len 80)
      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx))
    (Error
     ("rail cannot reach beneficiary" (rail Sepa_credit) (beneficiary ben-us)))
    |}]
;;

let%expect_test "an ACH credit is returned; an RTP payment is final" =
  let send rail =
    us_draft ~rail (usd "100.00")
    |> Or_error.bind ~f:(Transfer.exempt ~exemption:Trusted_beneficiary)
    |> Or_error.bind ~f:(fun t -> Transfer.submit t ~at:(at 1) ())
    |> Or_error.bind ~f:(Transfer.transition ~to_:Settled ~at:(at 2))
    |> ok_exn
  in
  let return t =
    print_s
      [%sexp
        (Transfer.transition
           t
           ~to_:(Returned (Iso_reason.of_code "AC04" |> ok_exn))
           ~at:(at 3)
         |> Or_error.map ~f:(fun (t : Transfer.t) -> t.status)
         : Transfer.Status.t Or_error.t)]
  in
  return (send ach);
  return (send Rtp);
  [%expect
    {|
    (Ok (Returned AC04))
    (Error
     ("transition not allowed" (transfer tr-us) (from Settled)
      (to_ (Returned AC04)) (rail Rtp)))
    |}]
;;

let%expect_test "US transfers need V2" =
  let t = us_draft (usd "1.00") |> ok_exn in
  let bytes = Bin_prot.Writer.to_string Transfer.Stable.V2.bin_writer_t t in
  [%test_result: Transfer.t]
    (Bin_prot.Reader.of_string Transfer.Stable.V2.bin_reader_t bytes)
    ~expect:t;
  Expect_test_helpers_core.require_does_raise (fun () ->
    Bin_prot.Writer.to_string Transfer.Stable.V1.bin_writer_t t);
  [%expect
    {|
    ("Rail.Stable.V1 cannot hold this rail; write Rail.Stable.V2"
     (rail (
       Ach
       (sec      Ppd)
       (same_day false))))
    |}]
;;

let%expect_test "addresses on the newer networks" =
  let show network raw =
    print_s
      [%sexp
        (network : Network.t)
        , (Crypto_address.create network raw |> Or_error.map ~f:Crypto_address.address
           : string Or_error.t)]
  in
  (* The EIP-55 specification's example address, on each EVM layer. *)
  List.iter [ Network.Base; Arbitrum; Plasma ] ~f:(fun n ->
    show n "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed");
  (* A synthetic Spark address: Bech32m over 33 bytes, not a real wallet. *)
  let data =
    Bech32.convert_bits (List.init 33 ~f:(fun i -> i)) ~from:8 ~to_:5 ~pad:true
    |> Option.value_exn
  in
  let spark = Bech32.encode ~hrp:"sp" ~data Bech32m in
  show Spark spark;
  show Spark (Bech32.encode ~hrp:"sp" ~data Bech32);
  show Spark (Bech32.encode ~hrp:"bc" ~data Bech32m);
  (* A Base address cannot be written in the V1 format. *)
  let base =
    Crypto_address.create_exn Base "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"
  in
  Expect_test_helpers_core.require_does_raise (fun () ->
    Bin_prot.Writer.to_string Crypto_address.Stable.V1.bin_writer_t base);
  print_s
    [%sexp
      (Bin_prot.Reader.of_string
         Crypto_address.Stable.V2.bin_reader_t
         (Bin_prot.Writer.to_string Crypto_address.Stable.V2.bin_writer_t base)
       |> Crypto_address.network
       : Network.t)];
  [%expect
    {|
    (Base (Ok 0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed))
    (Arbitrum (Ok 0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed))
    (Plasma (Ok 0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed))
    (Spark (Ok sp1qqqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jq4uevgw))
    (Spark
     (Error
      ("invalid address" (network Spark)
       sp1qqqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqqfqdv
       "expected a Bech32m address with an sp, spt, sprt or sps prefix")))
    (Spark
     (Error
      ("invalid address" (network Spark)
       bc1qqqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqh3uczx
       "expected a Bech32m address with an sp, spt, sprt or sps prefix")))
    ("Network.Stable.V1 cannot hold this network; write Network.Stable.V2"
     (network Base))
    Base
    |}]
;;
