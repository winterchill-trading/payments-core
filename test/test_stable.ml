open! Core
open Payments_core

(* A changed digest means old bytes no longer decode: add a Stable.V2 instead of editing
   V1, and keep V1 readable. *)
let%expect_test "stable bin_prot digests" =
  List.iter
    [ "Network", [%bin_digest: Network.Stable.V1.t]
    ; "Network V2", [%bin_digest: Network.Stable.V2.t]
    ; "Rail", [%bin_digest: Rail.Stable.V1.t]
    ; "Rail V2", [%bin_digest: Rail.Stable.V2.t]
    ; "Iban", [%bin_digest: Iban.Stable.V1.t]
    ; "Crypto_address", [%bin_digest: Crypto_address.Stable.V1.t]
    ; "Crypto_address V2", [%bin_digest: Crypto_address.Stable.V2.t]
    ; "Aba", [%bin_digest: Aba.Stable.V1.t]
    ; "Us_account", [%bin_digest: Us_account.Stable.V1.t]
    ; "Lightning.Target", [%bin_digest: Lightning.Target.Stable.V1.t]
    ; "Beneficiary", [%bin_digest: Beneficiary.Stable.V1.t]
    ; "Beneficiary V2", [%bin_digest: Beneficiary.Stable.V2.t]
    ; "Uk_account", [%bin_digest: Uk_account.Stable.V1.t]
    ; "Sca.Challenge", [%bin_digest: Sca.Challenge.Stable.V1.t]
    ; "Transfer", [%bin_digest: Transfer.Stable.V1.t]
    ; "Transfer V2", [%bin_digest: Transfer.Stable.V2.t]
    ; "Standing_order", [%bin_digest: Standing_order.Stable.V1.t]
    ; "Standing_order V2", [%bin_digest: Standing_order.Stable.V2.t]
    ; "Destination V2", [%bin_digest: Beneficiary.Destination.Stable.V2.t]
    ; "Destination V3", [%bin_digest: Beneficiary.Destination.Stable.V3.t]
    ; "Beneficiary V3", [%bin_digest: Beneficiary.Stable.V3.t]
    ; "Transfer V3", [%bin_digest: Transfer.Stable.V3.t]
    ; "Standing_order V3", [%bin_digest: Standing_order.Stable.V3.t]
    ]
    ~f:(fun (name, digest) -> printf "%-18s %s\n" name digest);
  [%expect
    {|
    Network            180a350eb7c08de68cbf9d0f401227c7
    Network V2         230269990e834e3234f64c0c552fbc80
    Rail               8dfb73bd6a838ed1c3162c265c00348e
    Rail V2            fee5a27b75a6b2b5ba1d49211ee91bdb
    Iban               d9a8da25d5656b016fb4dbdc2e4197fb
    Crypto_address     43b704f271695d2835e803b2791b70ee
    Crypto_address V2  3fbc05fb09020fb57e3e0e14fcd0e075
    Aba                d9a8da25d5656b016fb4dbdc2e4197fb
    Us_account         faf03819c021ce77bf9689ca9a8ede35
    Lightning.Target   3b138a460675950e7aa544c9dd14b78b
    Beneficiary        a75ca3f385560ca0ba4035db9d691422
    Beneficiary V2     a85e2adcac09faa994c19cf238277036
    Uk_account         6e7545573d853b6b662db8445912a323
    Sca.Challenge      692de572288496b8a81cf5b64932add8
    Transfer           5d0ea6cdb7be87a968a899269f33e433
    Transfer V2        66c69aec0eb90148648f1fde8e873a6b
    Standing_order     39121335464084124a22220017780c99
    Standing_order V2  3d51c4973f10ad2fa929734d2b745ad0
    Destination V2     52993a0e79b0d08349ce4147db1fdeb2
    Destination V3     fd29ade86fca96b926eb651b63b57586
    Beneficiary V3     556ee8156c429ae7803bfdb1fd11635c
    Transfer V3        c61708f73324bc86d303b2309e342f51
    Standing_order V3  6e5220ffab8fcadb0af727b7632b5a8c
    |}]
;;

let%expect_test "a transfer round trips through bin_prot and sexp" =
  let t =
    Transfer.submit (Helpers.draft ()) ~provider_ref:"p-1" ~at:(Helpers.at 1) ()
    |> Or_error.ok_exn
  in
  let bytes = Bin_prot.Writer.to_string Transfer.Stable.V1.bin_writer_t t in
  let back = Bin_prot.Reader.of_string Transfer.Stable.V1.bin_reader_t bytes in
  [%test_result: Transfer.t] back ~expect:t;
  [%test_result: Transfer.t] (Transfer.t_of_sexp (Transfer.sexp_of_t t)) ~expect:t;
  print_s [%sexp (t : Transfer.t)];
  [%expect
    {|
    ((id tr-1) (direction Out) (rail Sepa_credit) (account customer:1/eur)
     (counterparty
      (Beneficiary
       ((id ben-1) (owner customer-1) (name "Ada Example")
        (destination (Bank_account (iban DE89370400440532013000) (bic ())))
        (trusted false) (first_seen "2026-10-13 09:00:00Z") (payee_check ()))))
     (amount "25.00 EUR") (fee "0.50 EUR") (reference "invoice 42")
     (details
      (Sepa
       ((payment_id
         ((end_to_end_id E2E-0001) (instruction_id ()) (uetr ()) (tx_id ())
          (msg_id ()) (pmt_inf_id ())))
        (debtor
         ((name "Customer One") (iban DE77100100100123456789) (bic ())
          (address ())))
        (creditor
         ((name "Ada Example") (iban DE89370400440532013000) (bic ())
          (address ())))
        (remittance (Unstructured "invoice 42")) (charge_bearer SLEV)
        (purpose ()) (requested_execution_date ())
        (acceptance_time ("2026-10-13 09:00:01Z")) (value_date ())
        (booking_date ()) (last_status ()))))
     (status Submitted)
     (history
      ((Submitted "2026-10-13 09:00:01Z") (Draft "2026-10-13 09:00:00Z")))
     (authorization ((Exempt Low_value_remote))) (provider_ref (p-1)))
    |}]
;;

(* The UK rails were appended after the original six, so every rail V1 can hold is written
   the same way by V2, and old bytes read through either version. *)
let%test_unit "Rail V1 bytes are V2 bytes" =
  Quickcheck.test Rail.Stable.V1.quickcheck_generator ~f:(fun rail ->
    let v1 = Bin_prot.Writer.to_string Rail.Stable.V1.bin_writer_t rail in
    [%test_result: string]
      (Bin_prot.Writer.to_string Rail.Stable.V2.bin_writer_t rail)
      ~expect:v1;
    [%test_result: Rail.t]
      (Bin_prot.Reader.of_string Rail.Stable.V2.bin_reader_t v1)
      ~expect:rail)
;;

let%expect_test "Rail V1 refuses the UK rails" =
  List.iter [ Rail.Faster_payments; Chaps; Bacs ] ~f:(fun rail ->
    Expect_test_helpers_core.require_does_raise (fun () ->
      Bin_prot.Writer.to_string Rail.Stable.V1.bin_writer_t rail));
  [%expect
    {|
    ("Rail.Stable.V1 cannot hold this rail; write Rail.Stable.V2"
     (rail Faster_payments))
    ("Rail.Stable.V1 cannot hold this rail; write Rail.Stable.V2" (rail Chaps))
    ("Rail.Stable.V1 cannot hold this rail; write Rail.Stable.V2" (rail Bacs))
    |}]
;;

let uk_beneficiary =
  Beneficiary.create
    ~id:(Beneficiary.Id.of_string "ben-uk")
    ~owner:"customer-1"
    ~name:"Ada Example"
    ~rail:Faster_payments
    ~now:(Helpers.at 0)
    "04-00-04 12345678"
  |> Or_error.ok_exn
;;

let%expect_test "Beneficiary V1 refuses a UK account; V2 round trips it" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    Bin_prot.Writer.to_string Beneficiary.Stable.V1.bin_writer_t uk_beneficiary);
  let bytes =
    Bin_prot.Writer.to_string Beneficiary.Stable.V2.bin_writer_t uk_beneficiary
  in
  [%test_result: Beneficiary.t]
    (Bin_prot.Reader.of_string Beneficiary.Stable.V2.bin_reader_t bytes)
    ~expect:uk_beneficiary;
  [%expect
    {|
    ("Beneficiary.Destination.Stable.V1 cannot hold a UK account; write V2"
     "04-00-04 12345678")
    |}]
;;

(* Destinations and payee checks gained constructors at the end, so a V1 beneficiary's
   bytes are also valid V2 bytes. *)
let%expect_test "Beneficiary V1 bytes read as V2" =
  let b = Helpers.beneficiary () in
  let v1 = Bin_prot.Writer.to_string Beneficiary.Stable.V1.bin_writer_t b in
  [%test_result: string]
    (Bin_prot.Writer.to_string Beneficiary.Stable.V2.bin_writer_t b)
    ~expect:v1;
  [%test_result: Beneficiary.t]
    (Bin_prot.Reader.of_string Beneficiary.Stable.V2.bin_reader_t v1)
    ~expect:b;
  [%expect {| |}]
;;
