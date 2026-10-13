open! Core
open Payments_core

(* A changed digest means old bytes no longer decode: add a Stable.V2 instead of editing
   V1, and keep V1 readable. *)
let%expect_test "stable bin_prot digests" =
  List.iter
    [ "Network", [%bin_digest: Network.Stable.V1.t]
    ; "Rail", [%bin_digest: Rail.Stable.V1.t]
    ; "Iban", [%bin_digest: Iban.Stable.V1.t]
    ; "Crypto_address", [%bin_digest: Crypto_address.Stable.V1.t]
    ; "Lightning.Target", [%bin_digest: Lightning.Target.Stable.V1.t]
    ; "Beneficiary", [%bin_digest: Beneficiary.Stable.V1.t]
    ; "Sca.Challenge", [%bin_digest: Sca.Challenge.Stable.V1.t]
    ; "Transfer", [%bin_digest: Transfer.Stable.V1.t]
    ; "Standing_order", [%bin_digest: Standing_order.Stable.V1.t]
    ]
    ~f:(fun (name, digest) -> printf "%-18s %s\n" name digest);
  [%expect
    {|
    Network            180a350eb7c08de68cbf9d0f401227c7
    Rail               8dfb73bd6a838ed1c3162c265c00348e
    Iban               d9a8da25d5656b016fb4dbdc2e4197fb
    Crypto_address     43b704f271695d2835e803b2791b70ee
    Lightning.Target   3b138a460675950e7aa544c9dd14b78b
    Beneficiary        a75ca3f385560ca0ba4035db9d691422
    Sca.Challenge      692de572288496b8a81cf5b64932add8
    Transfer           5d0ea6cdb7be87a968a899269f33e433
    Standing_order     39121335464084124a22220017780c99
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
