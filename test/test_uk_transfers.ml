open! Core
open Payments_core
open Helpers

let gbp s = Money.Amount.of_decimal_string Money.Currency.gbp s |> Or_error.ok_exn
let gbp_account = account "customer:1/gbp"

let uk_beneficiary =
  Beneficiary.create
    ~id:(Beneficiary.Id.of_string "ben-uk")
    ~owner:"customer-1"
    ~name:"Ada Example"
    ~rail:Faster_payments
    ~now:t0
    "04-00-04 12345678"
  |> Or_error.ok_exn
;;

let uk_draft
  ?(rail = Rail.Faster_payments)
  ?uk_rules
  ?(reference = "rent")
  ?(now = t0)
  amount
  =
  Transfer.draft
    ~id:(Transfer.Id.of_string "tr-uk")
    ~rail
    ~account:gbp_account
    ~beneficiary:uk_beneficiary
    ~amount
    ~reference
    ?uk_rules
    ~now
    ()
;;

let show r =
  print_s
    [%sexp
      (Or_error.map r ~f:(fun (t : Transfer.t) -> t.rail, t.details)
       : (Rail.t * Transfer.Details.t) Or_error.t)]
;;

let%expect_test "drafts on the UK rails" =
  show (uk_draft (gbp "250.00"));
  show (uk_draft (gbp "1000000.01"));
  show (uk_draft (eur "250.00"));
  show (uk_draft ~reference:"a reference over eighteen" (gbp "250.00"));
  (* A bank's own lower limit. *)
  let uk_rules = Uk_scheme.Rules.create ~fps_max_amount:(gbp "10000.00") () |> ok_exn in
  show (uk_draft ~uk_rules (gbp "10000.01"));
  (* CHAPS: same-day value before the cut-off, next business day after it. *)
  show (uk_draft ~rail:Chaps (gbp "1500000.00"));
  show
    (uk_draft
       ~rail:Chaps
       ~now:(Time_ns.of_string_with_utc_offset "2026-10-16 17:30:00Z")
       (gbp "1500000.00"));
  show (uk_draft ~rail:Bacs (gbp "1200.00"));
  (* A UK account cannot be paid over SEPA, nor a SEPA IBAN over Faster Payments. *)
  show (uk_draft ~rail:Sepa_credit (gbp "1.00"));
  show
    (Transfer.draft
       ~id:(Transfer.Id.of_string "tr-de")
       ~rail:Faster_payments
       ~account:gbp_account
       ~beneficiary:(beneficiary ())
       ~amount:(gbp "1.00")
       ~now:t0
       ());
  [%expect
    {|
    (Ok (Faster_payments (Uk_domestic ((scheme_ref ()) (value_date ())))))
    (Error
     ("over the Faster Payments limit" (amount "1000000.01 GBP")
      (max "1000000.00 GBP")))
    (Error ("UK domestic payments are in GBP" (amount "250.00 EUR")))
    (Error
     ("reference too long" (scheme Fps) (max_len 18) "a reference over eighteen"))
    (Error
     ("over the Faster Payments limit" (amount "10000.01 GBP")
      (max "10000.00 GBP")))
    (Ok (Chaps (Uk_domestic ((scheme_ref ()) (value_date (2026-10-13))))))
    (Ok (Chaps (Uk_domestic ((scheme_ref ()) (value_date (2026-10-19))))))
    (Ok (Bacs (Uk_domestic ((scheme_ref ()) (value_date ())))))
    (Error
     ("rail cannot reach beneficiary" (rail Sepa_credit) (beneficiary ben-uk)))
    (Error
     ("rail cannot reach beneficiary" (rail Faster_payments) (beneficiary ben-1)))
    |}]
;;

let%expect_test "a Faster Payment is sent, settles and comes back" =
  let t =
    uk_draft (gbp "250.00")
    |> Or_error.bind ~f:(Transfer.exempt ~exemption:Trusted_beneficiary)
    |> Or_error.bind ~f:(fun t -> Transfer.submit t ~provider_ref:"fps-1" ~at:(at 1) ())
    |> ok_exn
  in
  let step t to_ sec =
    match Transfer.transition t ~to_ ~at:(at sec) with
    | Ok t ->
      print_s [%message "ok" (t.status : Transfer.Status.t)];
      t
    | Error e ->
      print_s [%message "refused" ~_:(e : Error.t)];
      t
  in
  let t = step t Timed_out 2 in
  let t = step t Settled 3 in
  let _ = step t (Returned (Iso_reason.of_code "AC04" |> ok_exn)) 4 in
  [%expect
    {|
    (refused
     ("transition not allowed" (transfer tr-uk) (from Submitted) (to_ Timed_out)
      (rail Faster_payments)))
    (ok (t.status Settled))
    (ok (t.status (Returned AC04)))
    |}]
;;

let%expect_test "an incoming Faster Payment" =
  let incoming amount =
    Transfer.incoming
      ~id:(Transfer.Id.of_string "in-uk")
      ~rail:Faster_payments
      ~account:gbp_account
      ~from:(External { name = "Employer Ltd"; detail = "20-00-00 55779911" })
      ~amount
      ~reference:"salary october 2026 with a long reference"
      ~status:Settled
      ~now:t0
      ()
  in
  show (incoming (gbp "2500.00"));
  show (incoming (eur "2500.00"));
  [%expect
    {|
    (Ok (Faster_payments (Uk_domestic ((scheme_ref ()) (value_date ())))))
    (Error ("UK domestic payments are in GBP" (amount "2500.00 EUR")))
    |}]
;;

let%expect_test "UK transfers round trip through V2 and are refused by V1" =
  let t = uk_draft (gbp "250.00") |> ok_exn in
  let bytes = Bin_prot.Writer.to_string Transfer.Stable.V2.bin_writer_t t in
  [%test_result: Transfer.t]
    (Bin_prot.Reader.of_string Transfer.Stable.V2.bin_reader_t bytes)
    ~expect:t;
  Expect_test_helpers_core.require_does_raise (fun () ->
    Bin_prot.Writer.to_string Transfer.Stable.V1.bin_writer_t t);
  [%expect
    {|
    ("Rail.Stable.V1 cannot hold this rail; write Rail.Stable.V2"
     (rail Faster_payments))
    |}]
;;

(* Every type V1 can hold was extended at the end, so V1 bytes are valid V2 bytes. *)
let%expect_test "a SEPA transfer is written the same by V1 and V2" =
  let t = draft () in
  let v1 = Bin_prot.Writer.to_string Transfer.Stable.V1.bin_writer_t t in
  [%test_result: string]
    (Bin_prot.Writer.to_string Transfer.Stable.V2.bin_writer_t t)
    ~expect:v1;
  [%test_result: Transfer.t]
    (Bin_prot.Reader.of_string Transfer.Stable.V2.bin_reader_t v1)
    ~expect:t;
  [%expect {| |}]
;;

let%expect_test "a monthly Faster Payments standing order" =
  let order =
    Standing_order.create
      ~id:(Standing_order.Id.of_string "so-uk")
      ~account:gbp_account
      ~beneficiary:uk_beneficiary
      ~rail:Faster_payments
      ~amount:(gbp "900.00")
      ~reference:"rent"
      ~schedule:(Monthly { day = 1 })
      ~start:(Date.of_string "2026-11-01")
      ()
    |> ok_exn
  in
  let bytes = Bin_prot.Writer.to_string Standing_order.Stable.V2.bin_writer_t order in
  [%test_result: Standing_order.t]
    (Bin_prot.Reader.of_string Standing_order.Stable.V2.bin_reader_t bytes)
    ~expect:order;
  print_s [%sexp (order.next_run : Date.t), (order.rail : Rail.t)];
  [%expect {| (2026-11-01 Faster_payments) |}]
;;
