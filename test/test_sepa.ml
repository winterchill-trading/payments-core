open! Core
open Payments_core
open Helpers

let ok_or_error r = print_s [%sexp (Result.map r ~f:(fun _ -> "ok") : string Or_error.t)]

let details ?(charge_bearer = Iso20022.Charge_bearer.SLEV) ?(creditor_name = "Ada Example") () =
  match sepa_details () with
  | Sepa d -> { d with charge_bearer; creditor = { d.creditor with name = creditor_name } }
  | _ -> assert false
;;

let%expect_test "scheme validation" =
  ok_or_error (Sepa.validate Sct (details ()) ~amount:(eur "10.00"));
  ok_or_error (Sepa.validate Sct (details ()) ~amount:(Money.Amount.of_string "10.00 USD"));
  ok_or_error (Sepa.validate Sct (details ~charge_bearer:SHAR ()) ~amount:(eur "10.00"));
  ok_or_error (Sepa.validate Sct (details ~creditor_name:"Zoë" ()) ~amount:(eur "10.00"));
  ok_or_error (Sepa.validate Sct_inst (details ()) ~amount:(eur "250000.00"));
  [%expect {|
    (Ok ok)
    (Error ("SEPA transfers are in EUR" (amount "10.00 USD")))
    (Error ("SEPA requires charge bearer SLEV" (got SHAR)))
    (Error ("characters outside the SEPA set" name "Zo\195\171"))
    (Ok ok)
    |}]
;;

let%expect_test "reason codes per R-transaction" =
  List.iter
    [ Sepa.Scheme.Sct, Sepa.Use.Reject, Iso_reason.AB05
    ; Sct_inst, Reject, AB05
    ; Sct, Return, AC04
    ; Sct, Return, DUPL
    ; Sct, Recall, DUPL
    ; Sct, Recall_refusal, NOAS
    ; Sct, Reject, Other "ZZ99"
    ]
    ~f:(fun (scheme, use, reason) -> ok_or_error (Sepa.check_reason scheme use reason));
  [%expect {|
    (Error
     ("reason code not allowed by the rulebook" (scheme Sct) (use Reject)
      (reason AB05)))
    (Ok ok)
    (Ok ok)
    (Error
     ("reason code not allowed by the rulebook" (scheme Sct) (use Return)
      (reason DUPL)))
    (Ok ok)
    (Ok ok)
    (Error
     ("reason code not allowed by the rulebook" (scheme Sct) (use Reject)
      (reason ZZ99)))
    |}]
;;

let show (t : Transfer.t) =
  let last_status =
    match t.details with
    | Sepa d -> d.last_status
    | _ -> None
  in
  print_s
    [%message
      ""
        ~status:(t.status : Transfer.Status.t)
        (last_status : Iso20022.Transaction_status.t option)]
;;

let submitted ?(rail = Rail.Sepa_credit) () =
  Transfer.submit (draft ~rail ()) ~provider_ref:"p-1" ~at:(at 0) () |> Or_error.ok_exn
;;

let%expect_test "pacs.002 status reports drive the lifecycle" =
  let t = submitted () in
  let report ?reason code t =
    match Transfer.apply_status_report t code ?reason ~at:(at 1) () with
    | Ok t ->
      show t;
      t
    | Error e ->
      print_s [%sexp (e : Error.t)];
      t
  in
  let t = report ACTC t in
  let t = report ACSP t in
  let t = report PDNG t in
  let _ = report ACCC t in
  let _ = report RJCT t in
  let _ = report RJCT ~reason:AC04 t in
  let _ = report RJCT ~reason:DUPL t in
  [%expect {|
    ((status Submitted) (last_status (ACTC)))
    ((status Pending) (last_status (ACSP)))
    ((status Pending) (last_status (PDNG)))
    ((status Settled) (last_status (ACCC)))
    "RJCT needs a reason code"
    ((status (Rejected AC04)) (last_status (RJCT)))
    ("reason code not allowed by the rulebook" (scheme Sct) (use Reject)
     (reason DUPL))
    |}]
;;

let%expect_test "SCT Inst times out after 10 s, which releases the hold" =
  let t = submitted ~rail:Sepa_instant () in
  print_s [%sexp (Transfer.check_inst_timeout t ~now:(at 10) |> Option.map ~f:(fun t -> t.status) : Transfer.Status.t option)];
  let rejected = Transfer.check_inst_timeout t ~now:(at 11) |> Option.value_exn in
  show rejected;
  let b = ledger () in
  let b =
    Booking.apply
      b
      (Booking.created chart b
         (Transfer.incoming ~id:(Transfer.Id.of_string "in") ~rail:Sepa_instant ~account:customer
            ~details:(sepa_details ()) ~from:(External { name = "x"; detail = "y" })
            ~amount:(eur "50.00") ~status:Settled ~now:t0 ()
          |> Or_error.ok_exn)
         ~event_id:"in" ~at:t0
       |> Or_error.ok_exn)
    |> Or_error.ok_exn
  in
  let hold = Booking.transition chart b (draft ~rail:Sepa_instant ()) ~to_:Submitted ~event_id:"e1" ~at:(at 0) |> Or_error.ok_exn in
  let b = Booking.apply b hold |> Or_error.ok_exn in
  print_entries
    (Booking.transition chart b t ~to_:rejected.status ~event_id:"e2" ~at:(at 11) |> Or_error.ok_exn);
  [%expect {|
    ()
    ((status Timed_out) (last_status ()))
    e2/release  (Hold_release transfer:tr-1)  ref=tr-1
      debit  customer:1/eur/pending                   25.50 EUR
      credit customer:1/eur                           25.50 EUR
    |}]
;;

let%expect_test "recalls: deadline per reason, refusal and acceptance" =
  let settled_at = Time_ns.of_string_with_utc_offset "2026-10-13 10:00:00Z" in
  let settled = Transfer.transition (submitted ()) ~to_:Settled ~at:settled_at |> Or_error.ok_exn in
  let a_month_later = Time_ns.of_string_with_utc_offset "2026-11-13 10:00:00Z" in
  ok_or_error (Transfer.request_recall settled ~reason:DUPL ~at:a_month_later);
  ok_or_error (Transfer.request_recall settled ~reason:FRAD ~at:a_month_later);
  ok_or_error (Transfer.request_recall settled ~reason:AC04 ~at:(at 3600));
  let recall = Transfer.request_recall settled ~reason:DUPL ~at:(at 3600) |> Or_error.ok_exn in
  show recall;
  show (Transfer.transition recall ~to_:(Recall_refused NOAS) ~at:(at 7200) |> Or_error.ok_exn);
  show (Transfer.transition recall ~to_:Recalled ~at:(at 7200) |> Or_error.ok_exn);
  show (Transfer.transition recall ~to_:(Returned FOCR) ~at:(at 7200) |> Or_error.ok_exn);
  print_s
    [%message
      (Sepa.Deadline.return ~settled_on:(Date.of_string "2026-12-23") : Date.t)
        (Sepa.Deadline.recall ~settled_on:(Date.of_string "2026-10-13") DUPL : Date.t)
        (Sepa.Deadline.recall_answer ~received_on:(Date.of_string "2026-10-13") : Date.t)];
  [%expect {|
    (Error ("recall deadline passed" (deadline 2026-10-27)))
    (Ok ok)
    (Error
     ("reason code not allowed by the rulebook" (scheme Sct) (use Rfro)
      (reason AC04)))
    ((status (Recall_requested DUPL)) (last_status ()))
    ((status (Recall_refused NOAS)) (last_status ()))
    ((status Recalled) (last_status ()))
    ((status (Returned FOCR)) (last_status ()))
    (("Sepa.Deadline.return ~settled_on:(Date.of_string \"2026-12-23\")"
      2026-12-29)
     ("Sepa.Deadline.recall ~settled_on:(Date.of_string \"2026-10-13\") DUPL"
      2026-10-27)
     ("Sepa.Deadline.recall_answer ~received_on:(Date.of_string \"2026-10-13\")"
      2026-11-03))
    |}]
;;

let%expect_test "on-chain transfers carry travel-rule data" =
  let b = beneficiary ~rail:(Onchain Ethereum) ~raw:"0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed" () in
  let usdc = Money.Amount.of_string "250.000000 USDC@ethereum" in
  let draft ?details ?eur_value () =
    Transfer.draft
      ~id:(Transfer.Id.of_string "oc-1")
      ~rail:(Onchain Ethereum)
      ~account:(account "customer:1/usdc")
      ~beneficiary:b
      ~amount:usdc
      ?details
      ?eur_value
      ~now:t0
      ()
  in
  let tr wallet : Transfer.Details.t =
    Onchain
      { travel_rule =
          Some
            { originator =
                { name = "Customer One"; account = "customer:1"; identification = Some (Customer_number "c-1"); lei = None }
            ; beneficiary =
                { name = "Ada Example"; account = "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"; identification = None; lei = None }
            ; counterparty_wallet = wallet
            }
      ; tx_hash = None
      ; block_hash = None
      ; confirmations = 0
      ; finalized = false
      }
  in
  ok_or_error (draft ());
  ok_or_error (draft ~details:(tr (Self_hosted { ownership_verified = false })) ());
  ok_or_error (draft ~details:(tr (Self_hosted { ownership_verified = false })) ~eur_value:(eur "230.00") ());
  ok_or_error (draft ~details:(tr (Hosted { provider_name = "Example Exchange"; provider_lei = None })) ());
  [%expect {|
    (Error "on-chain transfers need travel-rule data")
    (Error
     ("self-hosted wallet ownership must be verified above the threshold"
      (eur_value ())))
    (Ok ok)
    (Ok ok)
    |}]
;;

let%expect_test "SCT Inst: ACCP settles; a late ACCP after the time-out is a fresh debit" =
  let t = submitted ~rail:Sepa_instant () in
  show (Transfer.apply_status_report t ACCP ~at:(at 2) () |> Or_error.ok_exn);
  let timed_out = Transfer.check_inst_timeout t ~now:(at 11) |> Option.value_exn in
  let late = Transfer.apply_status_report timed_out ACCP ~at:(at 30) () |> Or_error.ok_exn in
  show late;
  let b = ledger () in
  let fund =
    Transfer.incoming ~id:(Transfer.Id.of_string "in") ~rail:Sepa_instant ~account:customer
      ~details:(sepa_details ()) ~from:(External { name = "x"; detail = "y" })
      ~amount:(eur "50.00") ~status:Settled ~now:t0 ()
    |> Or_error.ok_exn
  in
  let b = Booking.apply b (Booking.created chart b fund ~event_id:"in" ~at:t0 |> Or_error.ok_exn) |> Or_error.ok_exn in
  print_entries (Booking.transition chart b timed_out ~to_:Settled ~event_id:"late" ~at:(at 30) |> Or_error.ok_exn);
  (* no returns on SCT Inst, only the FOCR answer to a recall *)
  let settled = Transfer.apply_status_report t ACCP ~at:(at 2) () |> Or_error.ok_exn in
  ok_or_error (Transfer.transition settled ~to_:(Returned AC04) ~at:(at 60));
  let recall = Transfer.request_recall settled ~reason:TECH ~at:(at 60) |> Or_error.ok_exn in
  ok_or_error (Transfer.transition recall ~to_:(Returned FOCR) ~at:(at 90));
  [%expect {|
    ((status Settled) (last_status (ACCP)))
    ((status Settled) (last_status (ACCP)))
    late/late-settlement  (Transfer late-settlement)  ref=tr-1
      debit  customer:1/eur                           25.00 EUR
      credit settlement:sepa_instant/EUR              25.00 EUR
    late/fee  (Transfer fee)  ref=tr-1
      debit  customer:1/eur                           0.50 EUR
      credit income:fees/EUR                          0.50 EUR
    (Error
     ("transition not allowed" (transfer tr-1) (from Settled)
      (to_ (Returned AC04)) (rail Sepa_instant)))
    (Ok ok)
    |}]
;;
