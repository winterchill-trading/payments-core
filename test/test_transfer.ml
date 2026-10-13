open! Core
open Payments_core
open Helpers

let step t to_ sec =
  match Transfer.transition t ~to_ ~at:(at sec) with
  | Ok t ->
    print_s [%message "ok" ~status:(t.status : Transfer.Status.t)];
    t
  | Error e ->
    print_s [%message "refused" ~_:(e : Error.t)];
    t
;;

let%expect_test "allowed transitions table" =
  List.iter Transfer.allowed_transitions ~f:(fun (from, tos) ->
    printf "%-13s -> %s\n" from (String.concat ~sep:", " tos));
  [%expect
    {|
    Draft         -> Awaiting_sca, Submitted, Cancelled, Failed
    Awaiting_sca  -> Submitted, Cancelled, Failed
    Submitted     -> Pending, Settled, Rejected, Failed, Timed_out
    Pending       -> Settled, Rejected, Failed, Timed_out, Reorged
    Settled       -> Returned, Recall_requested, Reorged
    Timed_out     -> Settled, Rejected
    Reorged       -> Pending
    Recall_requested -> Recalled, Recall_refused, Returned
    Rejected      ->
    Failed        ->
    Returned      ->
    Recall_refused ->
    Recalled      ->
    Cancelled     ->
    |}]
;;

let%expect_test "outgoing SEPA lifecycle with SCA and a return" =
  let t = draft () in
  let t =
    Transfer.require_sca t ~challenge:(Sca.Id.of_string "ch-1") ~at:(at 1)
    |> Or_error.ok_exn
  in
  let t = Transfer.submit t ~provider_ref:"prov-77" ~at:(at 5) () |> Or_error.ok_exn in
  let t = step t Pending 6 in
  let t = step t Pending 7 in
  let t = step t Settled 3600 in
  let t = step t (Returned MS02) 7200 in
  let t = step t Settled 7300 in
  print_s
    [%message
      (Transfer.is_terminal t : bool)
        (Transfer.total_debit t : Money.Amount.t)
        (t.provider_ref : string option)
        ~history:(List.rev t.history : (Transfer.Status.t * Utc_time.t) list)];
  [%expect
    {|
    (ok (status Pending))
    (refused
     ("transition not allowed" (transfer tr-1) (from Pending) (to_ Pending)
      (rail Sepa_credit)))
    (ok (status Settled))
    (ok (status (Returned MS02)))
    (refused
     ("transition not allowed" (transfer tr-1) (from (Returned MS02))
      (to_ Settled) (rail Sepa_credit)))
    (("Transfer.is_terminal t" true) ("Transfer.total_debit t" "25.50 EUR")
     (t.provider_ref (prov-77))
     (history
      ((Draft "2026-10-13 09:00:00Z") (Awaiting_sca "2026-10-13 09:00:01Z")
       (Submitted "2026-10-13 09:00:05Z") (Pending "2026-10-13 09:00:06Z")
       (Settled "2026-10-13 10:00:00Z") ((Returned MS02) "2026-10-13 11:00:00Z"))))
    |}]
;;

let%expect_test "final rails: on-chain cannot return, settled is terminal" =
  let b = beneficiary ~rail:Lightning ~raw:"satoshi@example.com" () in
  let t =
    Transfer.draft
      ~id:(Transfer.Id.of_string "tr-ln")
      ~rail:Lightning
      ~account:(account "customer:1/btc")
      ~beneficiary:b
      ~amount:(Money.Amount.of_string "0.00010000 BTC")
      ~now:t0
      ()
    |> Or_error.ok_exn
  in
  let t = step t Submitted 1 in
  let t = step t Settled 2 in
  let t = step t (Returned MS02) 3 in
  print_s [%message (Transfer.is_terminal t : bool)];
  [%expect
    {|
    (ok (status Submitted))
    (ok (status Settled))
    (refused
     ("transition not allowed" (transfer tr-ln) (from Settled)
      (to_ (Returned MS02)) (rail Lightning)))
    ("Transfer.is_terminal t" true)
    |}]
;;

let%expect_test "draft validation" =
  let try_ ?(rail = Rail.Sepa_credit) amount fee =
    match
      Transfer.draft
        ~details:(sepa_details ())
        ~id:(Transfer.Id.of_string "tr-x")
        ~rail
        ~account:(account "customer:1/eur")
        ~beneficiary:(beneficiary ())
        ~amount
        ~fee
        ~now:t0
        ()
    with
    | Ok _ -> print_endline "ok"
    | Error e -> print_s [%message "rejected" ~_:(e : Error.t)]
  in
  try_ (eur "0.00") (eur "0");
  try_ (eur "1.00") (eur "-0.01");
  try_ (eur "1.00") (Money.Amount.of_string "0.00000001 BTC");
  try_ ~rail:Lightning (eur "1.00") (eur "0");
  try_ (eur "1.00") (eur "0");
  [%expect
    {|
    (rejected ("amount must be positive" (amount "0.00 EUR")))
    (rejected ("fee must not be negative" (fee "-0.01 EUR")))
    (rejected
     ("fee currency differs" (amount "1.00 EUR") (fee "0.00000001 BTC")))
    (rejected
     ("rail cannot reach beneficiary" (rail Lightning) (beneficiary ben-1)))
    ok
    |}]
;;

let%expect_test "incoming fee may not exceed the amount" =
  (* Found by the booking property: the fee leg would overdraw the customer. *)
  Transfer.incoming
    ~id:(Transfer.Id.of_string "in-x")
    ~rail:Sepa_credit
    ~details:(sepa_details ())
    ~account:(account "customer:1/eur")
    ~from:(External { name = "Payer"; detail = "test" })
    ~amount:(eur "0.01")
    ~fee:(eur "1.16")
    ~status:Pending
    ~now:t0
    ()
  |> Result.iter_error ~f:(fun e -> print_s [%sexp (e : Error.t)]);
  [%expect {| ("fee exceeds the incoming amount" (amount "0.01 EUR") (fee "1.16 EUR")) |}]
;;
