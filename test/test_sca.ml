open! Core
open Async
open Payments_core

let t0 = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"
let at sec = Time_ns.add t0 (Time_ns.Span.of_int_sec sec)
let binding = Sca.Binding.create ~operation:"transfer" ~amount:"EUR 25.00" ~payee:"DE89370400440532013000"

let challenge () =
  Sca.Challenge.create
    ~id:(Sca.Id.of_string "ch-1")
    ~kind:Otp
    ~subject:"c-1"
    ~binding
    ~now:t0
    ()
;;

(* A verifier that accepts one fixed code, standing in for an OTP store. *)
module Fixed_code = struct
  let check (_ : Sca.Challenge.t) ~response = return (String.equal response "123456")
end

let run c ~response ~now ~binding =
  let%map c, outcome = Sca.verify (module Fixed_code) c ~binding ~response ~now in
  Core.print_s [%message (outcome : Sca.Outcome.t) ~status:(c.status : Sca.Status.t) ~attempts:(c.attempts : int)];
  c
;;

let%expect_test "wrong, then right" =
  let c = challenge () in
  let%bind c = run c ~response:"000000" ~now:(at 10) ~binding in
  let%bind c = run c ~response:"123456" ~now:(at 20) ~binding in
  let%bind _ = run c ~response:"123456" ~now:(at 30) ~binding in
  [%expect {|
    ((outcome (Wrong_response (remaining 2))) (status Pending) (attempts 1))
    ((outcome Verified) (status (Verified (at "2026-10-13 09:00:20Z")))
     (attempts 1))
    ((outcome Already_verified) (status (Verified (at "2026-10-13 09:00:20Z")))
     (attempts 1))
    |}];
  return ()
;;

let%expect_test "locks after three wrong answers" =
  let%bind c =
    Deferred.List.fold [ 1; 2; 3; 4 ] ~init:(challenge ()) ~f:(fun c i ->
      run c ~response:"999999" ~now:(at i) ~binding)
  in
  let%bind _ = run c ~response:"123456" ~now:(at 5) ~binding in
  [%expect {|
    ((outcome (Wrong_response (remaining 2))) (status Pending) (attempts 1))
    ((outcome (Wrong_response (remaining 1))) (status Pending) (attempts 2))
    ((outcome (Rejected Too_many_attempts)) (status (Failed Too_many_attempts))
     (attempts 3))
    ((outcome (Rejected Too_many_attempts)) (status (Failed Too_many_attempts))
     (attempts 3))
    ((outcome (Rejected Too_many_attempts)) (status (Failed Too_many_attempts))
     (attempts 3))
    |}];
  return ()
;;

let%expect_test "expiry and binding" =
  let c = challenge () in
  let other = Sca.Binding.create ~operation:"transfer" ~amount:"EUR 2500.00" ~payee:"DE89370400440532013000" in
  let%bind _ = run c ~response:"123456" ~now:(at 10) ~binding:other in
  let%bind _ = run c ~response:"123456" ~now:(at 300) ~binding in
  let%bind _ = run (Sca.Challenge.cancel c) ~response:"123456" ~now:(at 10) ~binding in
  [%expect {|
    ((outcome Binding_mismatch) (status Pending) (attempts 0))
    ((outcome (Rejected Expired)) (status (Failed Expired)) (attempts 0))
    ((outcome (Rejected Cancelled)) (status (Failed Cancelled)) (attempts 0))
    |}];
  return ()
;;

let%expect_test "TOTP matches the RFC 6238 SHA-1 vectors" =
  let secret = "12345678901234567890" in
  List.iter [ 59; 1111111109; 1111111111; 1234567890; 2000000000 ] ~f:(fun s ->
    let time = Time_ns.of_span_since_epoch (Time_ns.Span.of_int_sec s) in
    Core.printf "%d %s\n" s (Sca.Totp.code ~secret ~time ~digits:8));
  let time = Time_ns.of_span_since_epoch (Time_ns.Span.of_int_sec 59) in
  let prev = Sca.Totp.code ~secret ~time:(Time_ns.sub time (Time_ns.Span.of_int_sec 30)) ~digits:6 in
  Core.print_s
    [%message
      (Sca.Totp.check ~secret ~time prev : bool)
        (Sca.Totp.check ~secret ~time ~window:0 prev : bool)];
  [%expect {|
    59 94287082
    1111111109 07081804
    1111111111 14050471
    1234567890 89005924
    2000000000 69279037
    (("Sca.Totp.check ~secret ~time prev" true)
     ("Sca.Totp.check ~secret ~time ~window:0 prev" false))
    |}];
  return ()
;;
