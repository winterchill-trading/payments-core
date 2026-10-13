open! Core
open Async
open Payments_core

let t0 = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"
let at sec = Time_ns.add t0 (Time_ns.Span.of_int_sec sec)
let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn
let binding = Sca.Binding.create ~operation:"transfer" ~amount:(eur "25.00") ~payee:"ben-1"

let challenge ?(factors = Sca.Kind.[ Otp; Pin ]) ?max_attempts () =
  Sca.Challenge.create
    ~id:(Sca.Id.of_string "ch-1")
    ~subject:"c-1"
    ~binding
    ~factors
    ~now:t0
    ?max_attempts
    ()
;;

(* A verifier that accepts one fixed answer per factor, standing in for real checks. *)
module Fixed = struct
  let check (_ : Sca.Challenge.t) ~(kind : Sca.Kind.t) ~response =
    return
      (match kind with
       | Otp -> String.equal response "123456"
       | Pin -> String.equal response "4242"
       | _ -> false)
  ;;
end

let run ?(binding = binding) c ~kind ~response ~now =
  let%map c, outcome = Sca.verify (module Fixed) c ~binding ~kind ~response ~now in
  Core.print_s
    [%message
      (outcome : Sca.Outcome.t)
        ~status:(c.status : Sca.Status.t)
        ~elements:(Sca.Challenge.elements_passed c : Sca.Element.t list)];
  c
;;

let%expect_test "two categories are required" =
  Core.print_s
    [%sexp
      (challenge ~factors:[ Otp; Totp; Passkey ] () |> Result.map ~f:(fun _ -> ())
       : unit Or_error.t)];
  Core.print_s
    [%sexp (challenge ~max_attempts:6 () |> Result.map ~f:(fun _ -> ()) : unit Or_error.t)];
  [%expect {|
    (Error
     ("SCA needs factors from two element categories"
      (factors (Otp Totp Passkey))))
    (Error ("max_attempts must be 1..5" (max_attempts 6)))
    |}];
  return ()
;;

let%expect_test "possession then knowledge verifies; a used challenge authorises nothing" =
  let c = challenge () |> Or_error.ok_exn in
  let%bind c = run c ~kind:Otp ~response:"123456" ~now:(at 5) in
  let%bind c = run c ~kind:Pin ~response:"0000" ~now:(at 10) in
  let%bind c = run c ~kind:Pin ~response:"4242" ~now:(at 15) in
  let%bind _ = run c ~kind:Pin ~response:"4242" ~now:(at 20) in
  [%expect {|
    ((outcome (Factor_accepted (remaining (Pin)))) (status Pending)
     (elements (Possession)))
    ((outcome (Wrong_response (attempts_left 2))) (status Pending)
     (elements (Possession)))
    ((outcome Verified) (status (Verified (at "2026-10-13 09:00:15Z")))
     (elements (Knowledge Possession)))
    ((outcome Already_verified) (status (Verified (at "2026-10-13 09:00:15Z")))
     (elements (Knowledge Possession)))
    |}];
  return ()
;;

let%expect_test "locks after the configured wrong answers, no factor named" =
  let%bind c =
    Deferred.List.fold [ 1; 2; 3 ] ~init:(challenge () |> Or_error.ok_exn) ~f:(fun c i ->
      run c ~kind:Pin ~response:"9999" ~now:(at i))
  in
  let%bind _ = run c ~kind:Otp ~response:"123456" ~now:(at 5) in
  [%expect {|
    ((outcome (Wrong_response (attempts_left 2))) (status Pending) (elements ()))
    ((outcome (Wrong_response (attempts_left 1))) (status Pending) (elements ()))
    ((outcome (Rejected Too_many_attempts)) (status (Failed Too_many_attempts))
     (elements ()))
    ((outcome (Rejected Too_many_attempts)) (status (Failed Too_many_attempts))
     (elements ()))
    |}];
  return ()
;;

let%expect_test "binding, factor kind and expiry" =
  let c = challenge () |> Or_error.ok_exn in
  let other = Sca.Binding.create ~operation:"transfer" ~amount:(eur "2500.00") ~payee:"ben-1" in
  let other_currency =
    Sca.Binding.create ~operation:"transfer" ~amount:(Money.Amount.of_string "25.00 USD") ~payee:"ben-1"
  in
  let%bind _ = run c ~binding:other ~kind:Otp ~response:"123456" ~now:(at 10) in
  let%bind _ = run c ~binding:other_currency ~kind:Otp ~response:"123456" ~now:(at 10) in
  let%bind _ = run c ~kind:Biometric ~response:"x" ~now:(at 10) in
  let%bind _ = run c ~kind:Otp ~response:"123456" ~now:(at 300) in
  [%expect {|
    ((outcome Binding_mismatch) (status Pending) (elements ()))
    ((outcome Binding_mismatch) (status Pending) (elements ()))
    ((outcome (Not_a_factor Biometric)) (status Pending) (elements ()))
    ((outcome (Rejected Expired)) (status (Failed Expired)) (elements ()))
    |}];
  return ()
;;

let%expect_test "per-customer lock: temporary, then permanent within a day" =
  let fail_5 lock ~from =
    List.fold (List.range 0 5) ~init:(lock, None) ~f:(fun (lock, _) i ->
      Sca.Lock.record_failure lock ~now:(at (from + i)))
  in
  let lock, notice = fail_5 Sca.Lock.empty ~from:0 in
  Core.print_s
    [%message
      (lock.status : Sca.Lock.status)
        (notice : Sca.Lock.Notice.t option)
        (Sca.Lock.is_locked lock ~now:(at 60) : bool)
        (Sca.Lock.is_locked lock ~now:(at 1900) : bool)];
  let lock, notice = fail_5 lock ~from:2000 in
  Core.print_s [%message (lock.status : Sca.Lock.status) (notice : Sca.Lock.Notice.t option)];
  [%expect {|
    ((lock.status (Temporary (until "2026-10-13 09:30:04Z")))
     (notice (Locked_temporarily_next_is_permanent))
     ("Sca.Lock.is_locked lock ~now:(at 60)" true)
     ("Sca.Lock.is_locked lock ~now:(at 1900)" false))
    ((lock.status Permanent) (notice (Locked_permanently)))
    |}];
  return ()
;;

let%expect_test "idle session and exemption counters" =
  let s = Sca.Session.start ~subject:"c-1" ~now:t0 in
  Core.print_s
    [%message
      (Sca.Session.is_active s ~now:(at 300) : bool)
        (Sca.Session.is_active s ~now:(at 301) : bool)
        (Sca.Session.touch s ~now:(at 400) |> Result.is_ok : bool)];
  let c = Sca.Counters.zero in
  let c =
    List.fold [ "20.00"; "20.00"; "20.00"; "20.00" ] ~init:c ~f:(fun c a ->
      Sca.Counters.record_remote c ~amount:(eur a))
  in
  Core.print_s
    [%message
      (Sca.Counters.low_value_remote_ok c ~amount:(eur "20.00") : bool)
        (Sca.Counters.low_value_remote_ok c ~amount:(eur "20.01") : bool)
        (Sca.Counters.low_value_remote_ok Sca.Counters.zero ~amount:(eur "30.01") : bool)
        (Sca.Counters.contactless_ok Sca.Counters.zero ~amount:(eur "50.00") : bool)
        (List.map
           Sca.Exemption.
             [ Trusted_beneficiary; Low_value_remote; Tra { band = Eur_250; score = 12 } ]
           ~f:Sca.Exemption.article
         : string list)];
  [%expect {|
    (("Sca.Session.is_active s ~now:(at 300)" true)
     ("Sca.Session.is_active s ~now:(at 301)" false)
     ("(Sca.Session.touch s ~now:(at 400)) |> Result.is_ok" false))
    (("Sca.Counters.low_value_remote_ok c ~amount:(eur \"20.00\")" true)
     ("Sca.Counters.low_value_remote_ok c ~amount:(eur \"20.01\")" false)
     ("Sca.Counters.low_value_remote_ok Sca.Counters.zero ~amount:(eur \"30.01\")"
      false)
     ("Sca.Counters.contactless_ok Sca.Counters.zero ~amount:(eur \"50.00\")"
      true)
     ( "List.map\
      \n  (let open Sca.Exemption in\
      \n     [Trusted_beneficiary;\
      \n     Low_value_remote;\
      \n     Tra { band = Eur_250; score = 12 }]) ~f:Sca.Exemption.article"
      ("Art 13" "Art 16" "Art 18")))
    |}];
  return ()
;;

let%expect_test "audit event never carries the code" =
  let c = challenge () |> Or_error.ok_exn in
  let c, outcome = Sca.record c ~now:(at 1) ~kind:Otp ~correct:true in
  Core.print_s
    [%sexp
      (Sca.Event.of_outcome
         c
         outcome
         ~operation:"transfer"
         ~amount:(eur "25.00")
         ~payee:"ben-1"
         ~kind:Otp
         ~channel:"web"
         ~lock:Unlocked
         ~at:(at 1)
         ()
       : Sca.Event.t)];
  [%expect {|
    ((challenge ch-1) (subject c-1) (operation transfer) (amount ("25.00 EUR"))
     (payee (ben-1))
     (binding c3fa9095133f6a2f8dfd6d27b20b59931343a30d11e918ba00baee5e3616b9c1)
     (kind (Otp)) (elements_passed (Possession)) (channel web) (device_id ())
     (at "2026-10-13 09:00:01Z") (attempt 0) (result Factor_accepted)
     (lock Unlocked) (exemption ()))
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
