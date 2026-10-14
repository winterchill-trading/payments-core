open! Core
open Payments_core

let gbp s = Money.Amount.of_decimal_string Money.Currency.gbp s |> ok_exn
let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> ok_exn
let utc s = Time_ns.of_string_with_utc_offset s
let show r = print_s [%sexp (r : unit Or_error.t)]

let%expect_test "amounts" =
  show (Uk_scheme.validate Fps ~amount:(gbp "1000000.00") ~reference:"rent");
  show (Uk_scheme.validate Fps ~amount:(gbp "1000000.01") ~reference:"rent");
  show (Uk_scheme.validate Chaps ~amount:(gbp "25000000.00") ~reference:"completion");
  show (Uk_scheme.validate Bacs ~amount:(eur "10.00") ~reference:"");
  show (Uk_scheme.validate Fps ~amount:(gbp "0.00") ~reference:"");
  [%expect
    {|
    (Ok ())
    (Error
     ("over the Faster Payments limit" (amount "1000000.01 GBP")
      (max "1000000.00 GBP")))
    (Ok ())
    (Error ("UK domestic payments are in GBP" (amount "10.00 EUR")))
    (Error ("amount must be positive" (amount "0.00 GBP")))
    |}]
;;

let%expect_test "a bank's own Faster Payments limit" =
  let rules = Uk_scheme.Rules.create ~fps_max_amount:(gbp "25000.00") () |> ok_exn in
  show (Uk_scheme.validate ~rules Fps ~amount:(gbp "25000.00") ~reference:"");
  show (Uk_scheme.validate ~rules Fps ~amount:(gbp "25000.01") ~reference:"");
  (* CHAPS has no limit, whatever the Faster Payments one. *)
  show (Uk_scheme.validate ~rules Chaps ~amount:(gbp "25000.01") ~reference:"");
  print_s
    [%sexp
      (Uk_scheme.Rules.create ~fps_max_amount:(gbp "2000000.00") () |> Result.is_ok
       : bool)];
  print_s
    [%sexp
      (Uk_scheme.Rules.create ~fps_max_amount:(eur "100.00") () |> Result.is_ok : bool)];
  [%expect
    {|
    (Ok ())
    (Error
     ("over the Faster Payments limit" (amount "25000.01 GBP")
      (max "25000.00 GBP")))
    (Ok ())
    false
    false
    |}]
;;

let%expect_test "references" =
  show (Uk_scheme.validate Fps ~amount:(gbp "1.00") ~reference:"INVOICE 123456 ABC");
  show (Uk_scheme.validate Fps ~amount:(gbp "1.00") ~reference:"INVOICE 1234567 ABC");
  show (Uk_scheme.validate Chaps ~amount:(gbp "1.00") ~reference:(String.make 140 'x'));
  show (Uk_scheme.validate Bacs ~amount:(gbp "1.00") ~reference:"PAYROLL 10/2026");
  show (Uk_scheme.validate Bacs ~amount:(gbp "1.00") ~reference:"PAYROLL #10");
  show (Uk_scheme.validate Fps ~amount:(gbp "1.00") ~reference:"tab\there");
  [%expect
    {|
    (Ok ())
    (Error
     ("reference too long" (scheme Fps) (max_len 18) "INVOICE 1234567 ABC"))
    (Ok ())
    (Ok ())
    (Error ("character not allowed in the reference" (scheme Bacs) (c #)))
    (Error ("character not allowed in the reference" (scheme Fps) (c "\t")))
    |}]
;;

let%expect_test "CHAPS cut-off and value date" =
  let cutoff d =
    print_s
      [%sexp
        (Uk_scheme.default_chaps_cutoff (Date.of_string d)
         |> Option.map ~f:(Time_ns.to_string_iso8601_basic ~zone:Time_ns.Zone.utc)
         : string option)]
  in
  (* Wednesday in summer time, Monday after the clocks go back, a Saturday. *)
  cutoff "2026-10-14";
  cutoff "2026-10-26";
  cutoff "2026-10-24";
  let value now =
    print_s
      [%sexp
        (Uk_scheme.chaps_value_date Uk_scheme.Rules.default ~now:(utc now)
         : Date.t Or_error.t)]
  in
  value "2026-10-14 16:59:59Z";
  value "2026-10-14 17:00:01Z";
  (* Friday after the cut-off: Monday. *)
  value "2026-10-16 17:30:00Z";
  (* A bank's hook that knows Christmas Day and the Boxing Day substitute holiday. *)
  let rules =
    Uk_scheme.Rules.create
      ~chaps_cutoff:(fun d ->
        if List.mem [ "2026-12-25"; "2026-12-28" ] (Date.to_string d) ~equal:String.equal
        then None
        else Uk_scheme.default_chaps_cutoff d)
      ()
    |> ok_exn
  in
  print_s
    [%sexp
      (Uk_scheme.chaps_value_date rules ~now:(utc "2026-12-25 12:00:00Z")
       : Date.t Or_error.t)];
  [%expect
    {|
    (2026-10-14T17:00:00.000000000Z)
    (2026-10-26T18:00:00.000000000Z)
    ()
    (Ok 2026-10-14)
    (Ok 2026-10-15)
    (Ok 2026-10-19)
    (Ok 2026-12-29)
    |}]
;;

let%expect_test "rails" =
  List.iter Uk_scheme.Scheme.all ~f:(fun s ->
    let rail = Uk_scheme.Scheme.to_rail s in
    print_s
      [%sexp
        (s : Uk_scheme.Scheme.t)
        , (rail : Rail.t)
        , (Option.equal Uk_scheme.Scheme.equal (Uk_scheme.Scheme.of_rail rail) (Some s)
           : bool)]);
  [%expect
    {|
    (Fps Faster_payments true)
    (Chaps Chaps true)
    (Bacs Bacs true)
    |}]
;;
