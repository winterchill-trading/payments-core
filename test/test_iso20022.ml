open! Core
open Payments_core
open Iso20022

let show f x = print_s [%sexp (Result.map (f x) ~f:(fun _ -> "ok") : string Or_error.t)]

let%expect_test "creditor references (ISO 11649)" =
  List.iter
    [ "RF18 5390 0754 7034"; "RF18000000000539007547034"; "RF19 5390 0754 7034"; "RF18 5390-0754" ]
    ~f:(show Creditor_reference.of_string);
  print_s [%sexp (Creditor_reference.of_body "539007547034" : Creditor_reference.t Or_error.t)];
  [%expect {|
    (Ok ok)
    (Ok ok)
    (Error ("invalid ISO 11649 creditor reference" "RF19 5390 0754 7034"))
    (Error ("invalid ISO 11649 creditor reference" "RF18 5390-0754"))
    (Ok RF18539007547034)
    |}]
;;

let%expect_test "generated creditor references validate" =
  Quickcheck.test
    (String.gen_with_length 12 Quickcheck.Generator.char_alphanum)
    ~sexp_of:String.sexp_of_t
    ~f:(fun body ->
      let r = Creditor_reference.of_body body |> Or_error.ok_exn in
      [%test_result: Creditor_reference.t]
        (Creditor_reference.of_string (Creditor_reference.to_string r) |> Or_error.ok_exn)
        ~expect:r)
;;

let%expect_test "identifiers and text" =
  List.iter [ "E2E-2026-10-13/0001"; "/leading"; "a//b"; "naïve"; String.make 36 'x' ]
    ~f:(show (fun s -> Payment_id.create ~end_to_end_id:s ()));
  show Uetr.of_string "eb6305c9-1f7f-49de-aed0-16487c27b42d";
  show Uetr.of_string "eb6305c9-1f7f-39de-aed0-16487c27b42d";
  let state = Random.State.make [| 42 |] in
  Quickcheck.test Unit.quickcheck_generator ~sexp_of:Unit.sexp_of_t ~f:(fun () ->
    ignore (Uetr.of_string (Uetr.to_string (Uetr.create state)) |> Or_error.ok_exn : Uetr.t));
  show Remittance.unstructured "Invoice 2026-118, thank you";
  show Remittance.unstructured (String.make 141 'a');
  [%expect {|
    (Ok ok)
    (Error
     ("identifier with a leading, trailing or double slash" end_to_end_id
      /leading))
    (Error
     ("identifier with a leading, trailing or double slash" end_to_end_id a//b))
    (Error ("characters outside the SEPA set" end_to_end_id "na\195\175ve"))
    (Error ("field too long" end_to_end_id (max_length 35) (length 36)))
    (Ok ok)
    (Error ("UETR must be a UUID v4" eb6305c9-1f7f-39de-aed0-16487c27b42d))
    (Ok ok)
    (Error ("field too long" remittance (max_length 140) (length 141)))
    |}]
;;

let%expect_test "addresses" =
  let a : Postal_address.t =
    { country = "DE"
    ; town = "Berlin"
    ; post_code = Some "10115"
    ; street = Some "Invalidenstrasse"
    ; building_number = Some "1"
    ; lines = []
    }
  in
  show Postal_address.validate a;
  show Postal_address.validate { a with country = "Germany" };
  show Postal_address.validate { a with lines = [ "a"; "b"; "c" ] };
  [%expect {|
    (Ok ok)
    (Error ("country must be ISO 3166 alpha-2" Germany))
    (Error "at most two address lines")
    |}]
;;
