open! Core
open Payments_core
module D = Beneficiary.Destination

let show ?formats ~scheme fields =
  match D.check_local ?formats ~scheme fields with
  | Ok d -> print_s [%message "accepted" ~_:(d : D.t)]
  | Error { summary; fields } ->
    print_s [%message "rejected" (fields : D.Field_error.t list) (summary : Error.t)]
;;

let%expect_test "Kyrgyz details: each wrong field is named" =
  let kg = D.Local_scheme.kg in
  show ~scheme:kg [ "bank_code", "109018"; "account_number", "1090 0001 2345 6789" ];
  show ~scheme:kg [ "bank_code", "10901"; "account_number", "109000012345678X" ];
  show ~scheme:kg [ "bik", "109018"; "account_number", "1090000123456789" ];
  [%expect
    {|
    (accepted
     (Local (scheme kg)
      (fields ((bank_code 109018) (account_number 1090000123456789)))))
    (rejected
     (fields
      (((field (bank_code)) (reason "expected 6 digits"))
       ((field (account_number)) (reason "expected 16 digits"))))
     (summary
      (("local destination" kg) ("expected digits" bank_code (length 6) 10901))))
    (rejected
     (fields
      (((field (bank_code)) (reason missing))
       ((field (bik)) (reason "unexpected field"))))
     (summary
      (("local destination" kg)
       ("unexpected fields" (expected (bank_code account_number))
        (given (account_number bik))))))
    |}]
;;

let%expect_test "QR-IBAN, generic rules and a hook without field errors" =
  show ~scheme:D.Local_scheme.ch_qr_iban [ "qr_iban", "CH93 0076 2011 6238 5295 7" ];
  show ~scheme:D.Local_scheme.ch_qr_iban [ "qr_iban", "DE89 3704 0044 0532 0130 00" ];
  show ~scheme:D.Local_scheme.ch_qr_iban [ "qr_iban", "CH00 nonsense" ];
  show ~scheme:"Bad Scheme" [ "a", "1" ];
  show ~scheme:"xx" [];
  show ~scheme:"xx" [ "a", "1"; "Account", "2"; "b", " "; "a", "3" ];
  let refuse =
    D.Local_format.create ~scheme:"xx" ~validate:(fun _ ->
      Or_error.error_string "unknown bank")
  in
  show ~formats:[ refuse ] ~scheme:"xx" [ "code", "1" ];
  [%expect
    {|
    (rejected (fields (((field (qr_iban)) (reason "not a QR-IBAN"))))
     (summary
      (("local destination" ch_qr_iban) ("not a QR-IBAN" CH9300762011623852957))))
    (rejected (fields (((field (qr_iban)) (reason "QR-IBAN outside CH and LI"))))
     (summary
      (("local destination" ch_qr_iban) ("QR-IBAN outside CH and LI" DE))))
    (rejected
     (fields
      (((field (qr_iban)) (reason "(\"IBAN length\" CH (expected 21) (got 12))"))))
     (summary
      (("local destination" ch_qr_iban)
       ("IBAN length" CH (expected 21) (got 12)))))
    (rejected
     (fields (((field ()) (reason "local scheme must be lower snake case"))))
     (summary ("local scheme must be lower snake case" "Bad Scheme")))
    (rejected (fields (((field ()) (reason "no fields")))) (summary "no fields"))
    (rejected
     (fields
      (((field (Account)) (reason "field name must be lower snake case"))
       ((field (b)) (reason "empty field"))
       ((field (a)) (reason "duplicate field"))))
     (summary
      (("field name must be lower snake case" Account) ("empty field" b))))
    (rejected (fields (((field ()) (reason "unknown bank"))))
     (summary (("local destination" xx) "unknown bank")))
    |}]
;;

(* Field errors explain refusals, never make them: [check_local] accepts exactly what
   [create_local] accepts, and a refusal always names at least one problem. *)
let%test_unit "check_local agrees with create_local" =
  let open Quickcheck.Generator in
  let name = of_list [ "bank_code"; "account_number"; "qr_iban"; "bik"; "Bad"; "" ] in
  let value =
    of_list [ "109018"; "10901"; "1090000123456789"; " "; "CH4431999123000889012"; "x" ]
  in
  let gen =
    tuple2
      (of_list [ "kg"; "ch_qr_iban"; "xx"; "Bad" ])
      (Int.gen_incl 0 3 >>= fun n -> list_with_length n (tuple2 name value))
  in
  Quickcheck.test
    gen
    ~sexp_of:[%sexp_of: string * (string * string) list]
    ~f:(fun (scheme, fields) ->
      match D.create_local ~scheme fields, D.check_local ~scheme fields with
      | Ok a, Ok b -> [%test_result: D.t] b ~expect:a
      | Error a, Error { summary; fields } ->
        [%test_result: string]
          (Error.to_string_hum summary)
          ~expect:(Error.to_string_hum a);
        assert (not (List.is_empty fields))
      | _ -> failwith "check_local and create_local disagree")
;;
