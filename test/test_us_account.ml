open! Core
open Payments_core

let%expect_test "routing numbers" =
  List.iter
    [ "021000021" (* a widely published New York routing number *)
    ; "011000015" (* the Federal Reserve Bank of Boston *)
    ; "0210-0002-1"
    ; "021000022"
    ; "02100002"
    ; "02100002x"
    ]
    ~f:(fun raw -> print_s [%sexp (raw : string), (Aba.of_string raw : Aba.t Or_error.t)]);
  print_s [%sexp (Aba.check_digit "02100002" : int Or_error.t)];
  [%expect
    {|
    (021000021 (Ok 021000021))
    (011000015 (Ok 011000015))
    (0210-0002-1 (Ok 021000021))
    (021000022
     (Error ("routing number check digit does not match" (raw 021000022))))
    (02100002 (Error ("routing number must be nine digits" (raw 02100002))))
    (02100002x (Error ("routing number must be nine digits" (raw 02100002x))))
    (Ok 1)
    |}]
;;

let%test_unit "generated routing numbers pass the check" =
  Quickcheck.test Aba.quickcheck_generator ~f:(fun aba ->
    [%test_result: Aba.t] (Aba.of_string (Aba.to_string aba) |> ok_exn) ~expect:aba)
;;

let%expect_test "accounts" =
  let show raw =
    print_s
      [%sexp
        (raw : string)
        , (Us_account.of_string raw |> Or_error.map ~f:Us_account.to_string
           : string Or_error.t)]
  in
  show "021000021 123456789";
  show "021000021/123456789/savings";
  show "021000021 123";
  show "021000021 123456789012345678";
  show "021000021 123456789 brokerage";
  show "021000022 123456789";
  [%expect
    {|
    ("021000021 123456789" (Ok "021000021 123456789 checking"))
    (021000021/123456789/savings (Ok "021000021 123456789 savings"))
    ("021000021 123"
     (Error ("US account number must have 4 to 17 digits" (account_number 123))))
    ("021000021 123456789012345678"
     (Error
      ("US account number must have 4 to 17 digits"
       (account_number 123456789012345678))))
    ("021000021 123456789 brokerage"
     (Error ("account type is checking or savings" brokerage)))
    ("021000022 123456789"
     (Error ("routing number check digit does not match" (raw 021000022))))
    |}]
;;

let%test_unit "bin_prot and sexp round trip" =
  Quickcheck.test Us_account.quickcheck_generator ~f:(fun t ->
    let s = Bin_prot.Writer.to_string Us_account.Stable.V1.bin_writer_t t in
    [%test_result: Us_account.t]
      (Bin_prot.Reader.of_string Us_account.Stable.V1.bin_reader_t s)
      ~expect:t;
    [%test_result: Us_account.t] (Us_account.t_of_sexp (Us_account.sexp_of_t t)) ~expect:t)
;;
