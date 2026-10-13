open! Core
open Payments_core

let show s =
  match Iban.of_string s with
  | Ok iban ->
    printf
      "%s -> %s sepa=%b\n"
      s
      (Iban.to_string_grouped iban)
      (Iban.is_sepa iban)
  | Error e -> printf "%s -> %s\n" s (Error.to_string_hum e)
;;

let%expect_test "valid and invalid IBANs" =
  List.iter
    ~f:show
    [ "DE89 3704 0044 0532 0130 00"
    ; "gb82west12345698765432"
    ; "NO9386011117947"
    ; "CH9300762011623852957"
    ; "DE89 3704 0044 0532 0130 01"
    ; "DE89 3704 0044 0532 0130"
    ; "ZZ12 3456"
    ; "DE89-3704"
    ];
  [%expect {|
    DE89 3704 0044 0532 0130 00 -> DE89 3704 0044 0532 0130 00 sepa=true
    gb82west12345698765432 -> GB82 WEST 1234 5698 7654 32 sepa=true
    NO9386011117947 -> NO93 8601 1117 947 sepa=true
    CH9300762011623852957 -> CH93 0076 2011 6238 5295 7 sepa=true
    DE89 3704 0044 0532 0130 01 -> ("IBAN check digits do not match" (raw "DE89 3704 0044 0532 0130 01"))
    DE89 3704 0044 0532 0130 -> ("IBAN length" DE (expected 22) (got 20))
    ZZ12 3456 -> ("IBAN country not supported" ZZ)
    DE89-3704 -> ("IBAN must be alphanumeric" (raw DE89-3704))
    |}]
;;

let%expect_test "generated IBANs validate and round trip through sexp" =
  Quickcheck.test Iban.quickcheck_generator ~sexp_of:Iban.sexp_of_t ~f:(fun iban ->
    let s = Iban.to_string iban in
    [%test_result: Iban.t] (Iban.of_string_exn (Iban.to_string_grouped iban)) ~expect:iban;
    [%test_result: Iban.t] (Iban.t_of_sexp (Iban.sexp_of_t iban)) ~expect:iban;
    [%test_eq: int]
      (String.length s)
      (Option.value_exn (Iban.length_for_country (Iban.country iban))))
;;

let%expect_test "a single changed digit is always caught" =
  Quickcheck.test
    (Quickcheck.Generator.both Iban.quickcheck_generator Int.quickcheck_generator)
    ~sexp_of:[%sexp_of: Iban.t * int]
    ~f:(fun (iban, k) ->
      let s = Iban.to_string iban in
      let pos = 4 + (abs k % (String.length s - 4)) in
      let c = s.[pos] in
      let c' = Char.of_int_exn (((Char.get_digit_exn c + 1) % 10) + Char.to_int '0') in
      let mutated = String.mapi s ~f:(fun i x -> if i = pos then c' else x) in
      assert (Result.is_error (Iban.of_string mutated)))
;;
