open! Core
open Payments_core

(* The lines of the published weight table (version 8.50) that cover the specification's
   test cases, in file order, and the one substitution they need. The full files are not
   bundled; see the [Uk_modulus] documentation. *)
let weights =
  {|
070116 070116 MOD11 0 0 7 6 5 8 9 4 5 6 7 8 9 -1 12
070116 070116 MOD10 0 3 2 4 5 8 9 4 5 6 7 8 9 -1 13
074456 074456 MOD11 0 0 7 6 5 8 9 4 5 6 7 8 9 -1 12
074456 074456 MOD10 0 3 2 4 5 8 9 4 5 6 7 8 9 -1 13
086090 086090 MOD10 0 0 3 7 1 3 7 1 3 7 1 3 7 1 8
089000 089999 MOD10 0 0 0 0 0 0 7 1 3 7 1 3 7 1
090126 090129 MOD10 0 0 3 7 1 3 7 1 3 7 1 3 7 1
102400 107999 MOD11 0 0 0 0 0 0 8 7 6 5 4 3 2 1
110000 119280 DBLAL 0 0 2 1 2 1 2 1 2 1 2 1 2 1 1
134012 134020 MOD11 0 0 0 7 5 9 8 4 6 3 5 2 0 0 4
180002 180002 MOD11 0 0 0 0 0 0 8 7 6 5 4 3 2 1 14
200901 201159 MOD11 0 0 0 0 0 0 0 7 6 5 4 3 2 1 6
200901 201159 DBLAL 2 1 2 1 2 1 2 1 2 1 2 1 2 1 6
202700 203239 MOD11 0 0 0 0 0 0 0 7 6 5 4 3 2 1 6
202700 203239 DBLAL 2 1 2 1 2 1 2 1 2 1 2 1 2 1 6
309001 309633 MOD11 0 0 3 2 9 8 5 7 6 5 4 3 2 1 2
309001 309633 MOD11 0 0 3 2 9 8 1 7 6 5 4 3 2 1 9
309634 309634 MOD11 0 0 3 2 9 8 1 7 6 5 4 3 2 1
771900 772799 MOD11 0 0 1 2 5 3 6 4 8 7 10 9 3 1 7
820000 826097 MOD11 0 0 0 0 0 0 0 0 7 3 4 9 2 1
820000 826097 DBLAL 2 1 2 1 2 1 2 1 2 1 2 1 2 1 3
826919 827999 MOD11 0 0 0 0 0 0 0 0 7 3 4 9 2 1
826919 827999 DBLAL 2 1 2 1 2 1 2 1 2 1 2 1 2 1 3
870000 872791 MOD11 0 0 1 2 5 3 6 4 8 7 10 9 3 1 10
870000 872791 MOD11 0 0 5 10 9 8 0 7 6 5 4 3 2 1 11
938000 938696 MOD11 7 6 5 4 3 2 7 6 5 4 3 2 0 0 5
938000 938696 DBLAL 2 1 2 1 2 1 2 1 2 1 2 1 2 0 5
|}
;;

let table = Uk_modulus.Table.of_string ~substitutions:"938600 938611\n" weights |> ok_exn

(* The specification's own test cases (chapter 3), numbered as there. *)
let cases =
  [ 1, "089999", "66374958"
  ; 2, "107999", "88837491"
  ; 3, "202959", "63748472"
  ; 4, "871427", "46238510"
  ; 5, "872427", "46238510"
  ; 6, "871427", "09123496"
  ; 7, "871427", "99123496"
  ; 8, "820000", "73688637"
  ; 9, "827999", "73988638"
  ; 10, "827101", "28748352"
  ; 11, "134020", "63849203"
  ; 12, "118765", "64371389"
  ; 13, "200915", "41011166"
  ; 14, "938611", "07806039"
  ; 15, "938600", "42368003"
  ; 16, "938063", "55065200"
  ; 17, "772798", "99345694"
  ; 18, "086090", "06774744"
  ; 19, "309070", "02355688"
  ; 20, "309070", "12345668"
  ; 21, "309070", "12345677"
  ; 22, "309070", "99345694"
  ; 23, "938063", "15764273"
  ; 24, "938063", "15764264"
  ; 25, "938063", "15763217"
  ; 26, "118765", "64371388"
  ; 27, "203099", "66831036"
  ; 28, "203099", "58716970"
  ; 29, "089999", "66374959"
  ; 30, "107999", "88837493"
  ; 31, "074456", "12345112"
  ; 32, "070116", "34012583"
  ; 33, "074456", "11104102"
  ; 34, "180002", "00000190"
  ]
;;

let%expect_test "the specification's test cases" =
  List.iter cases ~f:(fun (n, sc, acct) ->
    let r =
      Uk_modulus.check table ~sort_code:(Sort_code.of_string_exn sc) ~account_number:acct
      |> ok_exn
    in
    printf "%2d %s %s %s\n" n sc acct (Sexp.to_string [%sexp (r : Uk_modulus.Result.t)]));
  [%expect
    {|
     1 089999 66374958 Valid
     2 107999 88837491 Valid
     3 202959 63748472 Valid
     4 871427 46238510 Valid
     5 872427 46238510 Valid
     6 871427 09123496 Valid
     7 871427 99123496 Valid
     8 820000 73688637 Valid
     9 827999 73988638 Valid
    10 827101 28748352 Valid
    11 134020 63849203 Valid
    12 118765 64371389 Valid
    13 200915 41011166 Not_checked
    14 938611 07806039 Valid
    15 938600 42368003 Valid
    16 938063 55065200 Valid
    17 772798 99345694 Valid
    18 086090 06774744 Valid
    19 309070 02355688 Valid
    20 309070 12345668 Valid
    21 309070 12345677 Valid
    22 309070 99345694 Valid
    23 938063 15764273 Invalid
    24 938063 15764264 Invalid
    25 938063 15763217 Invalid
    26 118765 64371388 Invalid
    27 203099 66831036 Invalid
    28 203099 58716970 Invalid
    29 089999 66374959 Invalid
    30 107999 88837493 Invalid
    31 074456 12345112 Valid
    32 070116 34012583 Valid
    33 074456 11104102 Valid
    34 180002 00000190 Valid
    |}]
;;

let%expect_test "unknown sort codes and account number lengths" =
  let check sc acct =
    print_s
      [%sexp
        (Uk_modulus.check
           table
           ~sort_code:(Sort_code.of_string_exn sc)
           ~account_number:acct
         : Uk_modulus.Result.t Or_error.t)]
  in
  check "000000" "12345678";
  check "089999" "374958";
  check "089999" "123456789";
  check "089999" "1234567a";
  [%expect
    {|
    (Ok Not_checked)
    (Ok Invalid)
    (Error
     ("account number must have six to eight digits; nine and ten digit numbers need the bank's own conversion"
      (digits 9)))
    (Error ("account number must be digits" (raw 1234567a)))
    |}]
;;

let%expect_test "parsing" =
  print_s
    [%sexp
      (Uk_modulus.Rule.of_string "089000 089999 MOD10 0 0 0 0 0 0 7 1 3 7 1 3 7 1"
       : Uk_modulus.Rule.t Or_error.t)];
  print_s
    [%sexp
      (Uk_modulus.Rule.of_string "089000 089999 MOD12 0" : Uk_modulus.Rule.t Or_error.t)];
  print_s
    [%sexp
      (List.length (Uk_modulus.Table.rules table (Sort_code.of_string_exn "30-90-70"))
       : int)];
  [%expect
    {|
    (Ok
     ((first 089000) (last 089999) (method_ Mod10)
      (weights (0 0 0 0 0 0 7 1 3 7 1 3 7 1)) (exception_ ())))
    (Error ("unknown modulus method" MOD12))
    2
    |}]
;;
