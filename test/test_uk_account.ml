open! Core
open Payments_core

let show r = print_s [%sexp (Or_error.map r ~f:Uk_account.to_string : string Or_error.t)]

let%expect_test "parsing" =
  show (Uk_account.of_string "04-00-04 12345678");
  show (Uk_account.of_string "040004/12345678");
  show (Uk_account.of_string "04 00 04 12345678");
  show (Uk_account.of_string "040004 345678");
  (* The ISO 13616 example GB IBAN. *)
  show (Uk_account.of_string "GB29 NWBK 6016 1331 9268 19");
  show (Uk_account.of_string "DE89 3704 0044 0532 0130 00");
  show (Uk_account.of_string "04-00-4 12345678");
  show (Uk_account.of_string "04-00-04");
  [%expect
    {|
    (Ok "04-00-04 12345678")
    (Ok "04-00-04 12345678")
    (Ok "04-00-04 12345678")
    (Ok "04-00-04 00345678")
    (Ok "60-16-13 31926819")
    (Error ("not a GB IBAN" (iban DE89370400440532013000)))
    (Error ("sort code must be six digits" (raw 04-00-4)))
    (Error
     ("expected a GB IBAN or a sort code and account number" (raw 04-00-04)))
    |}]
;;

let sort_code = Sort_code.of_string_exn "08-99-99"

let%expect_test "roll numbers" =
  show
    (Uk_account.create
       ~sort_code
       ~account_number:"66374958"
       ~roll_number:" A/123-45.6 "
       ());
  show (Uk_account.create ~sort_code ~account_number:"66374958" ~roll_number:"" ());
  show
    (Uk_account.create
       ~sort_code
       ~account_number:"66374958"
       ~roll_number:"1234567890123456789"
       ());
  show (Uk_account.create ~sort_code ~account_number:"66374958" ~roll_number:"no#hash" ());
  [%expect
    {|
    (Ok "08-99-99 66374958 roll A/123-45.6")
    (Error
     ("roll number must be 1 to 18 letters, digits, spaces or / - ." (raw "")))
    (Error
     ("roll number must be 1 to 18 letters, digits, spaces or / - ."
      (raw 1234567890123456789)))
    (Error
     ("roll number must be 1 to 18 letters, digits, spaces or / - ."
      (raw no#hash)))
    |}]
;;

let%expect_test "modulus check on create" =
  let modulus =
    Uk_modulus.Table.of_string "089000 089999 MOD10 0 0 0 0 0 0 7 1 3 7 1 3 7 1" |> ok_exn
  in
  show (Uk_account.create ~modulus ~sort_code ~account_number:"66374958" ());
  show (Uk_account.create ~modulus ~sort_code ~account_number:"66374959" ());
  show (Uk_account.of_string ~modulus "08-99-99 66374959");
  (* A sort code outside the table cannot be checked and passes. *)
  show (Uk_account.of_string ~modulus "04-00-04 66374959");
  [%expect
    {|
    (Ok "08-99-99 66374958")
    (Error
     ("account number fails the modulus check for its sort code"
      (sort_code 08-99-99) (account_number 66374959)))
    (Error
     ("account number fails the modulus check for its sort code"
      (sort_code 08-99-99) (account_number 66374959)))
    (Ok "04-00-04 66374959")
    |}]
;;

let%test_unit "bin_prot round trip" =
  Quickcheck.test Uk_account.quickcheck_generator ~f:(fun t ->
    let s = Bin_prot.Writer.to_string Uk_account.Stable.V1.bin_writer_t t in
    [%test_result: Uk_account.t]
      (Bin_prot.Reader.of_string Uk_account.Stable.V1.bin_reader_t s)
      ~expect:t;
    [%test_result: Uk_account.t] (Uk_account.t_of_sexp (Uk_account.sexp_of_t t)) ~expect:t)
;;
