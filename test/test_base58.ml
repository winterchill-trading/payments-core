open! Core
open Payments_core

let hex s = String.concat_map s ~f:(fun c -> sprintf "%02x" (Char.to_int c))

let%expect_test "known addresses decode with a valid checksum" =
  List.iter
    [ "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
    ; "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy"
    ; "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNb"
    ]
    ~f:(fun s ->
      printf "%s %s\n" s (Option.value_map (Base58.decode_check s) ~default:"invalid" ~f:hex));
  [%expect {|
    1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa 0062e907b15cbf27d5425399ebf6f0fb50ebb88f18
    3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy 05b472a266d0bd89c13706a4132ccfb16f7c3b9fcb
    1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNb invalid
    |}]
;;

let%expect_test "round trips, including leading zero bytes" =
  Quickcheck.test String.quickcheck_generator ~sexp_of:String.sexp_of_t ~f:(fun s ->
    [%test_result: string option] (Base58.decode (Base58.encode s)) ~expect:(Some s);
    [%test_result: string option]
      (Base58.decode_check (Base58.encode_check s))
      ~expect:(Some s));
  print_endline (Base58.encode "\000\000\001");
  [%expect {| 112 |}]
;;
