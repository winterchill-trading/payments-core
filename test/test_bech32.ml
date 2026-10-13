open! Core
open Payments_core

let hex s = String.concat_map s ~f:(fun c -> sprintf "%02x" (Char.to_int c))

let%expect_test "BIP 173 and BIP 350 strings" =
  List.iter
    [ "A12UEL5L"
    ; "a12uel5l"
    ; "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
    ; "A1LQFN3A" (* valid Bech32m, BIP 350 *)
    ; "A12uEL5L" (* mixed case *)
    ; "a12uel5m" (* bad checksum *)
    ]
    ~f:(fun s ->
      print_s
        [%message
          s ~_:(Option.map (Bech32.decode s) ~f:(fun d -> d.hrp, d.variant) : (string * Bech32.Variant.t) option)]);
  [%expect {|
    (A12UEL5L ((a Bech32)))
    (a12uel5l ((a Bech32)))
    (abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw ((abcdef Bech32)))
    (A1LQFN3A ((a Bech32m)))
    (A12uEL5L ())
    (a12uel5m ())
    |}]
;;

let%expect_test "SegWit addresses (vectors cross-checked with the BIP reference code)" =
  List.iter
    [ "bc", "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
    ; "bc", "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4"
    ; "tb", "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
    ; "bc", "bc1prp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qj0fj5d"
    ; "bc", "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
      (* v1 program checksummed as Bech32 instead of Bech32m: *)
    ; "bc", "bc1prp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
    ]
    ~f:(fun (hrp, s) ->
      match Bech32.decode_segwit ~hrp s with
      | Some (v, program) -> printf "%s v%d %s\n" s v (hex program)
      | None -> printf "%s invalid for %s\n" s hrp);
  [%expect {|
    bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4 v0 751e76e8199196d454941c45d1b3a323f1433bd6
    BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4 v0 751e76e8199196d454941c45d1b3a323f1433bd6
    tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7 v0 1863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262
    bc1prp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qj0fj5d v1 1863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262
    tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7 invalid for bc
    bc1prp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7 invalid for bc
    |}]
;;

let%expect_test "encode then decode round trips for both variants" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind hrp =
      String.gen_with_length 3 (Quickcheck.Generator.char_lowercase)
    in
    let%bind data = List.gen_with_length 20 (Int.gen_incl 0 31) in
    let%map variant = Quickcheck.Generator.of_list [ Bech32.Variant.Bech32; Bech32m ] in
    hrp, data, variant
  in
  Quickcheck.test gen ~sexp_of:[%sexp_of: string * int list * Bech32.Variant.t]
    ~f:(fun (hrp, data, variant) ->
      [%test_result: Bech32.decoded option]
        (Bech32.decode (Bech32.encode ~hrp ~data variant))
        ~expect:(Some { hrp; data; variant }))
;;

let%expect_test "convert_bits 8 -> 5 -> 8 is the identity" =
  Quickcheck.test (List.gen_with_length 32 (Int.gen_incl 0 255)) ~sexp_of:[%sexp_of: int list]
    ~f:(fun bytes ->
      let five = Option.value_exn (Bech32.convert_bits bytes ~from:8 ~to_:5 ~pad:true) in
      [%test_result: int list option]
        (Bech32.convert_bits five ~from:5 ~to_:8 ~pad:false)
        ~expect:(Some bytes))
;;
