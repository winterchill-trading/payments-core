open! Core
open Payments_core

let check network raw =
  match Crypto_address.create network raw with
  | Ok a ->
    printf "ok   %-16s %s\n" (Network.to_string network) (Crypto_address.address a)
  | Error e ->
    printf "FAIL %-16s %s\n" (Network.to_string network) (Error.to_string_hum e)
;;

let%expect_test "bitcoin" =
  List.iter
    [ Network.Bitcoin, "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
    ; Bitcoin, "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy"
    ; Bitcoin, "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4"
    ; Bitcoin, "bc1prp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qj0fj5d"
    ; Bitcoin, "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
    ; Bitcoin_testnet, "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
    ; Bitcoin_testnet, "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
    ]
    ~f:(fun (n, a) -> check n a);
  [%expect
    {|
    ok   bitcoin          1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa
    ok   bitcoin          3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy
    ok   bitcoin          bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4
    ok   bitcoin          bc1prp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qj0fj5d
    FAIL bitcoin          ("invalid address" (network Bitcoin)
     tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7
     "bad base58check")
    ok   bitcoin_testnet  tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7
    FAIL bitcoin_testnet  ("invalid address" (network Bitcoin_testnet)
     1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa "wrong version byte or length")
    |}]
;;

let%expect_test "evm, tron, solana" =
  List.iter
    [ Network.Ethereum, "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
    ; Ethereum, "0xfb6916095ca1df60bb79ce92ce3ea74c37c5d359"
    ; Polygon, "0xD1220A0CF47C7B9BE7A2E6BA89F429762E7B9ADB"
    ; Ethereum, "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD"
    ; Ethereum, "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAe"
    ; Tron, "TR7NHqjeKQxGTCi8q8ZY4pL8otSzgjLj6t"
    ; Tron, "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
    ; Solana, "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"
    ; Solana, "11111111111111111111111111111111"
    ; Solana, "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
    ]
    ~f:(fun (n, a) -> check n a);
  [%expect
    {|
    ok   ethereum         0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed
    ok   ethereum         0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359
    ok   polygon          0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb
    FAIL ethereum         ("invalid address" (network Ethereum)
     0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD "EIP-55 checksum mismatch")
    FAIL ethereum         ("invalid address" (network Ethereum)
     0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAe "expected 0x and 40 hex digits")
    ok   tron             TR7NHqjeKQxGTCi8q8ZY4pL8otSzgjLj6t
    FAIL tron             ("invalid address" (network Tron) 1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa
     "expected base58check of 0x41 and 20 bytes")
    ok   solana           TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA
    ok   solana           11111111111111111111111111111111
    FAIL solana           ("invalid address" (network Solana)
     0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed "expected base58 of 32 bytes")
    |}]
;;

let%expect_test "EIP-55 vectors from the EIP" =
  List.iter
    [ "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
    ; "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359"
    ; "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB"
    ; "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb"
    ]
    ~f:(fun a ->
      [%test_result: string option]
        (Crypto_address.eip55 (String.lowercase a))
        ~expect:(Some a));
  [%expect {| |}]
;;

let%expect_test "spellings of one address are equal" =
  let a =
    Crypto_address.create_exn Ethereum "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed"
  in
  let b =
    Crypto_address.create_exn Ethereum "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
  in
  print_s [%message (Crypto_address.equal a b : bool)];
  [%expect {| ("Crypto_address.equal a b" true) |}]
;;
