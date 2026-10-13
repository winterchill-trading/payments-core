open! Core
open Payments_core

let now = Time_ns.of_string_with_utc_offset "2026-10-13 09:00:00Z"

let create ?bic ~rail raw =
  Beneficiary.create
    ~id:(Beneficiary.Id.of_string "b-1")
    ~owner:"c-1"
    ~name:"Ada Example"
    ~rail
    ?bic
    ~now
    raw
;;

let show ?bic ~rail raw =
  match create ?bic ~rail raw with
  | Ok b ->
    print_s
      [%message
        (b.destination : Beneficiary.Destination.t)
          ~rails:(Beneficiary.Destination.rails b.destination : Rail.t list)]
  | Error e -> print_s [%message "rejected" ~_:(e : Error.t)]
;;

let%expect_test "destinations per rail" =
  show ~rail:Sepa_instant ~bic:"COBADEFFXXX" "DE89 3704 0044 0532 0130 00";
  show ~rail:Sepa_credit "SA03 8000 0000 6080 1016 7519";
  show ~rail:(Onchain Ethereum) "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed";
  show ~rail:(Onchain Bitcoin) "0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed";
  show ~rail:Lightning "satoshi@example.com";
  show ~rail:Intraledger "acct-42";
  show ~rail:Card_scheme "anything";
  [%expect {|
    ((b.destination
      (Bank_account (iban DE89370400440532013000) (bic (COBADEFFXXX))))
     (rails (Sepa_credit Sepa_instant)))
    (rejected ("IBAN country is outside SEPA" SA))
    ((b.destination
      (Crypto
       (address
        ((network Ethereum) (address 0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed)))
       (memo ())))
     (rails ((Onchain Ethereum))))
    (rejected
     ("invalid address" (network Bitcoin)
      0x5aaeb6053f3e94c9b9a09f33669435e7ef1beaed "bad base58check"))
    ((b.destination (Lightning (Address (user satoshi) (domain example.com))))
     (rails (Lightning)))
    ((b.destination (Internal (account acct-42))) (rails (Intraledger)))
    (rejected "card payments have no beneficiary")
    |}]
;;

let%expect_test "trust and cooling off" =
  let b = Or_error.ok_exn (create ~rail:Sepa_credit "DE89370400440532013000") in
  let cooling_off = Time_ns.Span.of_hr 24. in
  let later h = Time_ns.add now (Time_ns.Span.of_hr h) in
  print_s
    [%message
      (b.trusted : bool)
        ((Beneficiary.trust b).trusted : bool)
        (Beneficiary.is_new b ~now:(later 1.) ~cooling_off : bool)
        (Beneficiary.is_new b ~now:(later 24.) ~cooling_off : bool)
        (Beneficiary.usable_on b Sepa_instant : bool)
        (Beneficiary.usable_on b Lightning : bool)];
  [%expect {|
    ((b.trusted false) ("(Beneficiary.trust b).trusted" true)
     ("Beneficiary.is_new b ~now:(later 1.) ~cooling_off" true)
     ("Beneficiary.is_new b ~now:(later 24.) ~cooling_off" false)
     ("Beneficiary.usable_on b Sepa_instant" true)
     ("Beneficiary.usable_on b Lightning" false))
    |}]
;;
