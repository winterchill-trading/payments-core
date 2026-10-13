open! Core
open Payments_core

let show s =
  match Lightning.Target.of_string s with
  | Ok (Invoice i) ->
    print_s
      [%message
        "invoice"
          ~network:(i.network : Network.t)
          ~amount_msat:(i.amount_msat : int option)
          ~timestamp:(i.timestamp : Time_ns.Alternate_sexp.t)
          ~expiry:(i.expiry : Time_ns.Span.t)
          ~payment_hash:i.payment_hash
          ~description:(i.description : string option)]
  | Ok t ->
    print_s
      [%message
        (t : Lightning.Target.t) ~pay_url:(Lightning.Target.pay_url t : string option)]
  | Error e -> print_s [%message "error" ~_:(e : Error.t)]
;;

(* Synthetic invoices built with the BIP 173 reference encoder; the 65-byte signature is
   zeros, which this library does not check (see the module docs). *)
let testnet_invoice =
  "lntb10u1p5ww7qqpp5qqqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0sdq2vdhkven9v5xqzjcqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqfujy6r"
;;

let any_amount_invoice =
  "lnbc1p5ww7qqpp5qqqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0sqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqw9wrdn"
;;

let%expect_test "invoices" =
  show testnet_invoice;
  show ("lightning:" ^ String.uppercase any_amount_invoice);
  (* BOLT 11 example: 2500u, "1 cup coffee", 60 s expiry *)
  show
    "lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpuaztrnwngzn3kdzw5hydlzf03qdgm2hdq27cqv3agm2awhz5se903vruatfhq77w3ls4evs3ch9zw97j25emudupq63nyw24cg27h2rspfj9srp";
  show (String.drop_suffix testnet_invoice 1 ^ "q");
  [%expect
    {|
    (invoice (network Bitcoin_testnet) (amount_msat (1000000))
     (timestamp "2025-10-09 08:53:20Z") (expiry 10m)
     (payment_hash
      000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f)
     (description (coffee)))
    (invoice (network Bitcoin) (amount_msat ())
     (timestamp "2025-10-09 08:53:20Z") (expiry 1h)
     (payment_hash
      000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f)
     (description ()))
    (invoice (network Bitcoin) (amount_msat (250000000))
     (timestamp "2017-06-01 10:57:38Z") (expiry 1m)
     (payment_hash
      0001020304050607080900010203040506070809000102030405060708090102)
     (description ("1 cup coffee")))
    (error "invalid bech32 invoice")
    |}]
;;

let%expect_test "expiry" =
  let i = Or_error.ok_exn (Lightning.Invoice.of_string testnet_invoice) in
  let at sec = Time_ns.add i.timestamp (Time_ns.Span.of_int_sec sec) in
  print_s
    [%message
      (Lightning.Invoice.is_expired i ~now:(at 599) : bool)
        (Lightning.Invoice.is_expired i ~now:(at 600) : bool)];
  [%expect
    {|
    (("Lightning.Invoice.is_expired i ~now:(at 599)" false)
     ("Lightning.Invoice.is_expired i ~now:(at 600)" true))
    |}]
;;

let%expect_test "lnurl and addresses" =
  List.iter
    ~f:show
    [ "lnurl1dp68gurn8ghj7um9wfmxjcm99ejhsctdwpkx2tmvde6hympdwpshj0m5v9nn67qvle2vp"
    ; "LIGHTNING:LNURL1DP68GURN8GHJ7UM9WFMXJCM99EJHSCTDWPKX2TMVDE6HYMPDWPSHJ0M5V9NN67QVLE2VP"
    ; "satoshi@Example.com"
    ; "bad user@example.com"
    ; "user@localhost"
    ];
  [%expect
    {|
    ((t (Lnurl https://service.example/lnurl-pay?tag=x))
     (pay_url (https://service.example/lnurl-pay?tag=x)))
    ((t (Lnurl https://service.example/lnurl-pay?tag=x))
     (pay_url (https://service.example/lnurl-pay?tag=x)))
    ((t (Address (user satoshi) (domain example.com)))
     (pay_url (https://example.com/.well-known/lnurlp/satoshi)))
    (error ("invalid Lightning address" "bad user@example.com"))
    (error ("invalid Lightning address" user@localhost))
    |}]
;;
