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
  [%expect
    {|
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
  [%expect
    {|
    ((b.trusted false) ("(Beneficiary.trust b).trusted" true)
     ("Beneficiary.is_new b ~now:(later 1.) ~cooling_off" true)
     ("Beneficiary.is_new b ~now:(later 24.) ~cooling_off" false)
     ("Beneficiary.usable_on b Sepa_instant" true)
     ("Beneficiary.usable_on b Lightning" false))
    |}]
;;

let%expect_test "UK destinations" =
  show ~rail:Faster_payments "04-00-04 12345678";
  show ~rail:Chaps "GB29 NWBK 6016 1331 9268 19";
  show ~rail:Bacs "04-00-04";
  (* A GB IBAN saved for SEPA reaches the UK rails too. *)
  show ~rail:Sepa_credit "GB29 NWBK 6016 1331 9268 19";
  (match
     Beneficiary.create
       ~id:(Beneficiary.Id.of_string "b-2")
       ~owner:"c-1"
       ~name:"Building Society Saver"
       ~rail:Faster_payments
       ~roll_number:"A/123456-7"
       ~now
       "04-00-04 12345678"
   with
   | Ok b ->
     print_s
       [%sexp
         (Beneficiary.Destination.uk_account b.destination
          |> Option.map ~f:Uk_account.to_string
          : string option)]
   | Error e -> print_s [%sexp (e : Error.t)]);
  [%expect
    {|
    ((b.destination
      (Uk_account
       ((sort_code 040004) (account_number 12345678) (roll_number ()))))
     (rails (Faster_payments Chaps Bacs)))
    ((b.destination
      (Uk_account
       ((sort_code 601613) (account_number 31926819) (roll_number ()))))
     (rails (Faster_payments Chaps Bacs)))
    (rejected
     ("expected a GB IBAN or a sort code and account number" (raw 04-00-04)))
    ((b.destination (Bank_account (iban GB29NWBK60161331926819) (bic ())))
     (rails (Sepa_credit Sepa_instant Faster_payments Chaps Bacs)))
    ("04-00-04 12345678 roll A/123456-7")
    |}]
;;

let%expect_test "payee checks through the V1 format" =
  List.iter
    [ Beneficiary.Payee_check.Match
    ; Close_match { suggested_name = "Ada Exampel" }
    ; No_match
    ; Not_possible
    ; Account_not_found
    ; Unavailable
    ]
    ~f:(fun check ->
      let via version writer reader =
        Bin_prot.Reader.of_string reader (Bin_prot.Writer.to_string writer check)
        |> fun back -> version, back
      in
      print_s
        [%sexp
          (check : Beneficiary.Payee_check.t)
          , (via
               "v1"
               Beneficiary.Payee_check.Stable.V1.bin_writer_t
               Beneficiary.Payee_check.Stable.V1.bin_reader_t
             : string * Beneficiary.Payee_check.t)
          , (via
               "v2"
               Beneficiary.Payee_check.Stable.V2.bin_writer_t
               Beneficiary.Payee_check.Stable.V2.bin_reader_t
             : string * Beneficiary.Payee_check.t)]);
  [%expect
    {|
    (Match (v1 Match) (v2 Match))
    ((Close_match (suggested_name "Ada Exampel"))
     (v1 (Close_match (suggested_name "Ada Exampel")))
     (v2 (Close_match (suggested_name "Ada Exampel"))))
    (No_match (v1 No_match) (v2 No_match))
    (Not_possible (v1 Not_possible) (v2 Not_possible))
    (Account_not_found (v1 Not_possible) (v2 Account_not_found))
    (Unavailable (v1 Not_possible) (v2 Unavailable))
    |}]
;;

let local ?formats ?postal_address ~scheme fields =
  Beneficiary.create_local
    ~id:(Beneficiary.Id.of_string "b-local")
    ~owner:"c-1"
    ~name:"Aibek Toktogulov"
    ?formats
    ?postal_address
    ~now
    ~scheme
    fields
;;

let show_local ?formats ~scheme fields =
  match local ?formats ~scheme fields with
  | Ok b ->
    print_s
      [%message
        (b.destination : Beneficiary.Destination.t)
          ~rails:(Beneficiary.Destination.rails b.destination : Rail.t list)]
  | Error e -> print_s [%message "rejected" ~_:(e : Error.t)]
;;

let%expect_test "local destinations: a Kyrgyz account number and BIK" =
  let kg = Beneficiary.Destination.Local_scheme.kg in
  (* Order and spacing as a customer might type them; stored normalised. *)
  show_local
    ~scheme:kg
    [ "account_number", "1090 0001 2345 6789"; "bank_code", " 109018 " ];
  show_local ~scheme:kg [ "bank_code", "10901"; "account_number", "1090000123456789" ];
  show_local ~scheme:kg [ "bank_code", "109018"; "account_number", "109000012345678X" ];
  show_local ~scheme:kg [ "bank_code", "109018" ];
  show_local ~scheme:kg [ "bik", "109018"; "account_number", "1090000123456789" ];
  [%expect
    {|
    ((b.destination
      (Local (scheme kg)
       (fields ((bank_code 109018) (account_number 1090000123456789)))))
     (rails ()))
    (rejected
     (("local destination" kg) ("expected digits" bank_code (length 6) 10901)))
    (rejected
     (("local destination" kg)
      ("expected digits" account_number (length 16) 109000012345678X)))
    (rejected
     (("local destination" kg)
      ("unexpected fields" (expected (bank_code account_number))
       (given (bank_code)))))
    (rejected
     (("local destination" kg)
      ("unexpected fields" (expected (bank_code account_number))
       (given (account_number bik)))))
    |}]
;;

let%expect_test "local destinations: QR-IBAN, generic rules and custom hooks" =
  let qr = Beneficiary.Destination.Local_scheme.ch_qr_iban in
  show_local ~scheme:qr [ "qr_iban", "CH44 3199 9123 0008 8901 2" ];
  (* An ordinary Swiss IBAN is not a QR-IBAN. *)
  show_local ~scheme:qr [ "qr_iban", "CH93 0076 2011 6238 5295 7" ];
  (* Unknown schemes pass with the generic checks only. *)
  show_local ~scheme:"xx_bank_giro" [ "giro_number", "123-4567" ];
  show_local ~scheme:"Bad Scheme" [ "a", "1" ];
  show_local ~scheme:"xx" [];
  show_local ~scheme:"xx" [ "a", "1"; "a", "2" ];
  show_local ~scheme:"xx" [ "Account", "1" ];
  show_local ~scheme:"xx" [ "a", "  " ];
  let upper =
    Beneficiary.Destination.Local_format.create ~scheme:"xx" ~validate:(fun fields ->
      Ok (List.map fields ~f:(fun (n, v) -> n, String.uppercase v)))
  in
  show_local ~formats:[ upper ] ~scheme:"xx" [ "code", "ab12" ];
  (* Passing formats replaces the defaults: kg is then only checked generically. *)
  show_local ~formats:[ upper ] ~scheme:"kg" [ "bank_code", "1" ];
  [%expect
    {|
    ((b.destination
      (Local (scheme ch_qr_iban) (fields ((qr_iban CH4431999123000889012)))))
     (rails ()))
    (rejected
     (("local destination" ch_qr_iban) ("not a QR-IBAN" CH9300762011623852957)))
    ((b.destination
      (Local (scheme xx_bank_giro) (fields ((giro_number 123-4567)))))
     (rails ()))
    (rejected ("local scheme must be lower snake case" "Bad Scheme"))
    (rejected "no fields")
    (rejected ("duplicate field" a))
    (rejected ("field name must be lower snake case" Account))
    (rejected ("empty field" a))
    ((b.destination (Local (scheme xx) (fields ((code AB12))))) (rails ()))
    ((b.destination (Local (scheme kg) (fields ((bank_code 1))))) (rails ()))
    |}]
;;

let%expect_test "local destinations are reachable on no rail and read back by scheme" =
  let b =
    local ~scheme:"kg" [ "bank_code", "109018"; "account_number", "1090000123456789" ]
    |> Or_error.ok_exn
  in
  print_s [%sexp (Beneficiary.usable_on b Sepa_credit : bool)];
  print_s
    [%sexp
      (Beneficiary.Destination.local b.destination ~scheme:"kg"
       : (string * string) list option)];
  print_s
    [%sexp
      (Beneficiary.Destination.local b.destination ~scheme:"ch_qr_iban"
       : (string * string) list option)];
  [%expect
    {|
    false
    (((bank_code 109018) (account_number 1090000123456789)))
    ()
    |}]
;;

let address : Beneficiary.Postal_address.t =
  { country = "US"
  ; town = "New York"
  ; post_code = Some "10001"
  ; street = Some "Fifth Avenue"
  ; building_number = Some "350"
  ; lines = []
  }
;;

let%expect_test "postal addresses and rails that need them" =
  let us = create ~rail:Fedwire "021000021 123456789" |> Or_error.ok_exn in
  let check ?address_required b rail =
    print_s
      [%sexp (Beneficiary.check_for_rail ?address_required b rail : unit Or_error.t)]
  in
  check us Fedwire;
  check us Rtp;
  let with_address =
    Beneficiary.with_postal_address us (Some address) |> Or_error.ok_exn
  in
  check with_address Fedwire;
  check with_address Sepa_credit;
  check ~address_required:(fun _ -> true) us Rtp;
  print_s
    [%sexp
      (Beneficiary.with_postal_address us (Some { address with country = "usa" })
       |> Result.map ~f:ignore
       : unit Or_error.t)];
  print_s
    [%sexp
      (Beneficiary.create
         ~id:(Beneficiary.Id.of_string "b-2")
         ~owner:"c-1"
         ~name:"Ada Example"
         ~rail:Chaps
         ~postal_address:address
         ~now
         "20-00-00 55779911"
       |> Result.map ~f:(fun b -> Option.is_some b.postal_address)
       : bool Or_error.t)];
  (* The sexp shows the address only when there is one. *)
  print_s [%sexp (with_address : Beneficiary.t)];
  [%expect
    {|
    (Error ("this rail needs the beneficiary's postal address" (rail Fedwire)))
    (Ok ())
    (Ok ())
    (Error ("destination cannot be reached on this rail" (rail Sepa_credit)))
    (Error ("this rail needs the beneficiary's postal address" (rail Rtp)))
    (Error ("postal address" ("country must be ISO 3166 alpha-2" usa)))
    (Ok true)
    ((id b-1) (owner c-1) (name "Ada Example")
     (destination
      (Us_account
       ((routing 021000021) (account_number 123456789) (account_type Checking))))
     (trusted false) (first_seen "2026-10-13 09:00:00Z") (payee_check ())
     (postal_address
      ((country US) (town "New York") (post_code (10001))
       (street ("Fifth Avenue")) (building_number (350)) (lines ()))))
    |}]
;;

let%expect_test "stable versions: V3 holds everything, older ones refuse what they cannot"
  =
  let kg =
    local
      ~postal_address:address
      ~scheme:"kg"
      [ "bank_code", "109018"; "account_number", "1090000123456789" ]
    |> Or_error.ok_exn
  in
  let v3 = Bin_prot.Writer.to_string Beneficiary.Stable.V3.bin_writer_t kg in
  [%test_result: Beneficiary.t]
    (Bin_prot.Reader.of_string Beneficiary.Stable.V3.bin_reader_t v3)
    ~expect:kg;
  [%test_result: Beneficiary.t]
    (Beneficiary.Stable.V3.t_of_sexp (Beneficiary.Stable.V3.sexp_of_t kg))
    ~expect:kg;
  let fails f = print_s [%sexp (Result.is_error (Or_error.try_with f) : bool)] in
  fails (fun () ->
    ignore (Bin_prot.Writer.to_string Beneficiary.Stable.V2.bin_writer_t kg : string));
  fails (fun () ->
    ignore (Bin_prot.Writer.to_string Beneficiary.Stable.V1.bin_writer_t kg : string));
  fails (fun () -> ignore (Beneficiary.Stable.V2.sexp_of_t kg : Sexp.t));
  let no_address = { kg with postal_address = None } in
  fails (fun () ->
    ignore
      (Bin_prot.Writer.to_string Beneficiary.Stable.V2.bin_writer_t no_address : string));
  let sepa = create ~rail:Sepa_credit "DE89370400440532013000" |> Or_error.ok_exn in
  (* An old-style beneficiary writes the same bytes through V2 as before. *)
  let v2 = Bin_prot.Writer.to_string Beneficiary.Stable.V2.bin_writer_t sepa in
  [%test_result: Beneficiary.t]
    (Bin_prot.Reader.of_string Beneficiary.Stable.V2.bin_reader_t v2)
    ~expect:sepa;
  let with_address =
    Beneficiary.with_postal_address sepa (Some address) |> Or_error.ok_exn
  in
  fails (fun () ->
    ignore
      (Bin_prot.Writer.to_string Beneficiary.Stable.V1.bin_writer_t with_address : string));
  [%expect {|
    true
    true
    true
    true
    true
    |}]
;;
