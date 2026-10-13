open! Core
open Payments_core
open Helpers

let person ?identification name account : Travel_rule.Person.t =
  { name; account; identification; lei = None }
;;

let rule ?(identification = Some (Travel_rule.Identification.Customer_number "c-1")) wallet
  : Travel_rule.t
  =
  { originator = person ?identification "Ada Example" "customer:1"
  ; beneficiary = person "Bob Example" "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
  ; counterparty_wallet = wallet
  }
;;

let show r ~eur_value = print_s [%sexp (Travel_rule.check r ~eur_value : unit Or_error.t)]

let%expect_test "travel rule checks" =
  show (rule (Hosted { provider_name = "Example Exchange"; provider_lei = None })) ~eur_value:None;
  show (rule ~identification:None (Hosted { provider_name = "Example Exchange"; provider_lei = None })) ~eur_value:None;
  show (rule (Self_hosted { ownership_verified = false })) ~eur_value:(Some (eur "1000.00"));
  show (rule (Self_hosted { ownership_verified = false })) ~eur_value:(Some (eur "1000.01"));
  show (rule (Self_hosted { ownership_verified = false })) ~eur_value:None;
  show (rule (Self_hosted { ownership_verified = true })) ~eur_value:(Some (eur "5000.00"));
  [%expect {|
    (Ok ())
    (Error
     "originator needs an address, official document, customer number or date and place of birth")
    (Ok ())
    (Error
     ("self-hosted wallet ownership must be verified above the threshold"
      (eur_value ("1000.01 EUR"))))
    (Error
     ("self-hosted wallet ownership must be verified above the threshold"
      (eur_value ())))
    (Ok ())
    |}]
;;

let%expect_test "LEI check digits" =
  List.iter [ "5493001KJTIIGC8Y1R12"; "5493001KJTIIGC8Y1R13"; "TOO-SHORT" ] ~f:(fun s ->
    print_s [%sexp (Travel_rule.Lei.of_string s : Travel_rule.Lei.t Or_error.t)]);
  [%expect {|
    (Ok 5493001KJTIIGC8Y1R12)
    (Error ("invalid LEI" 5493001KJTIIGC8Y1R13))
    (Error ("invalid LEI" TOO-SHORT))
    |}]
;;
