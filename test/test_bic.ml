open! Core
open Payments_core

let%expect_test "BIC shapes" =
  List.iter
    [ "DEUTDEFF"; "deutdeff500"; "NEDSZAJJXXX"; "DEUTDEF"; "DEU1DEFF"; "DEUTDEFF5000" ]
    ~f:(fun s ->
      match Bic.of_string s with
      | Ok b ->
        print_s
          [%message
            s ~bic:(Bic.to_string b) ~country:(Bic.country b) ~branch:(Bic.branch b : string option)]
      | Error e -> print_s [%message s ~error:(e : Error.t)]);
  [%expect {|
    (DEUTDEFF (bic DEUTDEFF) (country DE) (branch ()))
    (deutdeff500 (bic DEUTDEFF500) (country DE) (branch (500)))
    (NEDSZAJJXXX (bic NEDSZAJJXXX) (country ZA) (branch ()))
    (DEUTDEF (error ("invalid BIC" (raw DEUTDEF))))
    (DEU1DEFF (error ("invalid BIC" (raw DEU1DEFF))))
    (DEUTDEFF5000 (error ("invalid BIC" (raw DEUTDEFF5000))))
    |}]
;;
