open! Core
open Payments_core

let%expect_test "Easter and TARGET holidays" =
  List.iter [ 2024; 2025; 2026; 2027 ] ~f:(fun year ->
    printf "%d easter %s\n" year (Date.to_string (Target_calendar.easter_sunday ~year)));
  List.iter
    [ "2026-04-03"; "2026-04-06"; "2026-05-01"; "2026-12-24"; "2026-12-25"; "2026-12-28"; "2026-10-17" ]
    ~f:(fun d ->
      printf "%s business=%b\n" d (Target_calendar.is_business_day (Date.of_string d)));
  printf
    "3 business days after 2026-12-23: %s\n"
    (Date.to_string (Target_calendar.add_business_days (Date.of_string "2026-12-23") 3));
  [%expect {|
    2024 easter 2024-03-31
    2025 easter 2025-04-20
    2026 easter 2026-04-05
    2027 easter 2027-03-28
    2026-04-03 business=false
    2026-04-06 business=false
    2026-05-01 business=false
    2026-12-24 business=true
    2026-12-25 business=false
    2026-12-28 business=true
    2026-10-17 business=false
    3 business days after 2026-12-23: 2026-12-29
    |}]
;;
