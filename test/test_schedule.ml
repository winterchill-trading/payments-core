open! Core
open Payments_core

let d = Date.of_string

let%expect_test "next run dates" =
  let show t from =
    let rec go date n acc =
      if n = 0 then List.rev acc else go (Schedule.next_after t date) (n - 1) (date :: acc)
    in
    let first = Schedule.next_on_or_after t (d from) in
    print_s [%message (t : Schedule.t) from ~runs:(go first 5 [] : Date.t list)]
  in
  show Daily "2026-12-30";
  show (Weekly Fri) "2026-10-13";
  show (Weekly Tue) "2026-10-13";
  show (Monthly { day = 31 }) "2026-10-13";
  show (Monthly { day = 29 }) "2027-01-30";
  show (Monthly { day = 13 }) "2026-10-13";
  [%expect {|
    ((t Daily) 2026-12-30
     (runs (2026-12-30 2026-12-31 2027-01-01 2027-01-02 2027-01-03)))
    ((t (Weekly FRI)) 2026-10-13
     (runs (2026-10-16 2026-10-23 2026-10-30 2026-11-06 2026-11-13)))
    ((t (Weekly TUE)) 2026-10-13
     (runs (2026-10-13 2026-10-20 2026-10-27 2026-11-03 2026-11-10)))
    ((t (Monthly (day 31))) 2026-10-13
     (runs (2026-10-31 2026-11-30 2026-12-31 2027-01-31 2027-02-28)))
    ((t (Monthly (day 29))) 2027-01-30
     (runs (2027-02-28 2027-03-29 2027-04-29 2027-05-29 2027-06-29)))
    ((t (Monthly (day 13))) 2026-10-13
     (runs (2026-10-13 2026-11-13 2026-12-13 2027-01-13 2027-02-13)))
    |}]
;;

let%expect_test "next_after is strictly after and on schedule" =
  let gen =
    Quickcheck.Generator.both
      (Quickcheck.Generator.union
         [ Quickcheck.Generator.return Schedule.Daily
         ; Quickcheck.Generator.map Day_of_week.quickcheck_generator ~f:(fun d -> Schedule.Weekly d)
         ; Quickcheck.Generator.map (Int.gen_incl 1 31) ~f:(fun day -> Schedule.Monthly { day })
         ])
      (Date.gen_incl (d "2000-01-01") (d "2100-12-31"))
  in
  Quickcheck.test gen ~sexp_of:[%sexp_of: Schedule.t * Date.t] ~f:(fun (t, date) ->
    let next = Schedule.next_after t date in
    assert (Date.( > ) next date);
    (* nothing on schedule in between *)
    [%test_result: Date.t] (Schedule.next_on_or_after t (Date.add_days date 1)) ~expect:next;
    assert (Date.diff next date <= 31))
;;
