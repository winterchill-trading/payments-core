open! Core

(* Anonymous Gregorian algorithm (Meeus/Jones/Butcher). *)
let easter_sunday ~year =
  let a = year % 19 in
  let b = year / 100 in
  let c = year % 100 in
  let d = b / 4 in
  let e = b % 4 in
  let f = (b + 8) / 25 in
  let g = (b - f + 1) / 3 in
  let h = ((19 * a) + b - d - g + 15) % 30 in
  let i = c / 4 in
  let k = c % 4 in
  let l = (32 + (2 * e) + (2 * i) - h - k) % 7 in
  let m = (a + (11 * h) + (22 * l)) / 451 in
  let month = (h + l - (7 * m) + 114) / 31 in
  let day = ((h + l - (7 * m) + 114) % 31) + 1 in
  Date.create_exn ~y:year ~m:(Month.of_int_exn month) ~d:day
;;

let is_holiday date =
  let year = Date.year date in
  let easter = easter_sunday ~year in
  let fixed =
    match Date.month date, Date.day date with
    | Jan, 1 | May, 1 | Dec, 25 | Dec, 26 -> true
    | _ -> false
  in
  fixed
  || Date.equal date (Date.add_days easter (-2))
  || Date.equal date (Date.add_days easter 1)
;;

let is_business_day date = Date.is_weekday date && not (is_holiday date)

let add_business_days date n =
  let rec next d = if is_business_day d then d else next (Date.add_days d 1) in
  let rec go d n = if n = 0 then d else go (next (Date.add_days d 1)) (n - 1) in
  go (next date) n
;;
