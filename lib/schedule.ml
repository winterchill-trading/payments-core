open! Core

type t =
  | Daily
  | Weekly of Day_of_week.Stable.V1.t
  | Monthly of { day : int }
[@@deriving bin_io, sexp, compare, equal, hash]

let validate = function
  | Daily | Weekly _ -> Ok ()
  | Monthly { day } ->
    if day >= 1 && day <= 31
    then Ok ()
    else Or_error.error_s [%message "monthly day must be 1..31" (day : int)]
;;

(* A monthly order on the 31st runs on the last day of 30-day months and of February,
   which is what customers expect from "end of month" payments. *)
let in_month ~day ~year ~month =
  let last = Date.days_in_month ~year ~month in
  Date.create_exn ~y:year ~m:month ~d:(Int.min day last)
;;

let next_on_or_after t date =
  match t with
  | Daily -> date
  | Weekly dow ->
    let delta = (Day_of_week.to_int dow - Day_of_week.to_int (Date.day_of_week date)) % 7 in
    Date.add_days date delta
  | Monthly { day } ->
    let this = in_month ~day ~year:(Date.year date) ~month:(Date.month date) in
    if Date.( >= ) this date
    then this
    else (
      let first_of_next = Date.add_months (Date.create_exn ~y:(Date.year date) ~m:(Date.month date) ~d:1) 1 in
      in_month ~day ~year:(Date.year first_of_next) ~month:(Date.month first_of_next))
;;

let next_after t date = next_on_or_after t (Date.add_days date 1)
