open! Core

module Scheme = struct
  type t =
    | Fps
    | Chaps
    | Bacs
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]

  let of_rail : Rail.t -> t option = function
    | Faster_payments -> Some Fps
    | Chaps -> Some Chaps
    | Bacs -> Some Bacs
    | Sepa_credit
    | Sepa_instant
    | Onchain _
    | Lightning
    | Intraledger
    | Card_scheme
    | Ach _
    | Rtp
    | Fedwire -> None
  ;;

  let to_rail : t -> Rail.t = function
    | Fps -> Faster_payments
    | Chaps -> Chaps
    | Bacs -> Bacs
  ;;
end

let gbp s = Money.Amount.of_decimal_string Money.Currency.gbp s |> Or_error.ok_exn

(* Raised from GBP 250,000 to GBP 1,000,000 in February 2022. *)
let fps_scheme_max = gbp "1000000.00"

let last_sunday ~year ~month =
  let last = Date.add_days (Date.create_exn ~y:year ~m:(Month.shift month 1) ~d:1) (-1) in
  let back = Day_of_week.to_int (Date.day_of_week last) (* Sunday is 0 *) in
  Date.add_days last (-back)
;;

(* British Summer Time runs from 01:00 UTC on the last Sunday of March to 01:00 UTC on the
   last Sunday of October. Only whole days matter here (cut-offs fall on weekdays), so the
   switch is taken at the start of those Sundays. *)
let uk_offset_hours date =
  let year = Date.year date in
  if Date.( >= ) date (last_sunday ~year ~month:Mar)
     && Date.( < ) date (last_sunday ~year ~month:Oct)
  then 1
  else 0
;;

let default_chaps_cutoff date =
  if Day_of_week.is_sun_or_sat (Date.day_of_week date)
  then None
  else
    Some
      (Time_ns.of_date_ofday
         ~zone:Time_ns.Zone.utc
         date
         (Time_ns.Ofday.create ~hr:(18 - uk_offset_hours date) ()))
;;

module Rules = struct
  type t =
    { fps_max_amount : Money.Amount.t
    ; chaps_cutoff : Date.t -> Time_ns.t option
    }
  [@@deriving fields ~getters]

  let default = { fps_max_amount = fps_scheme_max; chaps_cutoff = default_chaps_cutoff }

  let create ?(fps_max_amount = fps_scheme_max) ?(chaps_cutoff = default_chaps_cutoff) () =
    let ok =
      Money.Currency.equal (Money.Amount.currency fps_max_amount) Money.Currency.gbp
      && Money.Amount.is_positive fps_max_amount
      &&
      match Money.Amount.compare_value fps_max_amount fps_scheme_max with
      | Ok c -> c <= 0
      | Error _ -> false
    in
    if ok
    then Ok { fps_max_amount; chaps_cutoff }
    else
      Or_error.error_s
        [%message
          "Faster Payments limit must be a positive GBP amount within the scheme's"
            (fps_max_amount : Money.Amount.t)
            (fps_scheme_max : Money.Amount.t)]
  ;;
end

module Details = struct
  module Stable = struct
    module V1 = struct
      type t =
        { scheme_ref : string option
        ; value_date : Date.Stable.V1.t option
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  include Stable.V1

  let empty = { scheme_ref = None; value_date = None }
end

let max_reference_length : Scheme.t -> int = function
  | Fps | Bacs -> 18
  | Chaps -> 140
;;

let reference_char_ok (scheme : Scheme.t) c =
  match scheme with
  | Bacs -> Char.is_alphanum c || List.mem [ '.'; '&'; '/'; '-'; ' ' ] c ~equal:Char.equal
  | Fps | Chaps -> Char.is_print c
;;

let validate ?(rules = Rules.default) (scheme : Scheme.t) ~amount ~reference =
  let open Or_error.Let_syntax in
  let%bind () =
    if Money.Currency.equal (Money.Amount.currency amount) Money.Currency.gbp
    then Ok ()
    else
      Or_error.error_s
        [%message "UK domestic payments are in GBP" (amount : Money.Amount.t)]
  in
  let%bind () =
    if Money.Amount.is_positive amount
    then Ok ()
    else Or_error.error_s [%message "amount must be positive" (amount : Money.Amount.t)]
  in
  let%bind () =
    match scheme with
    | Fps ->
      let max = Rules.fps_max_amount rules in
      (match Money.Amount.compare_value amount max with
       | Ok c when c > 0 ->
         Or_error.error_s
           [%message
             "over the Faster Payments limit"
               (amount : Money.Amount.t)
               (max : Money.Amount.t)]
       | _ -> Ok ())
    | Chaps | Bacs -> Ok ()
  in
  let max_len = max_reference_length scheme in
  let%bind () =
    if String.length reference <= max_len
    then Ok ()
    else
      Or_error.error_s
        [%message "reference too long" (scheme : Scheme.t) (max_len : int) reference]
  in
  match String.find reference ~f:(Fn.non (reference_char_ok scheme)) with
  | None -> Ok ()
  | Some c ->
    Or_error.error_s
      [%message "character not allowed in the reference" (scheme : Scheme.t) (c : char)]
;;

let chaps_value_date rules ~now =
  let today = Time_ns.to_date now ~zone:Time_ns.Zone.utc in
  List.init 15 ~f:(Date.add_days today)
  |> List.find ~f:(fun date ->
    match Rules.chaps_cutoff rules date with
    | Some cutoff -> Time_ns.( <= ) now cutoff
    | None -> false)
  |> Result.of_option ~error:(Error.of_string "no CHAPS business day within two weeks")
;;
