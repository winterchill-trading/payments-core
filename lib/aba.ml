open! Core

let weights = [| 3; 7; 1; 3; 7; 1; 3; 7; 1 |]
let digits s = String.to_list s |> List.map ~f:Char.get_digit_exn

let check_digit eight =
  if String.length eight = 8 && String.for_all eight ~f:Char.is_digit
  then (
    let sum =
      List.foldi (digits eight) ~init:0 ~f:(fun i acc d -> acc + (weights.(i) * d))
    in
    Ok ((10 - (sum % 10)) % 10))
  else Or_error.error_s [%message "expected eight digits" eight]
;;

module T = struct
  type t = string [@@deriving bin_io, compare, equal, hash]

  let of_string raw =
    let s =
      String.filter raw ~f:(fun c -> not (Char.equal c '-' || Char.is_whitespace c))
    in
    if String.length s <> 9 || not (String.for_all s ~f:Char.is_digit)
    then Or_error.error_s [%message "routing number must be nine digits" (raw : string)]
    else (
      let sum =
        List.foldi (digits s) ~init:0 ~f:(fun i acc d -> acc + (weights.(i) * d))
      in
      if sum % 10 = 0
      then Ok s
      else
        Or_error.error_s
          [%message "routing number check digit does not match" (raw : string)])
  ;;

  let sexp_of_t = String.sexp_of_t
  let t_of_sexp sexp = of_string (String.t_of_sexp sexp) |> Or_error.ok_exn
end

include T

module Stable = struct
  module V1 = T
end

let of_string_exn s = of_string s |> Or_error.ok_exn
let to_string t = t

let quickcheck_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%map eight = String.gen_with_length 8 Quickcheck.Generator.char_digit in
  eight ^ Int.to_string (check_digit eight |> Or_error.ok_exn)
;;

let quickcheck_observer = String.quickcheck_observer
let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
