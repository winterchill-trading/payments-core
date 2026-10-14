open! Core

module T = struct
  type t = string [@@deriving bin_io, compare, equal, hash]

  let of_string raw =
    let s =
      String.filter raw ~f:(fun c -> not (Char.equal c '-' || Char.is_whitespace c))
    in
    if String.length s = 6 && String.for_all s ~f:Char.is_digit
    then Ok s
    else Or_error.error_s [%message "sort code must be six digits" (raw : string)]
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

let to_string_hum t =
  sprintf
    "%s-%s-%s"
    (String.sub t ~pos:0 ~len:2)
    (String.sub t ~pos:2 ~len:2)
    (String.sub t ~pos:4 ~len:2)
;;

let to_int t = Int.of_string t
let quickcheck_generator = String.gen_with_length 6 Quickcheck.Generator.char_digit
let quickcheck_observer = String.quickcheck_observer
let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
