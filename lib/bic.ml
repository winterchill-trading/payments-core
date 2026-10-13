open! Core

module T = struct
  type t = string [@@deriving bin_io, compare, equal, hash]

  let of_string raw =
    let s = String.uppercase (String.strip raw) in
    let len = String.length s in
    let letters i j = String.for_all (String.sub s ~pos:i ~len:(j - i)) ~f:Char.is_alpha in
    let alnum i j = String.for_all (String.sub s ~pos:i ~len:(j - i)) ~f:Char.is_alphanum in
    if (len = 8 || len = 11) && letters 0 6 && alnum 6 len
    then Ok s
    else Or_error.error_s [%message "invalid BIC" (raw : string)]
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
let country t = String.sub t ~pos:4 ~len:2

let branch t =
  match String.length t with
  | 11 when not (String.equal (String.suffix t 3) "XXX") -> Some (String.suffix t 3)
  | _ -> None
;;
