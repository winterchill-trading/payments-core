open! Core

module Account_type = struct
  type t =
    | Checking
    | Savings
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]

  let to_string = function
    | Checking -> "checking"
    | Savings -> "savings"
  ;;
end

module Stable = struct
  module V1 = struct
    type t =
      { routing : Aba.Stable.V1.t
      ; account_number : string
      ; account_type : Account_type.t
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

include Stable.V1

let create ?(account_type = Account_type.Checking) ~routing ~account_number () =
  let n =
    String.filter account_number ~f:(fun c ->
      not (Char.equal c '-' || Char.is_whitespace c))
  in
  let len = String.length n in
  if len >= 4 && len <= 17 && String.for_all n ~f:Char.is_digit
  then Ok { routing; account_number = n; account_type }
  else
    Or_error.error_s
      [%message "US account number must have 4 to 17 digits" (account_number : string)]
;;

let of_string raw =
  let open Or_error.Let_syntax in
  let parts =
    String.split_on_chars (String.strip raw) ~on:[ ' '; '/' ]
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  let%bind routing, number, account_type =
    match parts with
    | [ r; n ] -> Ok (r, n, None)
    | [ r; n; kind ] ->
      (match String.lowercase kind with
       | "checking" -> Ok (r, n, Some Account_type.Checking)
       | "savings" -> Ok (r, n, Some Savings)
       | _ -> Or_error.error_s [%message "account type is checking or savings" kind])
    | _ ->
      Or_error.error_s
        [%message "expected a routing number and an account number" (raw : string)]
  in
  let%bind routing = Aba.of_string routing in
  create ?account_type ~routing ~account_number:number ()
;;

let to_string t =
  sprintf
    "%s %s %s"
    (Aba.to_string t.routing)
    t.account_number
    (Account_type.to_string t.account_type)
;;

let quickcheck_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%bind routing = Aba.quickcheck_generator in
  let%bind len = Int.gen_incl 4 17 in
  let%bind account_number = String.gen_with_length len Quickcheck.Generator.char_digit in
  let%map account_type = Account_type.quickcheck_generator in
  { routing; account_number; account_type }
;;

let quickcheck_observer =
  Quickcheck.Observer.unmap String.quickcheck_observer ~f:to_string
;;

let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
