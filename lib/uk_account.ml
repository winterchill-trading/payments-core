open! Core

module Stable = struct
  module V1 = struct
    type t =
      { sort_code : Sort_code.Stable.V1.t
      ; account_number : string
      ; roll_number : string option
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

include Stable.V1

let check_roll_number raw =
  let r = String.strip raw in
  let ok_char c =
    Char.is_alphanum c || List.mem [ ' '; '/'; '-'; '.' ] c ~equal:Char.equal
  in
  if String.length r >= 1 && String.length r <= 18 && String.for_all r ~f:ok_char
  then Ok r
  else
    Or_error.error_s
      [%message
        "roll number must be 1 to 18 letters, digits, spaces or / - ." (raw : string)]
;;

let create ?modulus ?roll_number ~sort_code ~account_number () =
  let open Or_error.Let_syntax in
  let%bind account_number = Uk_modulus.normalize_account_number account_number in
  let%bind roll_number =
    match roll_number with
    | None -> Ok None
    | Some r -> check_roll_number r >>| Option.some
  in
  let%map () =
    match modulus with
    | None -> Ok ()
    | Some table ->
      (match%bind Uk_modulus.check table ~sort_code ~account_number with
       | Valid | Not_checked -> Ok ()
       | Invalid ->
         Or_error.error_s
           [%message
             "account number fails the modulus check for its sort code"
               ~sort_code:(Sort_code.to_string_hum sort_code)
               (account_number : string)])
  in
  { sort_code; account_number; roll_number }
;;

let of_iban iban =
  let open Or_error.Let_syntax in
  if not (String.equal (Iban.country iban) "GB")
  then Or_error.error_s [%message "not a GB IBAN" (iban : Iban.t)]
  else (
    (* GB BBAN: four letters of bank code, six digits of sort code, eight of account. *)
    let bban = Iban.bban iban in
    let%bind sort_code = Sort_code.of_string (String.sub bban ~pos:4 ~len:6) in
    create ~sort_code ~account_number:(String.sub bban ~pos:10 ~len:8) ())
;;

let of_string ?modulus raw =
  let open Or_error.Let_syntax in
  let s = String.strip raw in
  match Iban.of_string s with
  | Ok iban -> of_iban iban
  | Error _ ->
    (match
       String.split_on_chars s ~on:[ ' '; '/' ] |> List.filter ~f:(Fn.non String.is_empty)
     with
     | [ sc; acct ] ->
       let%bind sort_code = Sort_code.of_string sc in
       create ?modulus ~sort_code ~account_number:acct ()
     (* "04 00 04 12345678": a sort code written with spaces. *)
     | [ a; b; c; acct ] ->
       let%bind sort_code = Sort_code.of_string (a ^ b ^ c) in
       create ?modulus ~sort_code ~account_number:acct ()
     | _ ->
       Or_error.error_s
         [%message "expected a GB IBAN or a sort code and account number" (raw : string)])
;;

let to_string t =
  let base = Sort_code.to_string_hum t.sort_code ^ " " ^ t.account_number in
  match t.roll_number with
  | None -> base
  | Some r -> base ^ " roll " ^ r
;;

let quickcheck_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%bind sort_code = Sort_code.quickcheck_generator in
  let%bind account_number = String.gen_with_length 8 Quickcheck.Generator.char_digit in
  let%map roll_number =
    Option.quickcheck_generator
      (String.gen_with_length 6 Quickcheck.Generator.char_alphanum)
  in
  { sort_code; account_number; roll_number }
;;

let quickcheck_observer =
  Quickcheck.Observer.unmap String.quickcheck_observer ~f:to_string
;;

let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
