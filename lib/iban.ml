open! Core

(* IBAN lengths from the SWIFT IBAN registry. *)
let lengths =
  String.Map.of_alist_exn
    [ "AD", 24
    ; "AE", 23
    ; "AL", 28
    ; "AT", 20
    ; "AZ", 28
    ; "BA", 20
    ; "BE", 16
    ; "BG", 22
    ; "BH", 22
    ; "BR", 29
    ; "BY", 28
    ; "CH", 21
    ; "CR", 22
    ; "CY", 28
    ; "CZ", 24
    ; "DE", 22
    ; "DK", 18
    ; "DO", 28
    ; "EE", 20
    ; "EG", 29
    ; "ES", 24
    ; "FI", 18
    ; "FO", 18
    ; "FR", 27
    ; "GB", 22
    ; "GE", 22
    ; "GI", 23
    ; "GL", 18
    ; "GR", 27
    ; "GT", 28
    ; "HR", 21
    ; "HU", 28
    ; "IE", 22
    ; "IL", 23
    ; "IQ", 23
    ; "IS", 26
    ; "IT", 27
    ; "JO", 30
    ; "KW", 30
    ; "KZ", 20
    ; "LB", 28
    ; "LC", 32
    ; "LI", 21
    ; "LT", 20
    ; "LU", 20
    ; "LV", 21
    ; "MC", 27
    ; "MD", 24
    ; "ME", 22
    ; "MK", 19
    ; "MR", 27
    ; "MT", 31
    ; "MU", 30
    ; "NL", 18
    ; "NO", 15
    ; "PK", 24
    ; "PL", 28
    ; "PS", 29
    ; "PT", 25
    ; "QA", 29
    ; "RO", 24
    ; "RS", 22
    ; "SA", 24
    ; "SC", 31
    ; "SE", 24
    ; "SI", 19
    ; "SK", 24
    ; "SM", 27
    ; "ST", 25
    ; "SV", 28
    ; "TL", 23
    ; "TN", 24
    ; "TR", 26
    ; "UA", 29
    ; "VA", 22
    ; "VG", 24
    ; "XK", 20
    ]
;;

(* EEA members plus the non-EEA scheme participants. Albania, Moldova, Montenegro and
   North Macedonia were admitted to the scheme in 2025. *)
let sepa_countries =
  String.Set.of_list
    [ "AT"
    ; "BE"
    ; "BG"
    ; "HR"
    ; "CY"
    ; "CZ"
    ; "DK"
    ; "EE"
    ; "FI"
    ; "FR"
    ; "DE"
    ; "GR"
    ; "HU"
    ; "IE"
    ; "IT"
    ; "LV"
    ; "LT"
    ; "LU"
    ; "MT"
    ; "NL"
    ; "PL"
    ; "PT"
    ; "RO"
    ; "SK"
    ; "SI"
    ; "ES"
    ; "SE"
    ; "IS"
    ; "LI"
    ; "NO"
    ; "CH"
    ; "GB"
    ; "SM"
    ; "VA"
    ; "MC"
    ; "AD"
    ; "GI"
    ; "AL"
    ; "MD"
    ; "ME"
    ; "MK"
    ]
;;

let length_for_country c = Map.find lengths c

(* Mod 97 over the rearranged IBAN with letters expanded to 10..35, computed digit by
   digit so no bignum is needed. *)
let mod97 s =
  String.fold s ~init:0 ~f:(fun acc c ->
    if Char.is_digit c
    then ((acc * 10) + Char.get_digit_exn c) % 97
    else ((acc * 100) + (Char.to_int c - Char.to_int 'A' + 10)) % 97)
;;

let check_digits ~country ~bban =
  let r = mod97 (bban ^ country ^ "00") in
  sprintf "%02d" (98 - r)
;;

module T = struct
  type t = string [@@deriving bin_io, compare, equal, hash]

  let normalize s =
    String.filter s ~f:(fun c -> not (Char.is_whitespace c)) |> String.uppercase
  ;;

  let of_string raw =
    let s = normalize raw in
    let open Or_error.Let_syntax in
    let%bind () =
      if String.length s >= 4 && String.for_all s ~f:Char.is_alphanum
      then Ok ()
      else Or_error.error_s [%message "IBAN must be alphanumeric" (raw : string)]
    in
    let country = String.prefix s 2 in
    let%bind expected =
      match length_for_country country with
      | Some n -> Ok n
      | None -> Or_error.error_s [%message "IBAN country not supported" country]
    in
    let%bind () =
      if String.length s = expected
      then Ok ()
      else
        Or_error.error_s
          [%message "IBAN length" country (expected : int) ~got:(String.length s : int)]
    in
    let rearranged = String.drop_prefix s 4 ^ String.prefix s 4 in
    if mod97 rearranged = 1
    then Ok s
    else Or_error.error_s [%message "IBAN check digits do not match" (raw : string)]
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

let to_string_grouped t =
  String.to_list t
  |> List.chunks_of ~length:4
  |> List.map ~f:String.of_char_list
  |> String.concat ~sep:" "
;;

let country t = String.prefix t 2
let bban t = String.drop_prefix t 4
let is_sepa t = Set.mem sepa_countries (country t)

(* Random valid IBANs: a known country and a numeric BBAN of the right length, with the
   check digits computed. Real BBANs also have per-country structure; this only needs to
   satisfy the checksum. *)
let quickcheck_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%bind country, len = Quickcheck.Generator.of_list (Map.to_alist lengths) in
  let%map digits = String.gen_with_length (len - 4) Quickcheck.Generator.char_digit in
  country ^ check_digits ~country ~bban:digits ^ digits
;;

let quickcheck_observer = String.quickcheck_observer
let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
