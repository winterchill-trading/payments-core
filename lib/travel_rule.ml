open! Core

module Lei = struct
  type t = string [@@deriving bin_io, sexp, compare, equal, hash]

  let of_string raw =
    let s = String.uppercase (String.strip raw) in
    let mod97 =
      String.fold s ~init:0 ~f:(fun acc c ->
        if Char.is_digit c
        then ((acc * 10) + Char.get_digit_exn c) % 97
        else ((acc * 100) + (Char.to_int c - Char.to_int 'A' + 10)) % 97)
    in
    if String.length s = 20 && String.for_all s ~f:Char.is_alphanum && mod97 = 1
    then Ok s
    else Or_error.error_s [%message "invalid LEI" raw]
  ;;

  let to_string t = t
end

module Identification = struct
  type t =
    | Address of Iso20022.Postal_address.t
    | Official_document of
        { kind : string
        ; number : string
        ; issuing_country : string
        }
    | Customer_number of string
    | Birth of
        { date : Date.Stable.V1.t
        ; place : string
        }
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Person = struct
  type t =
    { name : string
    ; account : string
    ; identification : Identification.t option
    ; lei : Lei.t option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Counterparty_wallet = struct
  type t =
    | Hosted of
        { provider_name : string
        ; provider_lei : Lei.t option
        }
    | Self_hosted of { ownership_verified : bool }
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Stable = struct
  module V1 = struct
    type t =
      { originator : Person.t
      ; beneficiary : Person.t
      ; counterparty_wallet : Counterparty_wallet.t
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

include Stable.V1

let self_hosted_threshold_eur =
  Money.Amount.of_decimal_string Money.Currency.eur "1000" |> Or_error.ok_exn
;;

let present ~field s =
  if String.is_empty (String.strip s)
  then Or_error.error_s [%message "travel rule field missing" field]
  else Ok ()
;;

let check t ~eur_value =
  let open Or_error.Let_syntax in
  let%bind () = present ~field:"originator name" t.originator.name in
  let%bind () = present ~field:"originator account" t.originator.account in
  let%bind () = present ~field:"beneficiary name" t.beneficiary.name in
  let%bind () = present ~field:"beneficiary account" t.beneficiary.account in
  let%bind () =
    match t.originator.identification with
    | Some _ -> Ok ()
    | None ->
      Or_error.error_string
        "originator needs an address, official document, customer number or date and \
         place of birth"
  in
  match t.counterparty_wallet with
  | Hosted { provider_name; _ } -> present ~field:"counterparty provider" provider_name
  | Self_hosted { ownership_verified = true } -> Ok ()
  | Self_hosted { ownership_verified = false } ->
    let over_threshold =
      match eur_value with
      | None -> true
      | Some v ->
        (match Money.Amount.compare_value v self_hosted_threshold_eur with
         | Ok c -> c > 0
         | Error _ -> true)
    in
    if over_threshold
    then
      Or_error.error_s
        [%message
          "self-hosted wallet ownership must be verified above the threshold"
            (eur_value : Money.Amount.t option)]
    else Ok ()
;;
