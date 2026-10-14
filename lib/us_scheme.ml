open! Core

module Scheme = struct
  type t =
    | Ach
    | Rtp
    | Fedwire
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]

  let of_rail : Rail.t -> t option = function
    | Ach _ -> Some Ach
    | Rtp -> Some Rtp
    | Fedwire -> Some Fedwire
    | Sepa_credit
    | Sepa_instant
    | Onchain _
    | Lightning
    | Intraledger
    | Card_scheme
    | Faster_payments
    | Chaps
    | Bacs -> None
  ;;
end

let usd s = Money.Amount.of_decimal_string Money.Currency.usd s |> Or_error.ok_exn

(* Raised from USD 1 million to USD 10 million in February 2025. *)
let rtp_network_max = usd "10000000.00"

(* Raised from USD 100,000 to USD 1 million in March 2022. *)
let same_day_ach_max = usd "1000000.00"

module Rules = struct
  type t = { rtp_max_amount : Money.Amount.t } [@@deriving fields ~getters]

  let default = { rtp_max_amount = rtp_network_max }

  let create ?(rtp_max_amount = rtp_network_max) () =
    let ok =
      Money.Currency.equal (Money.Amount.currency rtp_max_amount) Money.Currency.usd
      && Money.Amount.is_positive rtp_max_amount
      &&
      match Money.Amount.compare_value rtp_max_amount rtp_network_max with
      | Ok c -> c <= 0
      | Error _ -> false
    in
    if ok
    then Ok { rtp_max_amount }
    else
      Or_error.error_s
        [%message
          "RTP limit must be a positive USD amount within the network's"
            (rtp_max_amount : Money.Amount.t)
            (rtp_network_max : Money.Amount.t)]
  ;;
end

module Details = struct
  module Stable = struct
    module V1 = struct
      type t =
        { scheme_ref : string option
        ; effective_date : Date.Stable.V1.t option
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  include Stable.V1

  let empty = { scheme_ref = None; effective_date = None }
end

let max_reference_length : Scheme.t -> int = function
  | Ach -> 80
  | Rtp | Fedwire -> 140
;;

let over amount max =
  Result.ok (Money.Amount.compare_value amount max) |> Option.exists ~f:(fun c -> c > 0)
;;

let validate ?(rules = Rules.default) (rail : Rail.t) ~amount ~reference =
  let open Or_error.Let_syntax in
  let%bind scheme =
    Scheme.of_rail rail
    |> Or_error.of_option
         ~error:(Error.create_s [%message "not a US rail" (rail : Rail.t)])
  in
  let%bind () =
    if Money.Currency.equal (Money.Amount.currency amount) Money.Currency.usd
    then Ok ()
    else
      Or_error.error_s
        [%message "US domestic payments are in USD" (amount : Money.Amount.t)]
  in
  let%bind () =
    if Money.Amount.is_positive amount
    then Ok ()
    else Or_error.error_s [%message "amount must be positive" (amount : Money.Amount.t)]
  in
  let%bind () =
    match rail with
    | Rtp when over amount (Rules.rtp_max_amount rules) ->
      Or_error.error_s
        [%message
          "over the RTP limit"
            (amount : Money.Amount.t)
            ~max:(Rules.rtp_max_amount rules : Money.Amount.t)]
    | Ach { same_day = true; _ } when over amount same_day_ach_max ->
      Or_error.error_s
        [%message
          "over the Same Day ACH limit"
            (amount : Money.Amount.t)
            ~max:(same_day_ach_max : Money.Amount.t)]
    | _ -> Ok ()
  in
  let max_len = max_reference_length scheme in
  let%bind () =
    if String.length reference <= max_len
    then Ok ()
    else
      Or_error.error_s
        [%message "reference too long" (scheme : Scheme.t) (max_len : int) reference]
  in
  match String.find reference ~f:(Fn.non Char.is_print) with
  | None -> Ok ()
  | Some c ->
    Or_error.error_s
      [%message "character not allowed in the reference" (scheme : Scheme.t) (c : char)]
;;
