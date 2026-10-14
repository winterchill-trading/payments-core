open! Core

module Id =
  Opaque_id.Make
    (struct
      let module_name = "Payments_core.Beneficiary.Id"
    end)
    ()

module Payee_check = struct
  type t =
    | Match
    | Close_match of { suggested_name : string }
    | No_match
    | Not_possible
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Destination = struct
  type t =
    | Bank_account of
        { iban : Iban.Stable.V1.t
        ; bic : Bic.Stable.V1.t option
        }
    | Crypto of
        { address : Crypto_address.Stable.V1.t
        ; memo : string option
        }
    | Lightning of Lightning.Target.Stable.V1.t
    | Internal of { account : string }
  [@@deriving bin_io, sexp, compare, equal, hash]

  let rails = function
    | Bank_account { iban; _ } ->
      if Iban.is_sepa iban then [ Rail.Sepa_credit; Sepa_instant ] else []
    | Crypto { address; _ } -> [ Onchain (Crypto_address.network address) ]
    | Lightning _ -> [ Lightning ]
    | Internal _ -> [ Intraledger ]
  ;;
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Id.t
      ; owner : string
      ; name : string
      ; destination : Destination.t
      ; trusted : bool
      ; first_seen : Utc_time.Stable.V1.t
      ; payee_check : Payee_check.t option
      }
    [@@deriving bin_io, sexp, compare, equal, hash, fields]
  end
end

include Stable.V1

let destination_for ~(rail : Rail.t) ?bic ?memo raw =
  let open Or_error.Let_syntax in
  match rail with
  | Sepa_credit | Sepa_instant ->
    let%bind iban = Iban.of_string raw in
    let%bind bic =
      match bic with
      | None -> Ok None
      | Some b -> Bic.of_string b >>| Option.some
    in
    if Iban.is_sepa iban
    then Ok (Destination.Bank_account { iban; bic })
    else Or_error.error_s [%message "IBAN country is outside SEPA" (Iban.country iban)]
  | Onchain network ->
    let%map address = Crypto_address.create network raw in
    Destination.Crypto { address; memo }
  | Lightning ->
    let%map target = Lightning.Target.of_string raw in
    Destination.Lightning target
  | Intraledger ->
    if String.is_empty (String.strip raw)
    then Or_error.error_string "empty account id"
    else Ok (Destination.Internal { account = String.strip raw })
  | Card_scheme -> Or_error.error_string "card payments have no beneficiary"
  | Faster_payments | Chaps | Bacs ->
    Or_error.error_string "UK rails need a UK account destination"
;;

let create ~id ~owner ~name ~rail ?bic ?memo ~now raw =
  let open Or_error.Let_syntax in
  let name = String.strip name in
  let%bind () =
    if String.is_empty name
    then Or_error.error_string "beneficiary name is empty"
    else Ok ()
  in
  let%map destination = destination_for ~rail ?bic ?memo raw in
  { id; owner; name; destination; trusted = false; first_seen = now; payee_check = None }
;;

let usable_on t rail = List.mem (Destination.rails t.destination) rail ~equal:Rail.equal
let trust t = { t with trusted = true }
let untrust t = { t with trusted = false }
let with_payee_check t check = { t with payee_check = Some check }

let is_new t ~now ~cooling_off =
  Time_ns.Span.( < ) (Time_ns.diff now t.first_seen) cooling_off
;;
