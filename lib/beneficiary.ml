open! Core

module Id =
  Opaque_id.Make
    (struct
      let module_name = "Payments_core.Beneficiary.Id"
    end)
    ()

module Payee_check = struct
  module T = struct
    type t =
      | Match
      | Close_match of { suggested_name : string }
      | No_match
      | Not_possible
      | Account_not_found
      | Unavailable
    [@@deriving bin_io, sexp, compare, equal, hash]
  end

  module Stable = struct
    module V2 = T

    module V1 = struct
      (* The original four results: the bin_prot shape and digest come from here. *)
      module Repr = struct
        type t =
          | Match
          | Close_match of { suggested_name : string }
          | No_match
          | Not_possible
        [@@deriving bin_io]
      end

      type t = T.t [@@deriving sexp, compare, equal, hash]

      include
        Binable.Of_binable_without_uuid [@alert "-legacy"]
          (Repr)
          (struct
            type nonrec t = t

            let to_binable : t -> Repr.t = function
              | Match -> Match
              | Close_match { suggested_name } -> Close_match { suggested_name }
              | No_match -> No_match
              (* Both refine "the check could not be made", which is all V1 can say. *)
              | Not_possible | Account_not_found | Unavailable -> Not_possible
            ;;

            let of_binable : Repr.t -> t = function
              | Match -> Match
              | Close_match { suggested_name } -> Close_match { suggested_name }
              | No_match -> No_match
              | Not_possible -> Not_possible
            ;;
          end)
    end
  end

  type t = T.t =
    | Match
    | Close_match of { suggested_name : string }
    | No_match
    | Not_possible
    | Account_not_found
    | Unavailable
  [@@deriving sexp, compare, equal, hash]

  include (Stable.V1 : Binable.S with type t := t)
end

module Destination = struct
  module T = struct
    type t =
      | Bank_account of
          { iban : Iban.Stable.V1.t
          ; bic : Bic.Stable.V1.t option
          }
      | Crypto of
          { address : Crypto_address.Stable.V2.t
          ; memo : string option
          }
      | Lightning of Lightning.Target.Stable.V1.t
      | Internal of { account : string }
      | Uk_account of Uk_account.Stable.V1.t
      | Us_account of Us_account.Stable.V1.t
    [@@deriving bin_io, sexp, compare, equal, hash]
  end

  module Stable = struct
    module V2 = T

    module V1 = struct
      (* The original four destinations: the bin_prot shape and digest come from here. *)
      module Repr = struct
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
        [@@deriving bin_io]
      end

      type t = T.t [@@deriving sexp, compare, equal, hash]

      include
        Binable.Of_binable_without_uuid [@alert "-legacy"]
          (Repr)
          (struct
            type nonrec t = t

            let to_binable : t -> Repr.t = function
              | Bank_account { iban; bic } -> Bank_account { iban; bic }
              | Crypto { address; memo } -> Crypto { address; memo }
              | Lightning target -> Lightning target
              | Internal { account } -> Internal { account }
              | Uk_account account ->
                raise_s
                  [%message
                    "Beneficiary.Destination.Stable.V1 cannot hold a UK account; write V2"
                      ~_:(Uk_account.to_string account : string)]
              | Us_account account ->
                raise_s
                  [%message
                    "Beneficiary.Destination.Stable.V1 cannot hold a US account; write V2"
                      ~_:(Us_account.to_string account : string)]
            ;;

            let of_binable : Repr.t -> t = function
              | Bank_account { iban; bic } -> Bank_account { iban; bic }
              | Crypto { address; memo } -> Crypto { address; memo }
              | Lightning target -> Lightning target
              | Internal { account } -> Internal { account }
            ;;
          end)
    end
  end

  type t = T.t =
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
    | Uk_account of Uk_account.Stable.V1.t
    | Us_account of Us_account.Stable.V1.t
  [@@deriving sexp, compare, equal, hash]

  include (Stable.V1 : Binable.S with type t := t)

  let uk_rails = [ Rail.Faster_payments; Chaps; Bacs ]

  let rails = function
    | Bank_account { iban; _ } ->
      (if Iban.is_sepa iban then [ Rail.Sepa_credit; Sepa_instant ] else [])
      @ if String.equal (Iban.country iban) "GB" then uk_rails else []
    | Crypto { address; _ } -> [ Onchain (Crypto_address.network address) ]
    | Lightning _ -> [ Lightning ]
    | Internal _ -> [ Intraledger ]
    | Uk_account _ -> uk_rails
    | Us_account _ -> List.filter Rail.all ~f:Rail.is_us
  ;;

  let uk_account = function
    | Uk_account a -> Some a
    | Bank_account { iban; _ } -> Uk_account.of_iban iban |> Result.ok
    | Crypto _ | Lightning _ | Internal _ | Us_account _ -> None
  ;;

  let us_account = function
    | Us_account a -> Some a
    | Bank_account _ | Crypto _ | Lightning _ | Internal _ | Uk_account _ -> None
  ;;
end

module Stable = struct
  module V2 = struct
    type t =
      { id : Id.t
      ; owner : string
      ; name : string
      ; destination : Destination.Stable.V2.t
      ; trusted : bool
      ; first_seen : Utc_time.Stable.V1.t
      ; payee_check : Payee_check.Stable.V2.t option
      }
    [@@deriving bin_io, sexp, compare, equal, hash, fields]
  end

  module V1 = struct
    (* Same record as V2; only the field formats differ, so the shape (and digest) is the
       original one. *)
    type t = V2.t =
      { id : Id.t
      ; owner : string
      ; name : string
      ; destination : Destination.Stable.V1.t
      ; trusted : bool
      ; first_seen : Utc_time.Stable.V1.t
      ; payee_check : Payee_check.Stable.V1.t option
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

type t = Stable.V2.t =
  { id : Id.t
  ; owner : string
  ; name : string
  ; destination : Destination.t
  ; trusted : bool
  ; first_seen : Utc_time.t
  ; payee_check : Payee_check.t option
  }
[@@deriving sexp, compare, equal, hash, fields]

include (Stable.V1 : Binable.S with type t := t)

let destination_for ~(rail : Rail.t) ?bic ?memo ?roll_number ?modulus raw =
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
    let%bind account = Uk_account.of_string ?modulus raw in
    let%map account =
      match roll_number with
      | None -> Ok account
      | Some roll_number ->
        Uk_account.create
          ?modulus
          ~roll_number
          ~sort_code:account.sort_code
          ~account_number:account.account_number
          ()
    in
    Destination.Uk_account account
  | Ach _ | Rtp | Fedwire ->
    let%map account = Us_account.of_string raw in
    Destination.Us_account account
;;

let create ~id ~owner ~name ~rail ?bic ?memo ?roll_number ?modulus ~now raw =
  let open Or_error.Let_syntax in
  let name = String.strip name in
  let%bind () =
    if String.is_empty name
    then Or_error.error_string "beneficiary name is empty"
    else Ok ()
  in
  let%map destination = destination_for ~rail ?bic ?memo ?roll_number ?modulus raw in
  { id; owner; name; destination; trusted = false; first_seen = now; payee_check = None }
;;

let usable_on t rail = List.mem (Destination.rails t.destination) rail ~equal:Rail.equal
let trust t = { t with trusted = true }
let untrust t = { t with trusted = false }
let with_payee_check t check = { t with payee_check = Some check }

let is_new t ~now ~cooling_off =
  Time_ns.Span.( < ) (Time_ns.diff now t.first_seen) cooling_off
;;
