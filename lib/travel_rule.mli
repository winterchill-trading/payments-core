(** Originator and beneficiary information that must travel with a crypto-asset
    transfer (FATF Recommendation 16, EU Transfer of Funds Regulation 2023/1113).

    Under the EU regulation there is no minimum amount between service providers: every
    transfer carries the originator's name, account (address) and one identifying
    attribute, and the beneficiary's name and account. A transfer of more than 1000 EUR
    to or from a self-hosted wallet additionally needs the provider to have verified that
    its customer owns or controls that wallet. *)

open! Core

module Lei : sig
  (** Legal Entity Identifier (ISO 17442): 20 alphanumerics, mod-97 check digits. *)
  type t [@@deriving bin_io, sexp, compare, equal, hash]

  val of_string : string -> t Or_error.t
  val to_string : t -> string
end

module Identification : sig
  (** The originator attribute besides name and account: any one of these. *)
  type t =
    | Address of Iso20022.Postal_address.t
    | Official_document of
        { kind : string (** passport, national id card, ... *)
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

module Person : sig
  type t =
    { name : string
    ; account : string (** wallet address or account number at the provider *)
    ; identification : Identification.t option
    ; lei : Lei.t option (** for legal persons, when they have one *)
    }
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Counterparty_wallet : sig
  type t =
    | Hosted of
        { provider_name : string
        ; provider_lei : Lei.t option
        }
    | Self_hosted of { ownership_verified : bool }
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Stable : sig
  module V1 : sig
    type t =
      { originator : Person.t
      ; beneficiary : Person.t
      ; counterparty_wallet : Counterparty_wallet.t
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

type t = Stable.V1.t =
  { originator : Person.t
  ; beneficiary : Person.t
  ; counterparty_wallet : Counterparty_wallet.t
  }
[@@deriving bin_io, sexp, compare, equal, hash]

(** The self-hosted wallet threshold: 1000 EUR. *)
val self_hosted_threshold_eur : Money.Amount.t

(** Fails when a required field is missing. [eur_value] is the transfer's value in EUR
    at the time; when unknown, a self-hosted wallet must be verified. *)
val check : t -> eur_value:Money.Amount.t option -> unit Or_error.t
