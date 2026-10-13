(** Someone a customer pays: a bank account, a crypto address, a Lightning target or
    another account of the same institution.

    [trusted] and [first_seen] feed risk rules and strong customer authentication: a
    payment to a beneficiary the customer marked trusted may be exempt from a challenge
    (PSD2 RTS article 13), and a beneficiary first seen minutes ago is a classic
    account-takeover signal. *)

open! Core
module Id : Opaque_id.S

module Payee_check : sig
  (** Outcome of a payee-name check against the receiving bank (SEPA Verification of
      Payee), when the provider offers one. *)
  type t =
    | Match
    | Close_match of { suggested_name : string }
    | No_match
    | Not_possible
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Destination : sig
  type t =
    | Bank_account of
        { iban : Iban.Stable.V1.t
        ; bic : Bic.Stable.V1.t option
        }
    | Crypto of
        { address : Crypto_address.Stable.V1.t
        ; memo : string option (** destination tag or memo, where a chain uses one *)
        }
    | Lightning of Lightning.Target.Stable.V1.t
    | Internal of { account : string (** ledger account id of the recipient *) }
  [@@deriving bin_io, sexp, compare, equal, hash]

  (** Rails that can deliver to this destination. *)
  val rails : t -> Rail.t list
end

module Stable : sig
  module V1 : sig
    type t =
      { id : Id.t
      ; owner : string (** customer id owning this beneficiary *)
      ; name : string (** account holder name as entered *)
      ; destination : Destination.t
      ; trusted : bool
      ; first_seen : Time_ns.Stable.V1.t
      ; payee_check : Payee_check.t option
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

type t = Stable.V1.t =
  { id : Id.t
  ; owner : string
  ; name : string
  ; destination : Destination.t
  ; trusted : bool
  ; first_seen : Time_ns.t
  ; payee_check : Payee_check.t option
  }
[@@deriving bin_io, sexp, compare, equal, hash, fields]

(** Parses the destination for a rail: an IBAN for SEPA rails, an address on the rail's
    network for on-chain, an invoice, LNURL or address for Lightning, an account id for
    intraledger. New beneficiaries start untrusted. *)
val create
  :  id:Id.t
  -> owner:string
  -> name:string
  -> rail:Rail.t
  -> ?bic:string
  -> ?memo:string
  -> now:Time_ns.t
  -> string
  -> t Or_error.t

val usable_on : t -> Rail.t -> bool
val trust : t -> t
val untrust : t -> t
val with_payee_check : t -> Payee_check.t -> t

(** Younger than [cooling_off] at [now]. *)
val is_new : t -> now:Time_ns.t -> cooling_off:Time_ns.Span.t -> bool
