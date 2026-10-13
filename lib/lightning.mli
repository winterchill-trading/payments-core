(** Lightning payment targets: BOLT 11 invoices, LNURL (LUD-01) and Lightning addresses
    (LUD-16).

    Invoices are checked for Bech32 integrity, network, amount and the tagged fields this
    library needs. The node signature is not verified here (that needs secp256k1 public
    key recovery); the provider paying the invoice verifies it. *)

open! Core

module Invoice : sig
  type t =
    { raw : string (** lower case *)
    ; network : Network.t (** [Bitcoin] or [Bitcoin_testnet] *)
    ; amount_msat : int option (** [None] for "any amount" invoices *)
    ; timestamp : Time_ns.t
    ; expiry : Time_ns.Span.t (** defaults to one hour when the field is absent *)
    ; payment_hash : string (** hex *)
    ; description : string option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  val of_string : string -> t Or_error.t
  val expires_at : t -> Time_ns.t
  val is_expired : t -> now:Time_ns.t -> bool
end

module Target : sig
  type t =
    | Invoice of Invoice.t
    | Lnurl of string (** the decoded https URL *)
    | Address of
        { user : string
        ; domain : string
        }
  [@@deriving bin_io, sexp, compare, equal, hash]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  (** Accepts an invoice, an [lnurl1...] string, a [user@domain] address, each with an
      optional [lightning:] URI prefix. *)
  val of_string : string -> t Or_error.t

  (** The LNURL-pay endpoint an LNURL or a Lightning address resolves to. *)
  val pay_url : t -> string option
end
