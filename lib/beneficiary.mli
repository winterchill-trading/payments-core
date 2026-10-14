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
      Payee, UK Confirmation of Payee), when the provider offers one. *)
  type t =
    | Match
    | Close_match of { suggested_name : string }
    | No_match
    | Not_possible
    (** the receiving bank cannot check this account (not taking part, account type not
        covered, account switched) *)
    | Account_not_found (** the receiving bank has no such account *)
    | Unavailable (** no answer this time (time-out, service down); worth retrying *)
  [@@deriving sexp, compare, equal, hash]

  (** [bin_io] on [t] is {!Stable.V1}. *)
  include Binable.S with type t := t

  module Stable : sig
    module V2 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end

    module V1 : sig
      (** The first four results' format. [Account_not_found] and [Unavailable] are
          written as [Not_possible], which they refine, so a V1 round trip loses that
          detail. *)
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
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
    | Uk_account of Uk_account.Stable.V1.t (** sort code and account number *)
  [@@deriving sexp, compare, equal, hash]

  (** [bin_io] on [t] is {!Stable.V1}. *)
  include Binable.S with type t := t

  module Stable : sig
    module V2 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end

    module V1 : sig
      (** The first four destinations' format; writing a [Uk_account] raises. *)
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  (** Rails that can deliver to this destination. A GB IBAN reaches the UK rails as well
      as SEPA, since it carries the sort code and account number. *)
  val rails : t -> Rail.t list

  (** The sort code and account number, from a [Uk_account] or a GB IBAN. *)
  val uk_account : t -> Uk_account.t option
end

module Stable : sig
  module V2 : sig
    type t =
      { id : Id.t
      ; owner : string (** customer id owning this beneficiary *)
      ; name : string (** account holder name as entered *)
      ; destination : Destination.Stable.V2.t
      ; trusted : bool
      ; first_seen : Time_ns.Stable.V1.t
      ; payee_check : Payee_check.Stable.V2.t option
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end

  module V1 : sig
    (** The format before UK destinations (digest [a75ca3f3…]): writing a [Uk_account]
        destination raises, and newer payee check results are written as [Not_possible]. *)
    type t = V2.t =
      { id : Id.t
      ; owner : string
      ; name : string
      ; destination : Destination.Stable.V1.t
      ; trusted : bool
      ; first_seen : Time_ns.Stable.V1.t
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
  ; first_seen : Time_ns.t
  ; payee_check : Payee_check.t option
  }
[@@deriving sexp, compare, equal, hash, fields]

(** [bin_io] on [t] is {!Stable.V1}, so records that embed a beneficiary keep their
    format; use {!Stable.V2} where a UK destination can occur. *)
include Binable.S with type t := t

(** Parses the destination for a rail: an IBAN for SEPA rails, an address on the rail's
    network for on-chain, an invoice, LNURL or address for Lightning, an account id for
    intraledger, a sort code and account number or a GB IBAN for the UK rails
    ({!Uk_account.of_string}, checked against [modulus] when given, with [roll_number]).
    New beneficiaries start untrusted. *)
val create
  :  id:Id.t
  -> owner:string
  -> name:string
  -> rail:Rail.t
  -> ?bic:string
  -> ?memo:string
  -> ?roll_number:string
  -> ?modulus:Uk_modulus.Table.t
  -> now:Time_ns.t
  -> string
  -> t Or_error.t

val usable_on : t -> Rail.t -> bool
val trust : t -> t
val untrust : t -> t
val with_payee_check : t -> Payee_check.t -> t

(** Younger than [cooling_off] at [now]. *)
val is_new : t -> now:Time_ns.t -> cooling_off:Time_ns.Span.t -> bool
