(** Someone a customer pays: a bank account, a crypto address, a Lightning target, another
    account of the same institution, or account details in a market's own format.

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
    | Us_account of Us_account.Stable.V1.t (** routing and account number, type *)
    | Local of
        { scheme : string
        (** names the format, lower snake case: [kg] (bank code and account number),
            [ch_qr_iban] (the QR-IBAN for Swiss QR-bill payments), ... *)
        ; fields : (string * string) list
        (** in display order; names are lower snake case ([bank_code], [account_number],
            [qr_iban]) *)
        }
    (** Details in a domestic format the other variants do not cover: markets with no IBAN
        (a bank code and an account number) or a second identifier next to the IBAN. The
        same shape as a local address in banking-provider, so details read from a provider
        carry over unchanged. Build one with {!create_local}. *)
  [@@deriving sexp, compare, equal, hash]

  (** [bin_io] on [t] is {!Stable.V1}. *)
  include Binable.S with type t := t

  module Stable : sig
    module V3 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end

    module V2 : sig
      (** The six destinations before [Local]; writing a [Local] destination raises. *)
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end

    module V1 : sig
      (** The first four destinations' format; writing a [Uk_account], a [Us_account], a
          [Local] destination or a crypto address on a newer network raises. *)
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  (** Rails that can deliver to this destination. A GB IBAN reaches the UK rails as well
      as SEPA, since it carries the sort code and account number. A [Local] destination
      has none: no rail in {!Rail.t} names its domestic scheme, so the provider holding
      the account routes it. *)
  val rails : t -> Rail.t list

  (** The sort code and account number, from a [Uk_account] or a GB IBAN. *)
  val uk_account : t -> Uk_account.t option

  val us_account : t -> Us_account.t option

  (** The fields of a [Local] destination with this [scheme]. *)
  val local : t -> scheme:string -> (string * string) list option

  (** The [scheme] names of the local formats this library knows, spelled as in
      banking-provider. *)
  module Local_scheme : sig
    (** Kyrgyzstan: fields [bank_code] (the six-digit BIK) and [account_number] (sixteen
        digits), for example
        [[ "bank_code", "109018"; "account_number", "1090000123456789" ]]. *)
    val kg : string

    (** Switzerland and Liechtenstein: field [qr_iban], the QR-IBAN that QR-bills name
        next to the account's ordinary IBAN. *)
    val ch_qr_iban : string
  end

  (** Validation hooks, one per scheme: a format checks and normalises the fields of a
      local destination before it is accepted. *)
  module Local_format : sig
    type t

    (** A hook for [scheme]. [validate] gets the fields with surrounding spaces removed
        and returns them as they should be stored (normalised, in display order), or an
        error a caller can show. *)
    val create
      :  scheme:string
      -> validate:((string * string) list -> (string * string) list Or_error.t)
      -> t

    val scheme : t -> string
    val validate : t -> (string * string) list -> (string * string) list Or_error.t

    (** {!Local_scheme.kg}: exactly [bank_code] (six digits) and [account_number] (sixteen
        digits), spaces removed, returned in that order. *)
    val kg : t

    (** {!Local_scheme.ch_qr_iban}: exactly [qr_iban], a valid CH or LI IBAN whose
        institution id is in the QR range 30000-31999, stored in electronic form. *)
    val ch_qr_iban : t

    (** [[kg; ch_qr_iban]]. *)
    val defaults : t list
  end

  (** A [Local] destination. Every scheme needs a lower snake case name and at least one
      field, with lower snake case names, no duplicates and non-empty values. When
      [formats] (default {!Local_format.defaults}) has a hook for [scheme], the fields
      must pass it and are stored as it returns them; other schemes are accepted with the
      generic checks only, so an adapter can carry a format this library does not know. *)
  val create_local
    :  ?formats:Local_format.t list
    -> scheme:string
    -> (string * string) list
    -> t Or_error.t
end

(** A structured postal address (ISO 20022 style: country, town, optional post code,
    street and building number, at most two lines). *)
module Postal_address = Iso20022.Postal_address

type t =
  { id : Id.t
  ; owner : string
  ; name : string
  ; destination : Destination.t
  ; trusted : bool
  ; first_seen : Time_ns.t
  ; payee_check : Payee_check.t option
  ; postal_address : Postal_address.t option
  (** the beneficiary's address, for rails that need one (wires); omitted from the sexp
      when [None] *)
  }
[@@deriving sexp, compare, equal, hash, fields]

module Stable : sig
  module V3 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end

  module V2 : sig
    (** The format before local destinations and postal addresses: writing a [Local]
        destination or a beneficiary with an address raises; reading gives no address. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end

  module V1 : sig
    (** The format before UK destinations (digest [a75ca3f3…]): writing a [Uk_account],
        [Us_account] or [Local] destination, or an address, raises, and newer payee check
        results are written as [Not_possible]. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

(** [bin_io] on [t] is {!Stable.V1}, so records that embed a beneficiary keep their
    format; use {!Stable.V3} where a UK, US or local destination or an address can occur. *)
include Binable.S with type t := t

(** Parses the destination for a rail: an IBAN for SEPA rails, an address on the rail's
    network for on-chain, an invoice, LNURL or address for Lightning, an account id for
    intraledger, a sort code and account number or a GB IBAN for the UK rails
    ({!Uk_account.of_string}, checked against [modulus] when given, with [roll_number]), a
    routing number, account number and optional type for the US rails
    ({!Us_account.of_string}). [postal_address] is validated
    ({!Iso20022.Postal_address.validate}). New beneficiaries start untrusted. *)
val create
  :  id:Id.t
  -> owner:string
  -> name:string
  -> rail:Rail.t
  -> ?bic:string
  -> ?memo:string
  -> ?roll_number:string
  -> ?modulus:Uk_modulus.Table.t
  -> ?postal_address:Postal_address.t
  -> now:Time_ns.t
  -> string
  -> t Or_error.t

(** A beneficiary with a {!Destination.Local} destination, checked by
    {!Destination.create_local}:

    {[
      Beneficiary.create_local
        ~id
        ~owner
        ~name:"Aibek Toktogulov"
        ~now
        ~scheme:Beneficiary.Destination.Local_scheme.kg
        [ "bank_code", "109018"; "account_number", "1090 0001 2345 6789" ]
    ]} *)
val create_local
  :  id:Id.t
  -> owner:string
  -> name:string
  -> ?formats:Destination.Local_format.t list
  -> ?postal_address:Postal_address.t
  -> now:Time_ns.t
  -> scheme:string
  -> (string * string) list
  -> t Or_error.t

(** Sets or clears the address, validating a new one. *)
val with_postal_address : t -> Postal_address.t option -> t Or_error.t

val usable_on : t -> Rail.t -> bool

(** [Fedwire] and [Chaps]: the wire rails, where the receiving bank commonly asks for the
    beneficiary's address. *)
val default_address_required : Rail.t -> bool

(** Whether a payment on [rail] can go to this beneficiary: {!usable_on}, and a postal
    address when [address_required rail] (default {!default_address_required}); each
    provider and corridor has its own rule, so pass yours. *)
val check_for_rail : ?address_required:(Rail.t -> bool) -> t -> Rail.t -> unit Or_error.t

val trust : t -> t
val untrust : t -> t
val with_payee_check : t -> Payee_check.t -> t

(** Younger than [cooling_off] at [now]. *)
val is_new : t -> now:Time_ns.t -> cooling_off:Time_ns.Span.t -> bool
