(** The ISO 20022 (pain.001 / pacs.008 / pacs.002 / pacs.004 / camt.056) fields a core
    banking system keeps for each credit transfer, with the SEPA restrictions on them. *)

open! Core

module Sepa_text : sig
  (** The SEPA (EPC) basic Latin character set: [a-z A-Z 0-9 / - ? : ( ) . , ' +] and
      space. Banks may accept more, but anything outside this set can be rejected or
      transliterated on the way, so the core refuses it. *)
  val is_valid : string -> bool

  (** Fails on an empty string, a string over [max_length], or characters outside the
      set. *)
  val check : field:string -> max_length:int -> string -> unit Or_error.t

  (** Identifiers additionally must not start or end with ['/'] or contain ["//"]. *)
  val check_id : field:string -> string -> unit Or_error.t
end

module Uetr : sig
  (** Unique end-to-end transaction reference: a lower-case UUID version 4. *)
  type t [@@deriving bin_io, sexp, compare, equal, hash]

  val of_string : string -> t Or_error.t
  val to_string : t -> string
  val create : Random.State.t -> t
end

module Payment_id : sig
  type t =
    { end_to_end_id : string
    (** set by the originator, carried unchanged to the beneficiary; 1-35 SEPA
        characters; ["NOTPROVIDED"] when the customer gave none *)
    ; instruction_id : string option (** point-to-point id between two agents *)
    ; uetr : Uetr.t option
    ; tx_id : string option (** interbank transaction id, from the provider *)
    ; msg_id : string option (** message id of the pain.001 / pacs.008 *)
    ; pmt_inf_id : string option (** payment information id of the pain.001 batch *)
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

    val create
    :  end_to_end_id:string
    -> ?instruction_id:string
    -> ?uetr:Uetr.t
    -> ?tx_id:string
    -> ?msg_id:string
    -> ?pmt_inf_id:string
    -> unit
    -> t Or_error.t
  val not_provided : string

  (** Our own identifiers for a new transfer: an end-to-end id derived from
      [transfer_id] (characters outside the SEPA set become ['-'], slashes are made
      legal, at most 35 characters) and a fresh UETR. *)
  val generate : transfer_id:string -> Random.State.t -> t
end

module Creditor_reference : sig
  (** ISO 11649 structured creditor reference: [RF], two check digits (mod 97 as in
      IBANs) and up to 21 alphanumerics, e.g. [RF18 5390 0754 7034]. *)
  type t [@@deriving bin_io, sexp, compare, equal, hash]

  val of_string : string -> t Or_error.t
  val to_string : t -> string

  (** Adds the [RF] prefix and check digits to a reference body. *)
  val of_body : string -> t Or_error.t
end

module Remittance : sig
  (** SEPA carries either one of these, never both. *)
  type t =
    | Unstructured of string (** up to 140 SEPA characters *)
    | Structured of Creditor_reference.t
    | None_given
  [@@deriving bin_io, sexp, compare, equal, hash]

  val unstructured : string -> t Or_error.t
end

module Postal_address : sig
  (** Structured address. From November 2026 SEPA messages may no longer carry a fully
      unstructured address: town and country are mandatory, address lines are only
      allowed alongside them ("hybrid"). *)
  type t =
    { country : string (** ISO 3166 alpha-2 *)
    ; town : string
    ; post_code : string option
    ; street : string option
    ; building_number : string option
    ; lines : string list (** at most two, hybrid form *)
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  val validate : t -> unit Or_error.t
end

module Party : sig
  (** Debtor or creditor of a SEPA transfer. *)
  type t =
    { name : string (** up to 70 SEPA characters *)
    ; iban : Iban.Stable.V1.t
    ; bic : Bic.Stable.V1.t option
    ; address : Postal_address.t option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  val validate : t -> unit Or_error.t
end

module Charge_bearer : sig
  type t =
    | SLEV (** following the service level: mandatory in SEPA *)
    | DEBT
    | CRED
    | SHAR
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
end

module Transaction_status : sig
  (** pacs.002 / pain.002 transaction status codes. *)
  type t =
    | RCVD (** received *)
    | ACTC (** accepted after technical validation *)
    | ACCP (** accepted after customer profile checks *)
    | ACSP (** accepted, settlement in process *)
    | ACWC (** accepted with change *)
    | PDNG (** pending further checks *)
    | ACSC (** settlement completed on the debtor side *)
    | ACCC (** settlement completed, creditor account credited *)
    | RJCT (** rejected *)
    | CANC (** cancelled *)
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]

  val of_string : string -> t Or_error.t
end
