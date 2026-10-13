(** A blockchain address validated for one {!Network}.

    - Bitcoin: Base58Check P2PKH/P2SH and SegWit v0 (Bech32) / v1+ (Bech32m, Taproot).
    - EVM: [0x] plus 40 hex digits; mixed case must carry a valid EIP-55 checksum.
    - Tron: Base58Check of 21 bytes starting with [0x41] (the [T...] form).
    - Solana: Base58 of exactly 32 bytes (no checksum exists in the format). *)

open! Core

type t [@@deriving bin_io, sexp, compare, equal, hash]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

val create : Network.t -> string -> t Or_error.t
val create_exn : Network.t -> string -> t
val network : t -> Network.t

(** Canonical form: lower-case Bech32, EIP-55 checksummed hex, Base58 as given. Two
    spellings of one address compare equal. *)
val address : t -> string

(** EIP-55 checksummed spelling of a 20-byte hex address (with or without [0x]). *)
val eip55 : string -> string option
