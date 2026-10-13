(** Blockchains a crypto beneficiary can live on. Each constructor fixes the address
    format accepted by {!Crypto_address}. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Bitcoin
      | Bitcoin_testnet (** testnet and signet share address prefixes *)
      | Ethereum
      | Polygon
      | Tron
      | Solana
    [@@deriving bin_io, sexp, compare, equal, hash, quickcheck, enumerate]
  end
end

type t = Stable.V1.t =
  | Bitcoin
  | Bitcoin_testnet
  | Ethereum
  | Polygon
  | Tron
  | Solana
[@@deriving bin_io, sexp, compare, equal, hash, quickcheck, enumerate]

include Stringable.S with type t := t

(** EVM networks share one address format (20 bytes, EIP-55 checksum). *)
val is_evm : t -> bool
