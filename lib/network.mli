(** Blockchains a crypto beneficiary can live on. Each constructor fixes the address
    format accepted by {!Crypto_address}. *)

open! Core

module Stable : sig
  module V2 : sig
    type t =
      | Bitcoin
      | Bitcoin_testnet (** testnet and signet share address prefixes *)
      | Ethereum
      | Polygon
      | Tron
      | Solana
      | Base (** Ethereum layer 2, EVM addresses *)
      | Arbitrum (** Ethereum layer 2 (Arbitrum One), EVM addresses *)
      | Plasma (** stablecoin chain, EVM addresses *)
      | Spark (** Bitcoin layer 2, Bech32m addresses *)
    [@@deriving bin_io, sexp, compare, equal, hash, quickcheck, enumerate]
  end

  module V1 : sig
    (** The first six networks' format (digest [180a350e…]); writing a newer network
        raises. Its generator and [all] cover the six only. *)
    type t = V2.t [@@deriving bin_io, sexp, compare, equal, hash, quickcheck, enumerate]
  end
end

type t = Stable.V2.t =
  | Bitcoin
  | Bitcoin_testnet
  | Ethereum
  | Polygon
  | Tron
  | Solana
  | Base
  | Arbitrum
  | Plasma
  | Spark
[@@deriving sexp, compare, equal, hash, quickcheck, enumerate]

(** [bin_io] on [t] is {!Stable.V1}, so records that embed a network keep their format;
    use {!Stable.V2} where a newer network can occur. *)
include Binable.S with type t := t

include Stringable.S with type t := t

(** EVM networks share one address format (20 bytes, EIP-55 checksum). *)
val is_evm : t -> bool
