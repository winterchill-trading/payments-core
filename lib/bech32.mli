(** Bech32 (BIP 173) and Bech32m (BIP 350) strings: SegWit addresses, Lightning invoices
    (BOLT 11) and LNURLs. Data is a list of 5-bit values. *)

open! Core

module Variant : sig
  type t =
    | Bech32
    | Bech32m
  [@@deriving sexp, compare, equal]
end

type decoded =
  { hrp : string (** human-readable part, lower case *)
  ; data : int list (** 5-bit values without the 6-value checksum *)
  ; variant : Variant.t
  }
[@@deriving sexp, compare, equal]

val encode : hrp:string -> data:int list -> Variant.t -> string

(** [max_length] defaults to 90 (BIP 173); Lightning invoices and LNURLs are longer and
    pass [Int.max_value]. Mixed case is rejected. *)
val decode : ?max_length:int -> string -> decoded option

(** Regroups bits, e.g. 8-bit bytes to 5-bit values and back. With [pad:false], leftover
    bits must be zero padding of fewer than [from] bits. *)
val convert_bits : int list -> from:int -> to_:int -> pad:bool -> int list option

(** SegWit address for [hrp] ([bc], [tb], [bcrt]): witness version and program bytes. *)
val decode_segwit : hrp:string -> string -> (int * string) option

val encode_segwit : hrp:string -> version:int -> program:string -> string
