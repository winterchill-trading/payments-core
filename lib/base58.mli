(** Base58 with the Bitcoin alphabet, and Base58Check (payload plus the first four
    bytes of a double SHA-256). Used for legacy Bitcoin, Tron and Solana addresses. *)

open! Core

val encode : string -> string
val decode : string -> string option

(** [encode_check payload] appends the checksum and encodes. *)
val encode_check : string -> string

(** Decodes and verifies the checksum, returning the payload without it. *)
val decode_check : string -> string option
