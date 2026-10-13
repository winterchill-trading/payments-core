(** When an incoming on-chain deposit counts as settled.

    Bitcoin has only probabilistic finality: credit after 3 confirmations, 6 above
    10 000 EUR. Ethereum-style chains finalize checkpoints (about 15 minutes on
    Ethereum): credit at the [finalized] block tag. Tron and Solana use a fixed
    confirmation count here. *)

open! Core

type t =
  | Confirmations of int
  | Finalized
[@@deriving sexp, compare, equal]

(** [eur_value] unknown is treated as large. *)
val required : Network.t -> eur_value:Money.Amount.t option -> t

val is_met : t -> confirmations:int -> finalized:bool -> bool
