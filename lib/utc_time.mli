(** [Time_ns.t] whose sexp is always UTC ([2026-10-13 09:00:00Z]).

    [Time_ns.Stable.V1] renders sexps in the machine's local zone, so the same value
    prints differently on a laptop and in CI. Binary form is [Time_ns.Stable.V1]. *)

open! Core

type t = Time_ns.t [@@deriving bin_io, sexp, compare, equal, hash]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
