(** Opaque string identifiers; each [Make ()] gives a type that does not unify with the
    others, so a transfer id cannot be passed where a beneficiary id is expected. *)

open! Core

module type S = sig
  type t [@@deriving bin_io, sexp, compare, equal, hash, quickcheck]

  include Identifiable.S with type t := t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end
end

module Make
    (_ : sig
       val module_name : string
     end)
    () : S
