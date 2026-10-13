open! Core

module Stable = struct
  module V1 = struct
    type t = Time_ns.Stable.V1.t [@@deriving bin_io, compare, equal]

    let hash = Time_ns.hash
    let hash_fold_t = Time_ns.hash_fold_t
    let sexp_of_t = Time_ns.Alternate_sexp.sexp_of_t
    let t_of_sexp = Time_ns.Alternate_sexp.t_of_sexp
  end
end

include Stable.V1
