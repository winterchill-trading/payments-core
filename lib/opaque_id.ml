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

module Make (M : sig
    val module_name : string
  end)
    () =
struct
  module T = struct
    type t = string [@@deriving bin_io, sexp, compare, equal, hash]

    let of_string s = s
    let to_string t = t
    let module_name = M.module_name
  end

  include T
  include Identifiable.Make (T)

  let quickcheck_generator =
    Quickcheck.Generator.map (Int.gen_incl 0 99_999) ~f:(sprintf "id-%05d")
  ;;

  let quickcheck_observer = String.quickcheck_observer
  let quickcheck_shrinker = String.quickcheck_shrinker

  module Stable = struct
    module V1 = T
  end
end
