open! Core

module Stable = struct
  module V2 = struct
    type t =
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
    [@@deriving bin_io, sexp, compare, equal, hash, quickcheck, enumerate]
  end

  module V1 = struct
    (* The original six networks: the bin_prot shape and digest come from here. *)
    module Repr = struct
      type t =
        | Bitcoin
        | Bitcoin_testnet
        | Ethereum
        | Polygon
        | Tron
        | Solana
      [@@deriving bin_io, quickcheck, enumerate]
    end

    type t = V2.t [@@deriving sexp, compare, equal, hash]

    let of_repr : Repr.t -> t = function
      | Bitcoin -> Bitcoin
      | Bitcoin_testnet -> Bitcoin_testnet
      | Ethereum -> Ethereum
      | Polygon -> Polygon
      | Tron -> Tron
      | Solana -> Solana
    ;;

    let to_repr : t -> Repr.t = function
      | Bitcoin -> Bitcoin
      | Bitcoin_testnet -> Bitcoin_testnet
      | Ethereum -> Ethereum
      | Polygon -> Polygon
      | Tron -> Tron
      | Solana -> Solana
      | (Base | Arbitrum | Plasma | Spark) as network ->
        raise_s
          [%message
            "Network.Stable.V1 cannot hold this network; write Network.Stable.V2"
              (network : V2.t)]
    ;;

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr)
        (struct
          type nonrec t = t

          let to_binable = to_repr
          let of_binable = of_repr
        end)

    let all = List.map Repr.all ~f:of_repr

    let quickcheck_generator =
      Quickcheck.Generator.map Repr.quickcheck_generator ~f:of_repr
    ;;

    let quickcheck_observer = V2.quickcheck_observer
    let quickcheck_shrinker : t Quickcheck.Shrinker.t = Quickcheck.Shrinker.empty ()
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

include (Stable.V1 : Binable.S with type t := t)

let to_string t = Sexp.to_string (sexp_of_t t) |> String.lowercase

let of_string s =
  match List.find all ~f:(fun t -> String.equal (to_string t) (String.lowercase s)) with
  | Some t -> t
  | None -> raise_s [%message "unknown network" s]
;;

let is_evm = function
  | Ethereum | Polygon | Base | Arbitrum | Plasma -> true
  | Bitcoin | Bitcoin_testnet | Tron | Solana | Spark -> false
;;
