open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Bitcoin
      | Bitcoin_testnet
      | Ethereum
      | Polygon
      | Tron
      | Solana
    [@@deriving bin_io, sexp, compare, equal, hash, quickcheck, enumerate]
  end
end

include Stable.V1

let to_string t = Sexp.to_string (sexp_of_t t) |> String.lowercase

let of_string s =
  match List.find all ~f:(fun t -> String.equal (to_string t) (String.lowercase s)) with
  | Some t -> t
  | None -> raise_s [%message "unknown network" s]
;;

let is_evm = function
  | Ethereum | Polygon -> true
  | Bitcoin | Bitcoin_testnet | Tron | Solana -> false
;;
