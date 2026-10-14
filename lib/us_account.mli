(** A US bank account: ABA routing number, account number and account type, which is what
    ACH, RTP and Fedwire address. Account numbers have no national format; banks use 4 to
    17 digits. *)

open! Core

module Account_type : sig
  type t =
    | Checking
    | Savings
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate, quickcheck]
end

module Stable : sig
  module V1 : sig
    type t = private
      { routing : Aba.Stable.V1.t
      ; account_number : string (** 4 to 17 digits *)
      ; account_type : Account_type.t
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

type t = Stable.V1.t = private
  { routing : Aba.t
  ; account_number : string
  ; account_type : Account_type.t
  }
[@@deriving bin_io, sexp, compare, equal, hash, quickcheck]

(** [account_type] defaults to [Checking]. *)
val create
  :  ?account_type:Account_type.t
  -> routing:Aba.t
  -> account_number:string
  -> unit
  -> t Or_error.t

(** [<routing> <account number> [checking|savings]], separated by spaces or slashes
    ([021000021 123456789 savings], [021000021/123456789]). *)
val of_string : string -> t Or_error.t

(** [021000021 123456789 checking]. *)
val to_string : t -> string
