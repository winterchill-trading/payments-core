(** Strong customer authentication challenges.

    A challenge is bound to what it authorises ({!Binding}: amount and payee, the PSD2
    "dynamic linking" requirement), expires, and locks after too many wrong answers.
    How a response is checked is abstract ({!Verifier}): an OTP store, a TOTP secret, a
    WebAuthn assertion check, or a provider's API. *)

open! Core
open Async_kernel

module Kind : sig
  type t =
    | Otp (** one-time code sent by SMS or e-mail *)
    | Totp (** authenticator app, RFC 6238 *)
    | Passkey (** WebAuthn assertion *)
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
end

module Id : Opaque_id.S

module Binding : sig
  (** Hex SHA-256 over the authorised operation, so a code approved for one amount and
      payee cannot authorise another. *)
  type t [@@deriving bin_io, sexp, compare, equal, hash]

  val create : operation:string -> amount:string -> payee:string -> t
  val to_string : t -> string
end

module Failure : sig
  type t =
    | Expired
    | Too_many_attempts
    | Cancelled
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Status : sig
  type t =
    | Pending
    | Verified of { at : Utc_time.Stable.V1.t }
    | Failed of Failure.t
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Challenge : sig
  module Stable : sig
    module V1 : sig
      type t =
        { id : Id.t
        ; kind : Kind.t
        ; subject : string (** customer id *)
        ; binding : Binding.t
        ; created_at : Utc_time.Stable.V1.t
        ; expires_at : Utc_time.Stable.V1.t
        ; attempts : int (** wrong answers so far *)
        ; max_attempts : int
        ; status : Status.t
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    { id : Id.t
    ; kind : Kind.t
    ; subject : string
    ; binding : Binding.t
    ; created_at : Time_ns.t
    ; expires_at : Time_ns.t
    ; attempts : int
    ; max_attempts : int
    ; status : Status.t
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  (** Defaults: five minutes to answer, three wrong answers. The regulatory ceilings are
      five minutes of inactivity and five failed attempts (RTS articles 4 and 5). *)
  val create
    :  id:Id.t
    -> kind:Kind.t
    -> subject:string
    -> binding:Binding.t
    -> now:Time_ns.t
    -> ?ttl:Time_ns.Span.t
    -> ?max_attempts:int
    -> unit
    -> t

  val is_open : t -> now:Time_ns.t -> bool
  val cancel : t -> t

  (** Marks a pending challenge expired once [now] passes [expires_at]. *)
  val expire : t -> now:Time_ns.t -> t
end

module Outcome : sig
  type t =
    | Verified
    | Wrong_response of { remaining : int }
    | Rejected of Failure.t (** expired, locked or cancelled, now or before *)
    | Already_verified
    | Binding_mismatch
  [@@deriving sexp, compare, equal]
end

(** Applies one checked response. Pure; [verify] wraps it with a {!Verifier}. *)
val record : Challenge.t -> now:Time_ns.t -> correct:bool -> Challenge.t * Outcome.t

module type Verifier = sig
  val check : Challenge.t -> response:string -> bool Deferred.t
end

(** Checks [response] for the operation identified by [binding]. The verifier is not
    called when the challenge is closed or bound to a different operation. *)
val verify
  :  (module Verifier)
  -> Challenge.t
  -> binding:Binding.t
  -> response:string
  -> now:Time_ns.t
  -> (Challenge.t * Outcome.t) Deferred.t

module Totp : sig
  (** RFC 6238 time-based codes with HMAC-SHA1 and a 30-second step. *)
  val code : secret:string -> time:Time_ns.t -> digits:int -> string

  (** Accepts the code for the current step and [window] steps either side, which
      covers phone clocks a little off. *)
  val check : secret:string -> time:Time_ns.t -> ?window:int -> string -> bool
end
