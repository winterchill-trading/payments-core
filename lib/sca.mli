(** Strong customer authentication (PSD2 art. 97, RTS 2018/389).

    SCA needs two elements from different categories: knowledge, possession, inherence.
    An SMS OTP, an authenticator app code and a passkey are all possession, so a
    challenge pairs one of them with a PIN, password or biometric. A challenge is bound
    to what it authorises ({!Binding}: operation, amount, currency and payee; RTS art. 5
    dynamic linking), is consumed once verified (art. 4(1)), and fails after at most five
    wrong answers (art. 4(3)(b)); a wrong answer never says which factor was wrong
    (art. 4(3)(a)). How a response is checked is abstract ({!Verifier}). When SCA is
    skipped, the {!Exemption} and its RTS article are recorded, since the payer's bank
    carries the loss of an exempted fraudulent payment (PSD2 art. 74(2)). *)

open! Core
open Async_kernel

module Element : sig
  type t =
    | Knowledge
    | Possession
    | Inherence
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
end

module Kind : sig
  type t =
    | Otp (** one-time code sent by SMS or e-mail: possession of the SIM or inbox *)
    | Totp (** authenticator app, RFC 6238: possession *)
    | Passkey (** WebAuthn assertion from a bound device key: possession *)
    | Pin (** knowledge *)
    | Password (** knowledge *)
    | Biometric (** device biometric attestation: inherence *)
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]

  val element : t -> Element.t
end

module Id : Opaque_id.S

module Binding : sig
  (** Hex SHA-256 over [operation|amount in minor units|currency|payee], so a code
      approved for one amount and payee cannot authorise another. *)
  type t [@@deriving bin_io, sexp, compare, equal, hash]

  val create : operation:string -> amount:Money.Amount.t -> payee:string -> t

  (** Batch payments bind the total and the sorted payee list (RTS art. 5(3)(b)). *)
  val create_batch : operation:string -> total:Money.Amount.t -> payees:string list -> t

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

(** The hard ceiling for wrong answers, RTS art. 4(3)(b). *)
val max_attempts_ceiling : int

module Challenge : sig
  module Stable : sig
    module V1 : sig
      type t =
        { id : Id.Stable.V1.t
        ; subject : string (** customer id *)
        ; binding : Binding.t
        ; factors : Kind.t list (** what must pass, two categories at least *)
        ; passed : Kind.t list
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
    ; subject : string
    ; binding : Binding.t
    ; factors : Kind.t list
    ; passed : Kind.t list
    ; created_at : Time_ns.t
    ; expires_at : Time_ns.t
    ; attempts : int
    ; max_attempts : int
    ; status : Status.t
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  (** Fails unless [factors] span two element categories and [max_attempts] is 1 to
      {!max_attempts_ceiling}. Defaults: three attempts; five minutes to answer, which is
      our choice (the RTS sets no lifetime for an unanswered challenge). *)
  val create
    :  id:Id.t
    -> subject:string
    -> binding:Binding.t
    -> factors:Kind.t list
    -> now:Time_ns.t
    -> ?ttl:Time_ns.Span.t
    -> ?max_attempts:int
    -> unit
    -> t Or_error.t

  val is_open : t -> now:Time_ns.t -> bool
  val cancel : t -> t

  (** Marks a pending challenge expired once [now] passes [expires_at]. *)
  val expire : t -> now:Time_ns.t -> t

  (** Element categories of the factors that passed. *)
  val elements_passed : t -> Element.t list
end

module Outcome : sig
  type t =
    | Verified
    | Factor_accepted of { remaining : Kind.t list }
    | Wrong_response of { attempts_left : int } (** never names the factor *)
    | Rejected of Failure.t (** expired, locked or cancelled, now or before *)
    | Already_verified (** a verified challenge is consumed and authorises nothing more *)
    | Binding_mismatch
    | Not_a_factor of Kind.t (** the challenge did not ask for this kind *)
  [@@deriving sexp, compare, equal]
end

(** Applies one checked response for one factor. Pure; [verify] wraps it with a
    {!Verifier}. *)
val record
  :  Challenge.t
  -> now:Time_ns.t
  -> kind:Kind.t
  -> correct:bool
  -> Challenge.t * Outcome.t

module type Verifier = sig
  val check : Challenge.t -> kind:Kind.t -> response:string -> bool Deferred.t
end

(** Checks [response] for factor [kind] of the operation identified by [binding]. The
    verifier is not called when the challenge is closed, bound to a different operation
    or did not ask for [kind]. *)
val verify
  :  (module Verifier)
  -> Challenge.t
  -> binding:Binding.t
  -> kind:Kind.t
  -> response:string
  -> now:Time_ns.t
  -> (Challenge.t * Outcome.t) Deferred.t

module Lock : sig
  (** Per-customer blocking across challenges (RTS art. 4(3)(b) and 4(4)): five
      consecutive failures lock for 30 minutes; a second temporary lock within 24 hours
      is permanent until a secure re-enablement. The customer is alerted when the first
      temporary lock starts, before any permanent block. *)
  type status =
    | Unlocked
    | Temporary of { until : Utc_time.Stable.V1.t }
    | Permanent
  [@@deriving bin_io, sexp, compare, equal]

  type t =
    { consecutive_failures : int
    ; temporary_locks : Utc_time.Stable.V1.t list (** start times, newest first *)
    ; status : status
    }
  [@@deriving bin_io, sexp, compare, equal]

  module Notice : sig
    type t =
      | Locked_temporarily_next_is_permanent
      | Locked_permanently
    [@@deriving sexp, compare, equal]
  end

  val empty : t
  val is_locked : t -> now:Time_ns.t -> bool
  val record_failure : t -> now:Time_ns.t -> t * Notice.t option
  val record_success : t -> t

  (** Clears a permanent lock after the secure re-enablement procedure. *)
  val reenable : t -> t
end

module Session : sig
  (** Online access session after SCA; it ends after 5 minutes without activity
      (RTS art. 4(3)(d)). *)
  type t =
    { subject : string
    ; authenticated_at : Utc_time.Stable.V1.t
    ; last_activity : Utc_time.Stable.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal]

  val idle_timeout : Time_ns.Span.t
  val start : subject:string -> now:Time_ns.t -> t
  val is_active : t -> now:Time_ns.t -> bool

  (** Records activity; fails if the session already timed out. *)
  val touch : t -> now:Time_ns.t -> t Or_error.t
end

module Exemption : sig
  module Etv : sig
    (** Exemption threshold values for transaction risk analysis (RTS annex). *)
    type t =
      | Eur_100
      | Eur_250
      | Eur_500
    [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
  end

  type t =
    | Account_info_direct
    | Account_info_aisp
    | Contactless
    | Unattended_transport_parking
    | Trusted_beneficiary
    | Recurring_same_amount_payee
    | Own_accounts
    | Low_value_remote
    | Corporate_process
    | Tra of
        { band : Etv.t
        ; score : int
        }
  [@@deriving bin_io, sexp, compare, equal, hash]

  (** RTS article, e.g. ["Art 16"]. *)
  val article : t -> string
end

module Authorization : sig
  (** How an outgoing payment was authorised: kept on the transfer for the liability
      trail. *)
  type t =
    | Sca of Id.Stable.V1.t
    | Exempt of Exemption.t
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Counters : sig
  (** Payments since the customer's last SCA, per channel, for the cumulative limits of
      the low-value remote (art. 16: 30 EUR each, 100 EUR or 5 payments) and contactless
      (art. 11: 50 EUR each, 150 EUR or 5 payments) exemptions. Amounts in EUR. *)
  type t =
    { remote_count : int
    ; remote_sum : Money.Amount.t
    ; contactless_count : int
    ; contactless_sum : Money.Amount.t
    }
  [@@deriving sexp, compare, equal]

  val zero : t

  (** Whether a new payment of [amount] still fits the exemption. *)
  val low_value_remote_ok : t -> amount:Money.Amount.t -> bool

  val contactless_ok : t -> amount:Money.Amount.t -> bool
  val record_remote : t -> amount:Money.Amount.t -> t
  val record_contactless : t -> amount:Money.Amount.t -> t

  (** After every successful SCA. *)
  val reset : t
end

module Event : sig
  (** One audit record per challenge transition. Codes, PINs and passwords are never
      part of it. *)
  type t =
    { challenge : Id.Stable.V1.t
    ; subject : string
    ; operation : string
    ; amount : Money.Amount.Stable.V1.t option
    ; payee : string option
    ; binding : Binding.t
    ; kind : Kind.t option (** the factor answered, if any *)
    ; elements_passed : Element.t list
    ; channel : string
    ; device_id : string option
    ; at : Utc_time.Stable.V1.t
    ; attempt : int
    ; result : string (** the {!Outcome.t} constructor *)
    ; lock : Lock.status
    ; exemption : Exemption.t option
    }
  [@@deriving bin_io, sexp, compare, equal]

  val of_outcome
    :  Challenge.t
    -> Outcome.t
    -> operation:string
    -> ?amount:Money.Amount.t
    -> ?payee:string
    -> ?kind:Kind.t
    -> channel:string
    -> ?device_id:string
    -> lock:Lock.status
    -> at:Time_ns.t
    -> unit
    -> t
end

module Totp : sig
  (** RFC 6238 time-based codes with HMAC-SHA1 and a 30-second step. *)
  val code : secret:string -> time:Time_ns.t -> digits:int -> string

  (** Accepts the code for the current step and [window] steps either side, which
      covers phone clocks a little off. *)
  val check : secret:string -> time:Time_ns.t -> ?window:int -> string -> bool
end
