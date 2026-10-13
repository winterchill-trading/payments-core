open! Core
open Async_kernel

module Element = struct
  type t =
    | Knowledge
    | Possession
    | Inherence
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
end

module Kind = struct
  type t =
    | Otp
    | Totp
    | Passkey
    | Pin
    | Password
    | Biometric
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]

  (* EBA Q&A 2018_4039: the SIM receiving an SMS OTP is a possession element. *)
  let element : t -> Element.t = function
    | Otp | Totp | Passkey -> Possession
    | Pin | Password -> Knowledge
    | Biometric -> Inherence
  ;;
end

module Id =
  Opaque_id.Make
    (struct
      let module_name = "Payments_core.Sca.Id"
    end)
    ()

let hex s = String.concat_map s ~f:(fun c -> sprintf "%02x" (Char.to_int c))

module Binding = struct
  type t = string [@@deriving bin_io, sexp, compare, equal, hash]

  let digest fields =
    (* Length-prefixed fields so "ab"+"c" and "a"+"bc" hash differently. *)
    let field s = sprintf "%d:%s|" (String.length s) s in
    Cryptokit.hash_string
      (Cryptokit.Hash.sha256 ())
      (String.concat (List.map fields ~f:field))
    |> hex
  ;;

  let amount_fields a =
    [ Z.to_string (Money.Amount.minor_units a)
    ; Money.Currency.to_string (Money.Amount.currency a)
    ]
  ;;

  let create ~operation ~amount ~payee =
    digest ((operation :: amount_fields amount) @ [ payee ])
  ;;

  let create_batch ~operation ~total ~payees =
    digest ((operation :: amount_fields total) @ List.sort payees ~compare:String.compare)
  ;;

  let to_string t = t
end

module Failure = struct
  type t =
    | Expired
    | Too_many_attempts
    | Cancelled
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Status = struct
  type t =
    | Pending
    | Verified of { at : Utc_time.Stable.V1.t }
    | Failed of Failure.t
  [@@deriving bin_io, sexp, compare, equal, hash]
end

let max_attempts_ceiling = 5

let elements kinds =
  List.map kinds ~f:Kind.element |> List.dedup_and_sort ~compare:Element.compare
;;

module Challenge = struct
  module Stable = struct
    module V1 = struct
      type t =
        { id : Id.Stable.V1.t
        ; subject : string
        ; binding : Binding.t
        ; factors : Kind.t list
        ; passed : Kind.t list
        ; created_at : Utc_time.Stable.V1.t
        ; expires_at : Utc_time.Stable.V1.t
        ; attempts : int
        ; max_attempts : int
        ; status : Status.t
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  include Stable.V1

  let create
    ~id
    ~subject
    ~binding
    ~factors
    ~now
    ?(ttl = Time_ns.Span.of_min 5.)
    ?(max_attempts = 3)
    ()
    =
    let factors = List.dedup_and_sort factors ~compare:Kind.compare in
    if List.length (elements factors) < 2
    then
      Or_error.error_s
        [%message "SCA needs factors from two element categories" (factors : Kind.t list)]
    else if max_attempts < 1 || max_attempts > max_attempts_ceiling
    then Or_error.error_s [%message "max_attempts must be 1..5" (max_attempts : int)]
    else
      Ok
        { id
        ; subject
        ; binding
        ; factors
        ; passed = []
        ; created_at = now
        ; expires_at = Time_ns.add now ttl
        ; attempts = 0
        ; max_attempts
        ; status = Pending
        }
  ;;

  let expire t ~now =
    match t.status with
    | Pending when Time_ns.( >= ) now t.expires_at -> { t with status = Failed Expired }
    | _ -> t
  ;;

  let is_open t ~now =
    match (expire t ~now).status with
    | Pending -> true
    | Verified _ | Failed _ -> false
  ;;

  let cancel t =
    match t.status with
    | Pending -> { t with status = Failed Cancelled }
    | Verified _ | Failed _ -> t
  ;;

  let elements_passed t = elements t.passed
end

module Outcome = struct
  type t =
    | Verified
    | Factor_accepted of { remaining : Kind.t list }
    | Wrong_response of { attempts_left : int }
    | Rejected of Failure.t
    | Already_verified
    | Binding_mismatch
    | Not_a_factor of Kind.t
  [@@deriving sexp, compare, equal]
end

let record (c : Challenge.t) ~now ~kind ~correct =
  let c = Challenge.expire c ~now in
  match c.status with
  | Verified _ -> c, Outcome.Already_verified
  | Failed f -> c, Rejected f
  | Pending ->
    if not (List.mem c.factors kind ~equal:Kind.equal)
    then c, Not_a_factor kind
    else if correct
    then (
      let passed = List.dedup_and_sort (kind :: c.passed) ~compare:Kind.compare in
      let remaining =
        List.filter c.factors ~f:(fun k -> not (List.mem passed k ~equal:Kind.equal))
      in
      if List.is_empty remaining
      then { c with passed; status = Verified { at = now } }, Verified
      else { c with passed }, Factor_accepted { remaining })
    else (
      let attempts = c.attempts + 1 in
      if attempts >= c.max_attempts
      then
        { c with attempts; status = Failed Too_many_attempts }, Rejected Too_many_attempts
      else
        { c with attempts }, Wrong_response { attempts_left = c.max_attempts - attempts })
;;

module type Verifier = sig
  val check : Challenge.t -> kind:Kind.t -> response:string -> bool Deferred.t
end

let verify (module V : Verifier) (c : Challenge.t) ~binding ~kind ~response ~now =
  if not (Binding.equal binding c.binding)
  then return (c, Outcome.Binding_mismatch)
  else if (not (Challenge.is_open c ~now))
          || not (List.mem c.factors kind ~equal:Kind.equal)
  then return (record c ~now ~kind ~correct:false)
  else (
    let%map correct = V.check c ~kind ~response in
    record c ~now ~kind ~correct)
;;

module Lock = struct
  type status =
    | Unlocked
    | Temporary of { until : Utc_time.Stable.V1.t }
    | Permanent
  [@@deriving bin_io, sexp, compare, equal]

  type t =
    { consecutive_failures : int
    ; temporary_locks : Utc_time.Stable.V1.t list
    ; status : status
    }
  [@@deriving bin_io, sexp, compare, equal]

  module Notice = struct
    type t =
      | Locked_temporarily_next_is_permanent
      | Locked_permanently
    [@@deriving sexp, compare, equal]
  end

  let failures_before_lock = 5
  let temporary_lock = Time_ns.Span.of_min 30.
  let permanent_window = Time_ns.Span.of_day 1.
  let empty = { consecutive_failures = 0; temporary_locks = []; status = Unlocked }

  let is_locked t ~now =
    match t.status with
    | Unlocked -> false
    | Temporary { until } -> Time_ns.( < ) now until
    | Permanent -> true
  ;;

  let record_failure t ~now =
    match t.status with
    | Permanent -> t, None
    | Temporary _ when is_locked t ~now -> t, None
    | Unlocked | Temporary _ ->
      let consecutive_failures = t.consecutive_failures + 1 in
      if consecutive_failures < failures_before_lock
      then { t with consecutive_failures; status = Unlocked }, None
      else (
        let recent =
          List.filter t.temporary_locks ~f:(fun at ->
            Time_ns.Span.( < ) (Time_ns.diff now at) permanent_window)
        in
        if List.is_empty recent
        then
          ( { consecutive_failures = 0
            ; temporary_locks = now :: t.temporary_locks
            ; status = Temporary { until = Time_ns.add now temporary_lock }
            }
          , Some Notice.Locked_temporarily_next_is_permanent )
        else
          ( { consecutive_failures = 0
            ; temporary_locks = t.temporary_locks
            ; status = Permanent
            }
          , Some Locked_permanently ))
  ;;

  let record_success t =
    match t.status with
    | Permanent -> t
    | Unlocked | Temporary _ -> { t with consecutive_failures = 0 }
  ;;

  let reenable _ = empty
end

module Session = struct
  type t =
    { subject : string
    ; authenticated_at : Utc_time.Stable.V1.t
    ; last_activity : Utc_time.Stable.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal]

  let idle_timeout = Time_ns.Span.of_min 5.
  let start ~subject ~now = { subject; authenticated_at = now; last_activity = now }

  let is_active t ~now =
    Time_ns.Span.( <= ) (Time_ns.diff now t.last_activity) idle_timeout
  ;;

  let touch t ~now =
    if is_active t ~now
    then Ok { t with last_activity = now }
    else Or_error.error_string "session expired after 5 minutes of inactivity"
  ;;
end

module Exemption = struct
  module Etv = struct
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

  let article = function
    | Account_info_direct -> "Art 10"
    | Account_info_aisp -> "Art 10a"
    | Contactless -> "Art 11"
    | Unattended_transport_parking -> "Art 12"
    | Trusted_beneficiary -> "Art 13"
    | Recurring_same_amount_payee -> "Art 14"
    | Own_accounts -> "Art 15"
    | Low_value_remote -> "Art 16"
    | Corporate_process -> "Art 17"
    | Tra _ -> "Art 18"
  ;;
end

module Authorization = struct
  type t =
    | Sca of Id.Stable.V1.t
    | Exempt of Exemption.t
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Counters = struct
  type t =
    { remote_count : int
    ; remote_sum : Money.Amount.t
    ; contactless_count : int
    ; contactless_sum : Money.Amount.t
    }
  [@@deriving sexp, compare, equal]

  let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn
  let zero_eur = Money.Amount.zero Money.Currency.eur

  let zero =
    { remote_count = 0
    ; remote_sum = zero_eur
    ; contactless_count = 0
    ; contactless_sum = zero_eur
    }
  ;;

  let reset = zero

  let leq a b =
    Money.Amount.compare_value a b
    |> Result.ok
    |> Option.value_map ~default:false ~f:(fun c -> c <= 0)
  ;;

  (* Art 16 and art 11: the single payment cap must hold, and either the cumulative sum or
     the count since the last SCA; the PSP may pick either, we require the sum and the
     count both, the stricter reading. *)
  let ok ~single ~cumulative ~count ~sum ~n ~amount =
    leq amount (eur single)
    && leq (Money.Amount.add_exn sum amount) (eur cumulative)
    && n + 1 <= count
  ;;

  let low_value_remote_ok t ~amount =
    ok ~single:"30" ~cumulative:"100" ~count:5 ~sum:t.remote_sum ~n:t.remote_count ~amount
  ;;

  let contactless_ok t ~amount =
    ok
      ~single:"50"
      ~cumulative:"150"
      ~count:5
      ~sum:t.contactless_sum
      ~n:t.contactless_count
      ~amount
  ;;

  let record_remote t ~amount =
    { t with
      remote_count = t.remote_count + 1
    ; remote_sum = Money.Amount.add_exn t.remote_sum amount
    }
  ;;

  let record_contactless t ~amount =
    { t with
      contactless_count = t.contactless_count + 1
    ; contactless_sum = Money.Amount.add_exn t.contactless_sum amount
    }
  ;;
end

module Event = struct
  type t =
    { challenge : Id.Stable.V1.t
    ; subject : string
    ; operation : string
    ; amount : Money.Amount.Stable.V1.t option
    ; payee : string option
    ; binding : Binding.t
    ; kind : Kind.t option
    ; elements_passed : Element.t list
    ; channel : string
    ; device_id : string option
    ; at : Utc_time.Stable.V1.t
    ; attempt : int
    ; result : string
    ; lock : Lock.status
    ; exemption : Exemption.t option
    }
  [@@deriving bin_io, sexp, compare, equal]

  let of_outcome
    (c : Challenge.t)
    (outcome : Outcome.t)
    ~operation
    ?amount
    ?payee
    ?kind
    ~channel
    ?device_id
    ~lock
    ~at
    ()
    =
    let result =
      match outcome with
      | Verified -> "Verified"
      | Factor_accepted _ -> "Factor_accepted"
      | Wrong_response _ -> "Wrong_response"
      | Rejected _ -> "Rejected"
      | Already_verified -> "Already_verified"
      | Binding_mismatch -> "Binding_mismatch"
      | Not_a_factor _ -> "Not_a_factor"
    in
    { challenge = c.id
    ; subject = c.subject
    ; operation
    ; amount
    ; payee
    ; binding = c.binding
    ; kind
    ; elements_passed = Challenge.elements_passed c
    ; channel
    ; device_id
    ; at
    ; attempt = c.attempts
    ; result
    ; lock
    ; exemption = None
    }
  ;;
end

module Totp = struct
  let step = 30

  let code ~secret ~time ~digits =
    let counter =
      Int63.to_int_exn (Time_ns.to_int63_ns_since_epoch time) / 1_000_000_000 / step
    in
    let msg =
      String.init 8 ~f:(fun i -> Char.of_int_exn ((counter lsr (8 * (7 - i))) land 0xff))
    in
    let mac = Cryptokit.hash_string (Cryptokit.MAC.hmac_sha1 secret) msg in
    (* RFC 4226 dynamic truncation *)
    let offset = Char.to_int mac.[19] land 0xf in
    let byte i = Char.to_int mac.[offset + i] in
    let bin =
      ((byte 0 land 0x7f) lsl 24) lor (byte 1 lsl 16) lor (byte 2 lsl 8) lor byte 3
    in
    let modulus = Int.pow 10 digits in
    sprintf "%0*d" digits (bin % modulus)
  ;;

  (* Constant-time comparison so response timing does not leak matching digits. *)
  let equal_ct a b =
    String.length a = String.length b
    && String.foldi a ~init:0 ~f:(fun i acc c ->
         acc lor (Char.to_int c lxor Char.to_int b.[i]))
       = 0
  ;;

  let check ~secret ~time ?(window = 1) response =
    let digits = String.length response in
    digits >= 6
    && digits <= 8
    && List.exists (List.range (-window) window ~stop:`inclusive) ~f:(fun k ->
      let time = Time_ns.add time (Time_ns.Span.of_int_sec (k * step)) in
      equal_ct (code ~secret ~time ~digits) response)
  ;;
end
