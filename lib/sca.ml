open! Core
open Async_kernel

module Kind = struct
  type t =
    | Otp
    | Totp
    | Passkey
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
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

  let create ~operation ~amount ~payee =
    (* Length-prefixed fields so "ab"+"c" and "a"+"bc" hash differently. *)
    let field s = sprintf "%d:%s" (String.length s) s in
    Cryptokit.hash_string
      (Cryptokit.Hash.sha256 ())
      (String.concat [ field operation; field amount; field payee ])
    |> hex
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

module Challenge = struct
  module Stable = struct
    module V1 = struct
      type t =
        { id : Id.Stable.V1.t
        ; kind : Kind.t
        ; subject : string
        ; binding : Binding.t
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
    ~kind
    ~subject
    ~binding
    ~now
    ?(ttl = Time_ns.Span.of_min 5.)
    ?(max_attempts = 3)
    ()
    =
    { id
    ; kind
    ; subject
    ; binding
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
end

module Outcome = struct
  type t =
    | Verified
    | Wrong_response of { remaining : int }
    | Rejected of Failure.t
    | Already_verified
    | Binding_mismatch
  [@@deriving sexp, compare, equal]
end

let record (c : Challenge.t) ~now ~correct =
  let c = Challenge.expire c ~now in
  match c.status with
  | Verified _ -> c, Outcome.Already_verified
  | Failed f -> c, Rejected f
  | Pending ->
    if correct
    then { c with status = Verified { at = now } }, Verified
    else (
      let attempts = c.attempts + 1 in
      if attempts >= c.max_attempts
      then { c with attempts; status = Failed Too_many_attempts }, Rejected Too_many_attempts
      else { c with attempts }, Wrong_response { remaining = c.max_attempts - attempts })
;;

module type Verifier = sig
  val check : Challenge.t -> response:string -> bool Deferred.t
end

let verify (module V : Verifier) (c : Challenge.t) ~binding ~response ~now =
  if not (Binding.equal binding c.binding)
  then return (c, Outcome.Binding_mismatch)
  else if not (Challenge.is_open c ~now)
  then return (record c ~now ~correct:false)
  else (
    let%map correct = V.check c ~response in
    record c ~now ~correct)
;;

module Totp = struct
  let step = 30

  let code ~secret ~time ~digits =
    let counter = Int63.to_int_exn (Time_ns.to_int63_ns_since_epoch time) / 1_000_000_000 / step in
    let msg = String.init 8 ~f:(fun i -> Char.of_int_exn ((counter lsr (8 * (7 - i))) land 0xff)) in
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
