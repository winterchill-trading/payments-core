(** TARGET2 (T2) business days, the settlement calendar SEPA deadlines count in:
    every weekday except New Year's Day, Good Friday, Easter Monday, 1 May and
    25-26 December. *)

open! Core

val easter_sunday : year:int -> Date.t
val is_business_day : Date.t -> bool

(** [add_business_days d n] is the [n]th business day after [d] ([n >= 0]; [n = 0] gives
    [d] itself if it is a business day, else the next one). *)
val add_business_days : Date.t -> int -> Date.t
