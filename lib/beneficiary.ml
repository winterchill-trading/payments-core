open! Core

module Id =
  Opaque_id.Make
    (struct
      let module_name = "Payments_core.Beneficiary.Id"
    end)
    ()

module Payee_check = struct
  module T = struct
    type t =
      | Match
      | Close_match of { suggested_name : string }
      | No_match
      | Not_possible
      | Account_not_found
      | Unavailable
    [@@deriving bin_io, sexp, compare, equal, hash]
  end

  module Stable = struct
    module V2 = T

    module V1 = struct
      (* The original four results: the bin_prot shape and digest come from here. *)
      module Repr = struct
        type t =
          | Match
          | Close_match of { suggested_name : string }
          | No_match
          | Not_possible
        [@@deriving bin_io]
      end

      type t = T.t [@@deriving sexp, compare, equal, hash]

      include
        Binable.Of_binable_without_uuid [@alert "-legacy"]
          (Repr)
          (struct
            type nonrec t = t

            let to_binable : t -> Repr.t = function
              | Match -> Match
              | Close_match { suggested_name } -> Close_match { suggested_name }
              | No_match -> No_match
              (* Both refine "the check could not be made", which is all V1 can say. *)
              | Not_possible | Account_not_found | Unavailable -> Not_possible
            ;;

            let of_binable : Repr.t -> t = function
              | Match -> Match
              | Close_match { suggested_name } -> Close_match { suggested_name }
              | No_match -> No_match
              | Not_possible -> Not_possible
            ;;
          end)
    end
  end

  type t = T.t =
    | Match
    | Close_match of { suggested_name : string }
    | No_match
    | Not_possible
    | Account_not_found
    | Unavailable
  [@@deriving sexp, compare, equal, hash]

  include (Stable.V1 : Binable.S with type t := t)
end

module Destination = struct
  module T = struct
    type t =
      | Bank_account of
          { iban : Iban.Stable.V1.t
          ; bic : Bic.Stable.V1.t option
          }
      | Crypto of
          { address : Crypto_address.Stable.V2.t
          ; memo : string option
          }
      | Lightning of Lightning.Target.Stable.V1.t
      | Internal of { account : string }
      | Uk_account of Uk_account.Stable.V1.t
      | Us_account of Us_account.Stable.V1.t
      | Local of
          { scheme : string
          ; fields : (string * string) list
          }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end

  module Stable = struct
    module V3 = T

    module V2 = struct
      (* The six destinations before [Local]: the bin_prot shape and digest come from
         here. *)
      module Repr = struct
        type t =
          | Bank_account of
              { iban : Iban.Stable.V1.t
              ; bic : Bic.Stable.V1.t option
              }
          | Crypto of
              { address : Crypto_address.Stable.V2.t
              ; memo : string option
              }
          | Lightning of Lightning.Target.Stable.V1.t
          | Internal of { account : string }
          | Uk_account of Uk_account.Stable.V1.t
          | Us_account of Us_account.Stable.V1.t
        [@@deriving bin_io, sexp]
      end

      type t = T.t [@@deriving compare, equal, hash]

      let to_repr : t -> Repr.t = function
        | Bank_account { iban; bic } -> Bank_account { iban; bic }
        | Crypto { address; memo } -> Crypto { address; memo }
        | Lightning target -> Lightning target
        | Internal { account } -> Internal { account }
        | Uk_account a -> Uk_account a
        | Us_account a -> Us_account a
        | Local { scheme; _ } ->
          raise_s
            [%message
              "Beneficiary.Destination.Stable.V2 cannot hold a local destination; write \
               V3"
                scheme]
      ;;

      let of_repr : Repr.t -> t = function
        | Bank_account { iban; bic } -> Bank_account { iban; bic }
        | Crypto { address; memo } -> Crypto { address; memo }
        | Lightning target -> Lightning target
        | Internal { account } -> Internal { account }
        | Uk_account a -> Uk_account a
        | Us_account a -> Us_account a
      ;;

      include
        Binable.Of_binable_without_uuid [@alert "-legacy"]
          (Repr)
          (struct
            type nonrec t = t

            let to_binable = to_repr
            let of_binable = of_repr
          end)

      include
        Sexpable.Of_sexpable
          (Repr)
          (struct
            type nonrec t = t

            let to_sexpable = to_repr
            let of_sexpable = of_repr
          end)
    end

    module V1 = struct
      (* The original four destinations: the bin_prot shape and digest come from here. *)
      module Repr = struct
        type t =
          | Bank_account of
              { iban : Iban.Stable.V1.t
              ; bic : Bic.Stable.V1.t option
              }
          | Crypto of
              { address : Crypto_address.Stable.V1.t
              ; memo : string option
              }
          | Lightning of Lightning.Target.Stable.V1.t
          | Internal of { account : string }
        [@@deriving bin_io]
      end

      type t = T.t [@@deriving sexp, compare, equal, hash]

      include
        Binable.Of_binable_without_uuid [@alert "-legacy"]
          (Repr)
          (struct
            type nonrec t = t

            let to_binable : t -> Repr.t = function
              | Bank_account { iban; bic } -> Bank_account { iban; bic }
              | Crypto { address; memo } -> Crypto { address; memo }
              | Lightning target -> Lightning target
              | Internal { account } -> Internal { account }
              | Uk_account account ->
                raise_s
                  [%message
                    "Beneficiary.Destination.Stable.V1 cannot hold a UK account; write V2"
                      ~_:(Uk_account.to_string account : string)]
              | Us_account account ->
                raise_s
                  [%message
                    "Beneficiary.Destination.Stable.V1 cannot hold a US account; write V2"
                      ~_:(Us_account.to_string account : string)]
              | Local { scheme; _ } ->
                raise_s
                  [%message
                    "Beneficiary.Destination.Stable.V1 cannot hold a local destination; \
                     write V3"
                      scheme]
            ;;

            let of_binable : Repr.t -> t = function
              | Bank_account { iban; bic } -> Bank_account { iban; bic }
              | Crypto { address; memo } -> Crypto { address; memo }
              | Lightning target -> Lightning target
              | Internal { account } -> Internal { account }
            ;;
          end)
    end
  end

  type t = T.t =
    | Bank_account of
        { iban : Iban.Stable.V1.t
        ; bic : Bic.Stable.V1.t option
        }
    | Crypto of
        { address : Crypto_address.Stable.V1.t
        ; memo : string option
        }
    | Lightning of Lightning.Target.Stable.V1.t
    | Internal of { account : string }
    | Uk_account of Uk_account.Stable.V1.t
    | Us_account of Us_account.Stable.V1.t
    | Local of
        { scheme : string
        ; fields : (string * string) list
        }
  [@@deriving sexp, compare, equal, hash]

  include (Stable.V1 : Binable.S with type t := t)

  let uk_rails = [ Rail.Faster_payments; Chaps; Bacs ]

  let rails = function
    | Bank_account { iban; _ } ->
      (if Iban.is_sepa iban then [ Rail.Sepa_credit; Sepa_instant ] else [])
      @ if String.equal (Iban.country iban) "GB" then uk_rails else []
    | Crypto { address; _ } -> [ Onchain (Crypto_address.network address) ]
    | Lightning _ -> [ Lightning ]
    | Internal _ -> [ Intraledger ]
    | Uk_account _ -> uk_rails
    | Us_account _ -> List.filter Rail.all ~f:Rail.is_us
    (* No rail in [Rail.t] names a domestic scheme outside SEPA, the UK and the US; the
       provider that holds the account routes these. *)
    | Local _ -> []
  ;;

  let uk_account = function
    | Uk_account a -> Some a
    | Bank_account { iban; _ } -> Uk_account.of_iban iban |> Result.ok
    | Crypto _ | Lightning _ | Internal _ | Us_account _ | Local _ -> None
  ;;

  let us_account = function
    | Us_account a -> Some a
    | Bank_account _ | Crypto _ | Lightning _ | Internal _ | Uk_account _ | Local _ ->
      None
  ;;

  let local t ~scheme =
    match t with
    | Local l when String.equal l.scheme scheme -> Some l.fields
    | Local _
    | Bank_account _
    | Crypto _
    | Lightning _
    | Internal _
    | Uk_account _
    | Us_account _ -> None
  ;;

  module Local_scheme = struct
    let kg = "kg"
    let ch_qr_iban = "ch_qr_iban"
  end

  module Field_error = struct
    type t =
      { field : string option
      ; reason : string
      }
    [@@deriving sexp, compare, equal]
  end

  module Local_error = struct
    type t =
      { summary : Error.t
      ; fields : Field_error.t list
      }
    [@@deriving sexp_of]
  end

  module Local_format = struct
    type t =
      { scheme : string
      ; validate : (string * string) list -> (string * string) list Or_error.t
      ; field_errors : ((string * string) list -> Field_error.t list) option
      }

    let create ~scheme ~validate = { scheme; validate; field_errors = None }
    let with_field_errors t ~f = { t with field_errors = Some f }
    let scheme t = t.scheme
    let validate t fields = t.validate fields

    let field_errors t fields =
      match t.field_errors with
      | Some f -> f fields
      | None ->
        (match t.validate fields with
         | Ok _ -> []
         | Error e -> [ { Field_error.field = None; reason = Error.to_string_hum e } ])
    ;;

    let error field reason = { Field_error.field = Some field; reason }

    (* Per-field form of [exactly]: each missing name and each unexpected one. *)
    let expected_fields names fields =
      let given = List.map fields ~f:fst in
      List.filter_map names ~f:(fun n ->
        if List.mem given n ~equal:String.equal then None else Some (error n "missing"))
      @ List.filter_map given ~f:(fun n ->
        if List.mem names n ~equal:String.equal
        then None
        else Some (error n "unexpected field"))
    ;;

    let digit_errors ~field ~length fields =
      match List.Assoc.find fields field ~equal:String.equal with
      | None -> []
      | Some value ->
        let value = String.filter value ~f:(fun c -> not (Char.is_whitespace c)) in
        if String.length value = length && String.for_all value ~f:Char.is_digit
        then []
        else [ error field (sprintf "expected %d digits" length) ]
    ;;

    let digits ~field ~length value =
      if String.length value = length && String.for_all value ~f:Char.is_digit
      then Ok ()
      else Or_error.error_s [%message "expected digits" field (length : int) value]
    ;;

    (* Exactly the named fields, each once, returned in the order given. *)
    let exactly names fields =
      let given = List.map fields ~f:fst |> List.sort ~compare:String.compare in
      if List.equal String.equal given (List.sort names ~compare:String.compare)
      then
        Ok
          (List.map names ~f:(fun n ->
             n, List.Assoc.find_exn fields n ~equal:String.equal))
      else
        Or_error.error_s
          [%message
            "unexpected fields" ~expected:(names : string list) (given : string list)]
    ;;

    let without_spaces s = String.filter s ~f:(fun c -> not (Char.is_whitespace c))

    let kg_field_errors fields =
      expected_fields [ "bank_code"; "account_number" ] fields
      @ digit_errors ~field:"bank_code" ~length:6 fields
      @ digit_errors ~field:"account_number" ~length:16 fields
    ;;

    let qr_iban_field_errors fields =
      expected_fields [ "qr_iban" ] fields
      @
      match List.Assoc.find fields "qr_iban" ~equal:String.equal with
      | None -> []
      | Some value ->
        (match Iban.of_string value with
         | Error e -> [ error "qr_iban" (Error.to_string_hum e) ]
         | Ok iban ->
           (match Iban.country iban with
            | "CH" | "LI" ->
              let iid = Int.of_string_opt (String.prefix (Iban.bban iban) 5) in
              (match iid with
               | Some iid when iid >= 30000 && iid <= 31999 -> []
               | _ -> [ error "qr_iban" "not a QR-IBAN" ])
            | _ -> [ error "qr_iban" "QR-IBAN outside CH and LI" ]))
    ;;

    let kg =
      with_field_errors ~f:kg_field_errors
      @@ create ~scheme:Local_scheme.kg ~validate:(fun fields ->
        let open Or_error.Let_syntax in
        let%bind fields = exactly [ "bank_code"; "account_number" ] fields in
        let fields = List.map fields ~f:(fun (n, v) -> n, without_spaces v) in
        let find n = List.Assoc.find_exn fields n ~equal:String.equal in
        (* The bank code is the six-digit BIK; account numbers are sixteen digits. *)
        let%bind () = digits ~field:"bank_code" ~length:6 (find "bank_code") in
        let%map () = digits ~field:"account_number" ~length:16 (find "account_number") in
        fields)
    ;;

    let ch_qr_iban =
      with_field_errors ~f:qr_iban_field_errors
      @@ create ~scheme:Local_scheme.ch_qr_iban ~validate:(fun fields ->
        let open Or_error.Let_syntax in
        let%bind fields = exactly [ "qr_iban" ] fields in
        let%bind iban = Iban.of_string (snd (List.hd_exn fields)) in
        let%bind () =
          match Iban.country iban with
          | "CH" | "LI" -> Ok ()
          | country -> Or_error.error_s [%message "QR-IBAN outside CH and LI" country]
        in
        (* A QR-IBAN is told apart by its institution id (the first five BBAN digits),
           reserved in 30000-31999. *)
        let iid = Int.of_string_opt (String.prefix (Iban.bban iban) 5) in
        match iid with
        | Some iid when iid >= 30000 && iid <= 31999 ->
          Ok [ "qr_iban", Iban.to_string iban ]
        | _ ->
          Or_error.error_s [%message "not a QR-IBAN" ~_:(Iban.to_string iban : string)])
    ;;

    let defaults = [ kg; ch_qr_iban ]
  end

  let is_snake_name s =
    (not (String.is_empty s))
    && Char.is_lowercase s.[0]
    && String.for_all s ~f:(fun c ->
      Char.is_lowercase c || Char.is_digit c || Char.equal c '_')
  ;;

  let create_local ?(formats = Local_format.defaults) ~scheme fields =
    let open Or_error.Let_syntax in
    let%bind () =
      if is_snake_name scheme
      then Ok ()
      else Or_error.error_s [%message "local scheme must be lower snake case" scheme]
    in
    let fields = List.map fields ~f:(fun (n, v) -> n, String.strip v) in
    let%bind () =
      if List.is_empty fields then Or_error.error_string "no fields" else Ok ()
    in
    let%bind () =
      List.map fields ~f:(fun (n, v) ->
        if not (is_snake_name n)
        then Or_error.error_s [%message "field name must be lower snake case" n]
        else if String.is_empty v
        then Or_error.error_s [%message "empty field" n]
        else Ok ())
      |> Or_error.all_unit
    in
    let%bind () =
      match List.find_a_dup (List.map fields ~f:fst) ~compare:String.compare with
      | Some n -> Or_error.error_s [%message "duplicate field" n]
      | None -> Ok ()
    in
    let%map fields =
      match
        List.find formats ~f:(fun (f : Local_format.t) -> String.equal f.scheme scheme)
      with
      | None -> Ok fields
      | Some format ->
        Local_format.validate format fields
        |> Or_error.tag_s ~tag:[%message "local destination" scheme]
    in
    Local { scheme; fields }
  ;;

  (* Every problem [create_local] would report, one per field where it belongs to one;
     only computed once [create_local] has refused, so the two never disagree on whether
     the destination is valid. *)
  let local_field_errors ~formats ~scheme fields =
    let whole reason = { Field_error.field = None; reason } in
    let fields = List.map fields ~f:(fun (n, v) -> n, String.strip v) in
    let scheme_errors =
      if is_snake_name scheme
      then []
      else [ whole "local scheme must be lower snake case" ]
    in
    let generic =
      if List.is_empty fields
      then [ whole "no fields" ]
      else (
        let per_field =
          List.concat_map fields ~f:(fun (n, v) ->
            if not (is_snake_name n)
            then [ Local_format.error n "field name must be lower snake case" ]
            else if String.is_empty v
            then [ Local_format.error n "empty field" ]
            else [])
        in
        let dups =
          List.find_all_dups (List.map fields ~f:fst) ~compare:String.compare
          |> List.map ~f:(fun n -> Local_format.error n "duplicate field")
        in
        per_field @ dups)
    in
    match scheme_errors @ generic with
    | _ :: _ as errors -> errors
    | [] ->
      (match
         List.find formats ~f:(fun (f : Local_format.t) -> String.equal f.scheme scheme)
       with
       | None -> []
       | Some format -> Local_format.field_errors format fields)
  ;;

  let check_local ?(formats = Local_format.defaults) ~scheme fields =
    match create_local ~formats ~scheme fields with
    | Ok t -> Ok t
    | Error summary ->
      let fields =
        match local_field_errors ~formats ~scheme fields with
        | [] ->
          (* A format hook that refuses without naming a field. *)
          [ { Field_error.field = None; reason = Error.to_string_hum summary } ]
        | errors -> errors
      in
      Error { Local_error.summary; fields }
  ;;
end

module Postal_address = Iso20022.Postal_address

module Repr = struct
  module V3 = struct
    type t =
      { id : Id.t
      ; owner : string
      ; name : string
      ; destination : Destination.Stable.V3.t
      ; trusted : bool
      ; first_seen : Utc_time.Stable.V1.t
      ; payee_check : Payee_check.Stable.V2.t option
      ; postal_address : Postal_address.t option [@sexp.option]
      }
    [@@deriving bin_io, sexp, compare, equal, hash, fields]
  end

  module V2 = struct
    type t =
      { id : Id.t
      ; owner : string
      ; name : string
      ; destination : Destination.Stable.V2.t
      ; trusted : bool
      ; first_seen : Utc_time.Stable.V1.t
      ; payee_check : Payee_check.Stable.V2.t option
      }
    [@@deriving bin_io, sexp]
  end

  module V1 = struct
    type t =
      { id : Id.t
      ; owner : string
      ; name : string
      ; destination : Destination.Stable.V1.t
      ; trusted : bool
      ; first_seen : Utc_time.Stable.V1.t
      ; payee_check : Payee_check.Stable.V1.t option
      }
    [@@deriving bin_io, sexp]
  end
end

type t = Repr.V3.t =
  { id : Id.t
  ; owner : string
  ; name : string
  ; destination : Destination.t
  ; trusted : bool
  ; first_seen : Utc_time.t
  ; payee_check : Payee_check.t option
  ; postal_address : Postal_address.t option [@sexp.option]
  }
[@@deriving sexp, compare, equal, hash, fields]

(* Older formats have no room for an address; dropping it silently would lose what a wire
   needs, so writing one raises. *)
let refuse_address ~version (t : t) =
  match t.postal_address with
  | None -> ()
  | Some _ ->
    raise_s
      [%message
        "Beneficiary.Stable cannot hold a postal address; write V3"
          (version : string)
          ~id:(t.id : Id.t)]
;;

module Stable = struct
  module V3 = struct
    type t = Repr.V3.t [@@deriving bin_io, sexp, compare, equal, hash]
  end

  module V2 = struct
    type nonrec t = t [@@deriving compare, equal, hash]

    let to_repr (t : t) : Repr.V2.t =
      refuse_address ~version:"V2" t;
      { id = t.id
      ; owner = t.owner
      ; name = t.name
      ; destination = t.destination
      ; trusted = t.trusted
      ; first_seen = t.first_seen
      ; payee_check = t.payee_check
      }
    ;;

    let of_repr (r : Repr.V2.t) : t =
      { id = r.id
      ; owner = r.owner
      ; name = r.name
      ; destination = r.destination
      ; trusted = r.trusted
      ; first_seen = r.first_seen
      ; payee_check = r.payee_check
      ; postal_address = None
      }
    ;;

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr.V2)
        (struct
          type nonrec t = t

          let to_binable = to_repr
          let of_binable = of_repr
        end)

    include
      Sexpable.Of_sexpable
        (Repr.V2)
        (struct
          type nonrec t = t

          let to_sexpable = to_repr
          let of_sexpable = of_repr
        end)
  end

  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, hash]

    let to_repr (t : t) : Repr.V1.t =
      refuse_address ~version:"V1" t;
      { id = t.id
      ; owner = t.owner
      ; name = t.name
      ; destination = t.destination
      ; trusted = t.trusted
      ; first_seen = t.first_seen
      ; payee_check = t.payee_check
      }
    ;;

    let of_repr (r : Repr.V1.t) : t =
      { id = r.id
      ; owner = r.owner
      ; name = r.name
      ; destination = r.destination
      ; trusted = r.trusted
      ; first_seen = r.first_seen
      ; payee_check = r.payee_check
      ; postal_address = None
      }
    ;;

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr.V1)
        (struct
          type nonrec t = t

          let to_binable = to_repr
          let of_binable = of_repr
        end)

    include
      Sexpable.Of_sexpable
        (Repr.V1)
        (struct
          type nonrec t = t

          let to_sexpable = to_repr
          let of_sexpable = of_repr
        end)
  end
end

include (Stable.V1 : Binable.S with type t := t)

let destination_for ~(rail : Rail.t) ?bic ?memo ?roll_number ?modulus raw =
  let open Or_error.Let_syntax in
  match rail with
  | Sepa_credit | Sepa_instant ->
    let%bind iban = Iban.of_string raw in
    let%bind bic =
      match bic with
      | None -> Ok None
      | Some b -> Bic.of_string b >>| Option.some
    in
    if Iban.is_sepa iban
    then Ok (Destination.Bank_account { iban; bic })
    else Or_error.error_s [%message "IBAN country is outside SEPA" (Iban.country iban)]
  | Onchain network ->
    let%map address = Crypto_address.create network raw in
    Destination.Crypto { address; memo }
  | Lightning ->
    let%map target = Lightning.Target.of_string raw in
    Destination.Lightning target
  | Intraledger ->
    if String.is_empty (String.strip raw)
    then Or_error.error_string "empty account id"
    else Ok (Destination.Internal { account = String.strip raw })
  | Card_scheme -> Or_error.error_string "card payments have no beneficiary"
  | Faster_payments | Chaps | Bacs ->
    let%bind account = Uk_account.of_string ?modulus raw in
    let%map account =
      match roll_number with
      | None -> Ok account
      | Some roll_number ->
        Uk_account.create
          ?modulus
          ~roll_number
          ~sort_code:account.sort_code
          ~account_number:account.account_number
          ()
    in
    Destination.Uk_account account
  | Ach _ | Rtp | Fedwire ->
    let%map account = Us_account.of_string raw in
    Destination.Us_account account
;;

let usable_on t rail = List.mem (Destination.rails t.destination) rail ~equal:Rail.equal

let check_name name =
  let name = String.strip name in
  if String.is_empty name
  then Or_error.error_string "beneficiary name is empty"
  else Ok name
;;

let check_address = function
  | None -> Ok ()
  | Some a -> Postal_address.validate a |> Or_error.tag ~tag:"postal address"
;;

let make ~id ~owner ~name ~destination ~postal_address ~now =
  { id
  ; owner
  ; name
  ; destination
  ; trusted = false
  ; first_seen = now
  ; payee_check = None
  ; postal_address
  }
;;

let create
  ~id
  ~owner
  ~name
  ~rail
  ?bic
  ?memo
  ?roll_number
  ?modulus
  ?postal_address
  ~now
  raw
  =
  let open Or_error.Let_syntax in
  let%bind name = check_name name in
  let%bind () = check_address postal_address in
  let%map destination = destination_for ~rail ?bic ?memo ?roll_number ?modulus raw in
  make ~id ~owner ~name ~destination ~postal_address ~now
;;

let create_local ~id ~owner ~name ?formats ?postal_address ~now ~scheme fields =
  let open Or_error.Let_syntax in
  let%bind name = check_name name in
  let%bind () = check_address postal_address in
  let%map destination = Destination.create_local ?formats ~scheme fields in
  make ~id ~owner ~name ~destination ~postal_address ~now
;;

let with_postal_address t postal_address =
  let%map.Or_error () = check_address postal_address in
  { t with postal_address }
;;

let default_address_required : Rail.t -> bool = function
  | Fedwire | Chaps -> true
  | Sepa_credit
  | Sepa_instant
  | Onchain _
  | Lightning
  | Intraledger
  | Card_scheme
  | Faster_payments
  | Bacs
  | Ach _
  | Rtp -> false
;;

let check_for_rail ?(address_required = default_address_required) t rail =
  if not (usable_on t rail)
  then
    Or_error.error_s
      [%message "destination cannot be reached on this rail" (rail : Rail.t)]
  else if address_required rail && Option.is_none t.postal_address
  then
    Or_error.error_s
      [%message "this rail needs the beneficiary's postal address" (rail : Rail.t)]
  else Ok ()
;;

let trust t = { t with trusted = true }
let untrust t = { t with trusted = false }
let with_payee_check t check = { t with payee_check = Some check }

let is_new t ~now ~cooling_off =
  Time_ns.Span.( < ) (Time_ns.diff now t.first_seen) cooling_off
;;
