# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[Semantic Versioning](https://semver.org/).

## Unreleased

### Added

- UK rails: `Rail.Faster_payments`, `Rail.Chaps` and `Rail.Bacs` (Bacs as a stub: currency,
  reference and settlement time only), with `Rail.is_uk`. They live in `Rail.Stable.V2`.
- `Sort_code`, `Uk_account` (sort code, eight-digit account number, optional roll
  number; from text or a GB IBAN) and `Uk_modulus`, the published modulus check over
  weight and substitution tables the caller loads (all 34 test cases of the
  specification pass).
- `Uk_scheme`: GBP only, a configurable Faster Payments limit per payment (at most the
  scheme's GBP 1m), a CHAPS cut-off hook with a default of 18:00 UK time on weekdays,
  `chaps_value_date`, reference length and character rules, and `Details` (scheme id,
  value date).
- `Beneficiary.Destination.Uk_account`, `Destination.uk_account`; a GB IBAN destination
  also reaches the UK rails. `Beneficiary.create` takes `?roll_number` and `?modulus`.
- `Beneficiary.Payee_check.Account_not_found` and `Unavailable` (UK Confirmation of
  Payee and SEPA Verification of Payee outcomes).
- `Transfer.Details.Uk_domestic`; `Transfer.draft ?uk_rules` validates UK drafts and gives
  a CHAPS draft its value date; `Transfer.incoming` accepts GBP on the UK rails.
- `Stable.V2` for `Rail`, `Beneficiary`, `Beneficiary.Destination`,
  `Beneficiary.Payee_check`, `Transfer`, `Transfer.Counterparty`, `Transfer.Details` and
  `Standing_order`, with pinned digests.
- `payments uk-account` command, with an optional modulus table.
- US rails: `Rail.Ach { sec; same_day }` (NACHA standard entry class `Ppd`, `Ccd`, `Web`,
  `Tel`, `Ctx`), `Rail.Rtp` and `Rail.Fedwire`, with `Rail.is_us`.
- `Aba` (routing numbers with the check digit), `Us_account` (routing number, 4 to 17
  digit account number, checking or savings), `Beneficiary.Destination.Us_account` and
  `Destination.us_account`.
- `Us_scheme`: USD only, a configurable RTP limit (at most the network's USD 10m), the
  Same Day ACH limit (USD 1m), references, and `Details` (network id, ACH effective date);
  `Transfer.Details.Us_domestic` and `Transfer.draft ?us_rules`.
- Networks `Base`, `Arbitrum`, `Plasma` (EVM addresses) and `Spark` (Bech32m, prefix and
  checksum only) in `Network.Stable.V2`, with `Crypto_address.Stable.V2`, confirmation
  policies and settlement times.
- `Beneficiary.Destination.Local { scheme; fields }` for account details in a market's
  own format (the shape of a local address in banking-provider), built with
  `Beneficiary.create_local` / `Destination.create_local`. `Destination.Local_format`
  gives per-scheme validation hooks; `kg` (six-digit BIK as `bank_code`, sixteen-digit
  `account_number`) and `ch_qr_iban` (QR-IBAN range check) ship as defaults, and other
  schemes pass generic checks. `Destination.local` reads the fields back. Local
  destinations reach no rail in `Rail.t`.
- `Beneficiary.postal_address` (an `Iso20022.Postal_address.t`, validated), set through
  `create ?postal_address`, `create_local ?postal_address` or `with_postal_address`, and
  `Beneficiary.check_for_rail ?address_required`, which also asks for the address on
  `Fedwire` and `Chaps` by default (`default_address_required`).
- `Stable.V3` of `Beneficiary`, `Beneficiary.Destination`, `Transfer`,
  `Transfer.Counterparty` and `Standing_order`, which hold local destinations and
  addresses. V2 and V1 keep their digests, read with no address, and raise on writing a
  local destination or an address.

### Compatibility

- The `Stable.V2` formats above were pinned once for the UK additions and re-pinned for
  the US rails and new networks in the same unreleased change set; none was released in
  between.
- Every existing `Stable.V1` digest is unchanged, and so is the format of the unversioned
  `bin_io` on these types (it is the V1 format), so records elsewhere that embed them keep
  their digests. The new constructors were appended, so V1 bytes read as V2.
- Writing a UK or US rail, a UK or US account, UK or US details, or anything on a newer
  network through a V1 format raises;
  use V2 where those can occur. `Account_not_found` and `Unavailable` are written as
  `Not_possible` by V1.
