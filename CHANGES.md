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

### Compatibility

- Every existing `Stable.V1` digest is unchanged, and so is the format of the unversioned
  `bin_io` on these types (it is the V1 format), so records elsewhere that embed them keep
  their digests. The new constructors were appended, so V1 bytes read as V2.
- Writing a UK rail, a UK account destination or UK details through a V1 format raises;
  use V2 where those can occur. `Account_not_found` and `Unavailable` are written as
  `Not_possible` by V1.
