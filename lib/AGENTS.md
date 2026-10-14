# AGENTS.md (lib/)

The `Payments_core` library. It has no network access; a provider adapter maps its webhooks onto
these states and lives outside this repository.

- Rails and identifiers: `Rail`, `Network`, `Iban`, `Bic`, `Crypto_address`, `Base58`, `Bech32`,
  `Lightning`, `Beneficiary`, `Iso_reason`, `Iso20022`, `Sepa`, `Target_calendar`, and the UK
  `Sort_code`, `Uk_account`, `Uk_modulus` (published modulus check), `Uk_scheme`, and the US
  `Aba`, `Us_account`, `Us_scheme`.
- Lifecycles: `Transfer` (status machine with reason-coded rejects, returns, recalls, SCT Inst
  time-outs, reorgs), `Sca`, `Standing_order`, `Schedule`, `Swap`.
- Booking: `Booking` turns each transition into balanced `ledger` entries, one linked group per
  transfer. `Reconciliation` compares a provider's view with the ledger and returns breaks.
- Name id functors `Opaque_id`-style; `String_id` is shadowed by Core.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @payments-core/all`.
