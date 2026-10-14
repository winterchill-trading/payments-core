# payments-core

Provider-neutral payment rails and transfer lifecycles in OxCaml: SEPA Credit Transfer
and SCT Inst with their ISO 20022 fields and reason codes, UK Faster Payments, CHAPS and
Bacs with sort codes and the published modulus check, US ACH, RTP and Fedwire with ABA
routing numbers, on-chain transfers with
travel-rule data, Lightning, intraledger moves; beneficiary validation; strong customer
authentication; standing orders; currency swaps; and the pure booking rules that turn
every transfer transition into balanced `ledger` entries. Reconciliation compares the
provider's view with the ledger and records breaks.

## Why separate

Payment semantics (which states a SEPA transfer can be in, which reason code a return
may carry, when a Bitcoin deposit counts, what SCA must bind) are the same whichever
provider moves the money. Keeping them here, over `money` and `ledger` only, lets a
provider adapter map its webhooks onto these states and lets `card-core`, `risk-rules`
and `banking-provider` share one transfer type, without any of them knowing a provider.

## Quick start

```sh
opam exec -- dune build
opam exec -- dune test
opam exec -- dune exec payments -- iban "DE89 3704 0044 0532 0130 00"
opam exec -- dune exec payments -- uk-account "04-00-04 12345678"
opam exec -- dune exec payments -- uk-account -modulus valacdos.txt "08-99-99 66374958"
opam exec -- dune exec payments -- address -network bitcoin bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4
opam exec -- dune exec payments -- lightning satoshi@example.com
opam exec -- dune exec payments -- schedule -monthly 31 -from 2027-01-15 -count 3
opam exec -- dune exec payments -- demo          # postings of a sample SEPA transfer
opam exec -- dune exec payments -- demo -fail    # ... rejected after submission
```

No environment variables, no network access, no credentials: everything is pure.

## Usage

```ocaml
open Payments_core

(* A SEPA transfer: draft, authorise, submit, settle, booking each step. *)
let chart = Booking.Chart.default

let pay ~balances ~beneficiary ~details ~now =
  let open Or_error.Let_syntax in
  let%bind t =
    Transfer.draft
      ~id:(Transfer.Id.of_string "tr-1")
      ~rail:Sepa_credit
      ~account:(Ledger.Account_id.of_string "customer:1/eur")
      ~beneficiary
      ~amount:(Money.Amount.of_string "25.00 EUR")
      ~details
      ~now
      ()
  in
  let%bind t = Transfer.exempt t ~exemption:Trusted_beneficiary in
  (* hold amount + fee on the customer account *)
  let%bind hold = Booking.transition chart balances t ~to_:Submitted ~event_id:"evt-1" ~at:now in
  let%bind t = Transfer.submit t ~at:now () in
  (* later, from a pacs.002 ACCC report *)
  let%bind balances = Booking.apply balances hold in
  let%bind capture = Booking.transition chart balances t ~to_:Settled ~event_id:"evt-2" ~at:now in
  let%map t = Transfer.apply_status_report t ACCC ~at:now () in
  t, hold @ capture
```

## Modules

| Module | What it is |
| --- | --- |
| `Rail`, `Network` | SEPA credit / instant, Faster Payments, CHAPS, Bacs, ACH, RTP, Fedwire, on-chain per network (Bitcoin, Ethereum, Polygon, Tron, Solana, Base, Arbitrum, Plasma, Spark), Lightning, intraledger, card scheme |
| `Sort_code`, `Uk_account`, `Uk_modulus` | UK sort codes, account numbers with roll numbers, GB IBAN conversion, the published modulus check |
| `Uk_scheme` | Faster Payments limit (configurable), CHAPS cut-off hook and value dates, references, UK transfer details |
| `Aba`, `Us_account`, `Us_scheme` | US routing numbers (check digit), accounts (checking or savings), ACH standard entry classes and Same Day limit, RTP limit (configurable), Fedwire, US transfer details |
| `Iban`, `Bic` | ISO 13616 IBAN (country lengths, mod 97, SEPA membership), ISO 9362 BIC |
| `Crypto_address`, `Base58`, `Bech32` | per-network address checks (Base58Check, SegWit v0/v1+, EIP-55, Tron, Solana) |
| `Lightning` | BOLT 11 invoices (network, amount, expiry, payment hash), LNURL, Lightning addresses |
| `Beneficiary` | payee with destination (IBAN, UK account, crypto, Lightning, internal), trusted flag, first seen, payee-name check result (match, close match, no match, not possible, account not found, unavailable) |
| `Iso_reason`, `Iso20022` | reason codes with their R-transaction kinds; payment ids, UETR, creditor reference, remittance, parties, structured addresses, pacs.002 statuses, SEPA character set |
| `Sepa`, `Target_calendar` | rulebook checks, reason lists per R-transaction, SCT Inst timing, return/recall deadlines in TARGET business days |
| `Travel_rule`, `Confirmation_policy` | originator/beneficiary data per the EU transfer-of-funds rules; when a deposit counts |
| `Transfer` | status machine with reason-coded rejects, returns, recalls, SCT Inst time-outs, chain reorgs |
| `Sca` | two-category challenges bound to amount, currency and payee; locks; sessions; exemptions; counters; audit events |
| `Booking` | transition to ledger entries (hold, capture, fee, release, return, late settlement, reorg) |
| `Swap` | quotes with spread and fee; three-entry execution |
| `Standing_order`, `Schedule` | recurring drafts with month-end clamping and catch-up |
| `Reconciliation` | balance and status diffs; transaction breaks with ageing and maker-checker resolution |

## Coverage

| Standard | Covered | Checked against |
| --- | --- | --- |
| ISO 13616 IBAN | full validation | published examples, generated property |
| BIP 173 / 350 | Bech32, Bech32m, SegWit | BIP vectors, reference encoder |
| EIP-55 | checksum | EIP vectors |
| BOLT 11 | prefix, amount, timestamp, `p`, `d`, `x` | spec example invoice, synthetic invoices |
| RFC 6238 | TOTP SHA-1 | RFC vectors |
| ISO 11649 | creditor references | standard example, round-trip property |
| ISO 17442 | LEI check digits | published LEI |
| EPC SCT / SCT Inst 2025 | fields, reason codes, timing, deadlines | rulebook tables (see `docs/api-notes.md`) |
| UK modulus checking (v8) | MOD10, MOD11, double alternate, exceptions 1 to 14 | the specification's 34 test cases over the matching lines of the v8.50 tables |
| Faster Payments, CHAPS | GBP, FPS GBP 1m limit, CHAPS 18:00 close, references | scheme limits; bank cut-offs and holidays are inputs |

Nothing here talks to a network, so there is nothing to verify live; all tests run on
vectors and fixtures in CI.

## Stable formats

Each wire type has `Stable` versions with digests pinned in `test/test_stable.ml`. The UK
and US additions and the newer networks made `Stable.V2` of `Network`, `Crypto_address`, `Rail`, `Beneficiary` (with its `Destination` and
`Payee_check`), `Transfer` (with `Counterparty` and `Details`) and `Standing_order`. V1
stays as it was, and the unversioned `bin_io` of these types is still the V1 format so
records that embed them keep their digests; V1 refuses to write a UK rail, account or
details (raises), and writes the two new payee check results as `Not_possible`. New
constructors were appended, so V1 bytes are also valid V2 bytes.

## Known gaps

- BOLT 11 node signatures are not verified (needs secp256k1 key recovery); the paying
  provider verifies them. The `s` (payment secret) field is not required.
- The window for a request for recall by the originator (AC03, AM09, CUST) is not
  confirmed by the research; 13 months is assumed.
- The Tron and Solana deposit thresholds are our choice, not from a scheme document.
- Business-day deadlines use the TARGET calendar only; national holidays are ignored.
- Reconciliation breaks are values; persisting them is the caller's job.
- The modulus check needs the published weight and substitution tables, which change
  several times a year and are not bundled; without them every account is
  `Not_checked`. Nine and ten digit account numbers (bank-specific conversions) are
  refused.
- UK bank holidays are not known: the default CHAPS cut-off covers weekends only, and a
  bank passes its own hook. Faster Payments reason codes are not validated (any ISO
  reason is accepted on UK rails).
- Bacs is a stub: no files, service user numbers or processing calendar.
- US rails: no cut-off times, Federal Reserve holidays, NACHA files or R-code tables; ACH
  returns carry whatever four-character reason the provider reports. FedNow is not a
  rail yet. A routing number with a valid check digit may still be unassigned.
- Spark addresses are checked for prefix and Bech32m checksum only.
