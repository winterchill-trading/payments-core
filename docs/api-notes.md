# Notes on the schemes and standards

What was learned while modelling payments; read with
`specs/research/banking-infrastructure/payment-schemes.md` and
`strong-customer-authentication.md`.

## SEPA (EPC rulebooks 2025)

- SCT: credited to the beneficiary's bank within one business day; amount 0.01 to
  999 999 999.99 EUR; charge bearer always `SLEV`; returns within 3 TARGET business days.
- SCT Inst: 10 s hard limit from the acceptance time stamp (AT-T056); a confirmation can
  arrive later, so `Timed_out` is not final and a late settlement is booked as a fresh
  debit. No scheme maximum amount since October 2025 (AM02 retired). No returns after
  acceptance; the only pacs.004 is the `FOCR` answer to a recall. `ACCP` is the positive
  confirmation.
- Recall (camt.056): DUPL and TECH within 10 business days, FRAD within 13 months; RFRO
  (AC03, AM09, CUST) needs the beneficiary's consent. Answers within 15 business days:
  pacs.004 `FOCR` or camt.029 with NOAS, NOOR, ARDT, AC04, AM04, LEGL or CUST.
- Identifiers are Max35Text in the SEPA Latin set, never starting or ending with `/` and
  never containing `//`. Remittance is one unstructured line of at most 140 characters or
  one ISO 11649 creditor reference. Fully unstructured addresses stop being accepted on
  15 November 2026; `Postal_address` is structured with up to two hybrid lines.
- Business days are TARGET days: weekdays except 1 January, Good Friday, Easter Monday,
  1 May, 25 and 26 December.

## Idempotency

Every ledger entry booked for a transfer uses `<provider event id>/<leg>` as its
idempotency key (`hold`, `capture`, `fee`, `release`, `return`, `late-settlement`,
`reorg`, `deposit`, ...), so a webhook delivered twice books once. The transfer id is the
entry reference.

## Crypto

- Bitcoin deposits count at 3 confirmations, 6 above 10 000 EUR; Ethereum-style chains
  at the `finalized` tag. A reorg after crediting takes the credit and fee back until
  the transaction confirms again. Outgoing transfers settle at our threshold; a reorg
  after that is an operational incident, not a state.
- Travel rule (EU 2023/1113): every transfer between providers carries originator name,
  account and identification (address, document and customer number, or date and place
  of birth) and beneficiary name and account; self-hosted wallets above 1000 EUR need
  proof of ownership.
- Lightning: the preimage whose SHA-256 is the invoice's payment hash proves payment and
  is kept.

## SCA (PSD2 RTS)

- Two elements from different categories; OTP, TOTP and passkeys are all possession.
- The code is bound to operation, amount in minor units, currency and payee; any change
  invalidates it. A verified challenge is consumed.
- At most five consecutive failures (art. 4(3)(b)); five minutes of session inactivity
  (art. 4(3)(d)); the RTS sets no lifetime for an unanswered challenge (five minutes here
  is our choice).
- An exempted payment records the exemption and its article; low-value remote (art. 16:
  30 EUR, 100 EUR or 5 payments) and contactless (art. 11: 50, 150, 5) use counters
  since the last SCA. We require both the sum and the count limits, the stricter reading.
