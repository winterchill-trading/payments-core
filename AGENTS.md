# AGENTS.md

Guide for AI coding agents working in this repository. Read this first; the subdirectory
`AGENTS.md` files add detail for `lib/`, `bin/`, `test/` and `docs/`. The workspace-wide guide is
`../oxcaml-claude-skills/AGENTS.md`.

## Purpose

`payments-core` models payment rails and transfer lifecycles without network access: SEPA credit
and instant transfers with ISO 20022 fields and reason codes, on-chain transfers with travel-rule
data and a confirmation policy, Lightning, and intraledger moves. It also covers beneficiaries,
strong customer authentication (SCA), standing orders, swaps, and the pure booking rules that turn
each transfer transition into balanced `ledger` entries. A provider adapter maps its webhooks onto
these states.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The `payments-core` library (`Payments_core`): rails and networks, IBAN, BIC, addresses, Lightning, ISO 20022, SEPA, transfers, SCA, booking, swaps, standing orders, reconciliation |
| `bin/` | The `payments` command: `iban`, `bic`, `address`, `lightning`, `schedule`, `demo` |
| `test/` | Expect tests with published vectors, Base_quickcheck properties, the `cli.t` cram test |
| `docs/` | `api-notes.md`: what was learned modelling the schemes and SCA |
| `.github/` | CI workflow |
| `dune-project` | Source of truth for package metadata; `dune build` regenerates `payments-core.opam` |

## Build, test, docs

Inside the workspace (sibling checkouts and the shared `5.2.0+ox` switch are required):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @payments-core/all
opam exec --switch 5.2.0+ox -- dune build --root .. @payments-core/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @payments-core/doc
```

Parallel work: prefix the commands with `DUNE_BUILD_DIR=_build-payments-core`.

Standalone: `opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`,
then `opam install --deps-only --with-test .` and `dune build && dune test`. `money` and `ledger` come
from that repository.

## Conventions

- Every module has an `.mli`. Code is `open! Core`, labelled arguments, ppx_jane derivations.
- Each module that carries a wire contract exposes a `Stable` submodule with pinned digests; add a
  version rather than editing a pinned one.
- Vectors come from the standards (BIP 173 and 350, EIP-55, BOLT 11, RFC 6238, ISO 11649, ISO 17442).
  Add a new check with a vector from the standard, not from this code's own output.
- Expect tests with stable times must not print local-zone times (see Pitfalls).
- Formatting: `opam exec --switch 5.2.0+ox -- dune build --root .. @payments-core/fmt --auto-promote`.
  Never run `dune fmt` at the workspace root.

## Dependencies and dependents

- Depends on `money` and `ledger` (both pinned from GitHub in CI), plus `fixed-decimal` transitively.
- Used by `card-core` (SCA counters and exemptions), and by `neobank-api` and the banking libraries
  through the booking and transfer types.
- After a change to a transfer state or a booking, build the dependents:
  `dune build --root .. @card-core/all @neobank-api/all`.

## CI

`ci.yml` runs the `ox-setup` action with the pins for `fixed-decimal`, `money` and `ledger`, then
`dune build`, `dune test` and the shared `obj-magic` workflow. Check a run with
`gh run list --limit 1`, then at most one `gh run watch <id> --exit-status`.

## Pitfalls

- A library module named `String_id` is shadowed by `open! Core`, which has its own `String_id`
  without `Make`. The id functor is `Opaque_id`.
- `Time_ns.Stable.V1` sexps print in the machine's local zone, so an expect test over records with
  stable times differs between a laptop and CI. Compare times as values, or print them in UTC.
- SCT Inst times out after 10 seconds; a settlement that arrives late is booked as a fresh entry.
- Five failed SCA attempts lock the challenge for 30 minutes.
- A transfer's hold, fee and capture are booked as one linked group: all or nothing.
- `Swap.Quote.t` has public fields and `Swap.execute` does not recompute amounts from the rate; build
  the record directly when a pricing rule must apply.

## Where to read more

- `README.md`: why separate, quick start, usage, modules, coverage, known gaps.
- `docs/api-notes.md`, and `../specs/repos/payments-core.md`.
- `../specs/research/banking-infrastructure/` (payment schemes, SCA).
- `../oxcaml-claude-skills/skills/banking-standards/`.
- `../oxcaml-claude-skills/GOTCHAS.md`, section "Banking libraries".

## Never do

- Install or upgrade opam packages that change the compiler, `core`, `base`, `async` or
  `ppx_jane` on the shared switch. Run `opam install --switch 5.2.0+ox --dry-run <pkg>` first
  and stop if the plan touches any of them.
- Publish, tag or release. Never add a network call to this library; adapters live elsewhere.
- Print secrets, or poll CI in a loop.
