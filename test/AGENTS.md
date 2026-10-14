# AGENTS.md (test/)

Expect tests with published vectors, Base_quickcheck properties and the `cli.t` cram test.

- One `test_<module>.ml` per library area, plus `helpers.ml`.
- Properties: over random lifecycles the trial balance stays zero and every hold is closed; swap
  rounding; `Schedule.next_after`; generated ids pass the SEPA character checks.
- `test_stable.ml` pins the wire digests. A failure there means a wire shape changed.
- Expect output with local-zone or stable times must be made zone-independent (see root Pitfalls).
- Promote expect output with `dune promote <file>` for the one file that changed.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @payments-core/runtest`.
