# AGENTS.md (bin/)

The `payments` command: `iban`, `bic`, `address`, `lightning`, `schedule` and `demo`. `payments.ml`
parses arguments and calls `../lib`; validation rules stay in the library.

- `test/cli.t` is a cram test of the output. Promote only that file's changes with `dune promote`.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @payments-core/all`.
