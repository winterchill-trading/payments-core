# AGENTS.md (.github/)

`workflows/ci.yml` runs the `ox-setup` action with three pins (`fixed-decimal`, `money`, `ledger`),
the build, the tests and the shared `obj-magic` workflow.

- A new sibling dependency needs a `pins:` line, in dependency order.
- Check a run with `gh run list --limit 1`; at most one `gh run watch <id> --exit-status`.
