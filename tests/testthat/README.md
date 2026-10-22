# tidywater tests

- `test-<function>.R`: behavior, errors, warnings, and reference checks against published or hand-calculated values.
- `test-regression-<topic>.R`: golden tests that pin the package's current numeric output (`expect_golden()` in `helper-regression.R`, snapshots in `_snaps/`).

The regression files are excluded from the built package, so `R CMD check` skips them; CI runs them from the source tree. A failing golden test means the numbers changed. Don't accept new snapshots in a refactor. See [AGENTS.md](../../AGENTS.md#numeric-regression-golden-tests) for the workflow.
