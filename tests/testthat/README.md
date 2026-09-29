# tidywater tests

There are two kinds of tests here.

- **`test-<function>.R`**: behavior, errors, warnings and **reference checks**. A reference check compares against literature, WTP model, WaterPro, TELSS, spreadsheets or hand calculations. These values come from outside the package. Never change one just to make a refactor pass.
- **`test-regression-<topic>.R`**: numeric **regression (golden) tests**. These pin the package's *current* output, every slot of every water, over a wide range of inputs and options. They are not validation. Their job is to guarantee that a refactor doesn't change results.

## How golden tests work

`expect_golden(cases, name)` (in `helper-regression.R`) flattens a named list of outputs into one line per value:

```
typical :: ph = 7.5
typical :: hco3 = 0.00159295511741411
```

It stores the result as `_snaps/regression-<topic>/<name>.txt`. Later runs compare each number with `|new - old| <= abs_tol + rel_tol * |old|`. The defaults are `rel_tol = 1e-8` and `abs_tol = 0`. Text and `NA` must match exactly, and adding or removing a line counts as a failure. When something fails, the message lists each value that changed, old then new.

Tolerances are looser where a solver sets the precision:

| Model | Tolerance |
|---|---|
| Closed-form models, and `uniroot` at `tol = 1e-14` (`solve_ph`, `chemdose_ph`, `chemdose_chlordecay`) | default |
| `uniroot` at its default tolerance (`calculate_corrosion` CCPP, `opensys_ph`) | `rel_tol = 1e-4` plus a small `abs_tol` |
| Chloramine ODE (`chemdose_chloramine`) | `rel_tol = 1e-6`, `abs_tol = 1e-12` mol/L |

`regression_waters()` provides a standard set of scenario waters. Their own outputs are pinned in `test-regression-define_water.R`, so a failure further down the chain can be traced back to its input.

## Workflow

- **Refactoring** (no intended change in results): all golden tests must pass unchanged. **Do not regenerate or edit snapshot files in a refactor PR.** If a golden test fails, the refactor changed the numbers. Fix the code, not the snapshot.
- **Intended change in results** (a bug fix, a model update): run the tests, check the diffs, then accept them with `testthat::snapshot_accept("regression-<topic>/<name>.txt")` or `testthat::snapshot_review()`. The PR description must say which goldens changed and why.
- **New function or option**: add cases to the matching `test-regression-*.R` file. Running it creates the golden. Commit the new `.txt` file. On CI a missing snapshot is an error, not a silent pass.

Snapshots are skipped on CRAN (the testthat default). They run locally and on CI.

## Known bugs

Some cases deliberately pin behavior that is known to be wrong. So that fixing a bug is a deliberate, reviewed change to the numbers, these cases carry:

- a `# KNOWN-BUG #<issue>` comment in the test file;
- where a case exists specifically to show the bug, a `_knownbug<issue>` suffix on its name, so it shows up in the golden file.

Search for `KNOWN-BUG #<issue>` to find every test an issue touches.

When you fix the issue, accept the new values for those cases, drop the suffix and the comment, and check that nothing else moved.
