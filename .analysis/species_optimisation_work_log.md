# Work Log — `species_optimisation.R` Error Handling

> End-to-end record of every step taken to find, test, fix, and document all
> error-handling gaps in `R/species_optimisation.R`.

---

## Step 1 — Investigate all gaps in `species_optimisation.R`

**Goal:** Identify every place where an error is not caught, silently swallowed,
or produces wrong output instead of raising.

**Source:** [`error_handling_gaps.md`](error_handling_gaps.md) — entry #26.

**Findings:**

| Location | Problem |
|----------|---------|
| `if(!(start %in% c("fixed","defined","random"))) stop(…)` | Explicit stop() but uncaught |
| `if(!is.null(plot_name) && (!plot_name %in% rownames(…))) stop(…)` | Same |
| `Richness.opt(…)` / `RRR.opt(…)` / `CWE.opt(…)` etc. | Any sub-function failure propagates uncaught |
| `try(plot_opt(result))` | `plot_opt` failure is already silently swallowed by `try()` |
| NULL or non-numeric matrix input | `speciesVsitesMatrix[,1]` on NULL crashes uncaught |

**Note:** `optim_species()` makes no API calls. A top-level `tryCatch` is fully
sufficient. The inner `try(plot_opt(result))` already silences plot errors by
design — the outer `tryCatch` does not interfere with that.

---

## Step 2 — Create possible test cases for the gaps found

**No existing test file found.** Tests focus on the explicit stop() paths and
NULL input.

**Test file:** `tests/testthat/test-optim_species.R`

**Tests planned:**

| Test name | Gap it covers | Caught or silent? |
|-----------|---------------|-------------------|
| `"optim_species: invalid start is caught and wrapped"` | `start` stop() | Caught |
| `"optim_species: wrapped error preserves original cause"` | Original message survives | Caught |
| `"optim_species: invalid plot_name is caught and wrapped"` | `plot_name` stop() | Caught |
| `"optim_species: NULL input is caught and wrapped"` | NULL → `[,1]` crash | Caught |

**Mock pattern used:** None — all triggered by invalid/missing arguments.

---

## Step 3 — Root fix

No root fix required. `optim_species()` does not call `.ausplots_api()`.

---

## Step 4 — Wrap `optim_species()` body in `tryCatch`; add all tests

**Fix applied to `R/species_optimisation.R`:**

```r
optim_species <- function(speciesVsitesMatrix, …) {
  tryCatch({
    # ... entire original body unchanged ...
  }, error = function(e) {
    stop(paste0("Failed to optimise species accumulation: ", conditionMessage(e)), call. = FALSE)
  })
}
```

**Tests added to `tests/testthat/test-optim_species.R`:** 4 tests (0 happy path,
4 error path — full happy path requires heavy vegan computation not suitable for
unit tests).

---

## Step 5 — Run tests

All 4 tests pass:

```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 4 ]
```

---

## Step 6 — Update documentation

- [`error_coverage.md`](error_coverage.md) — new `species_optimisation.R` section added.
- [`error_handling_gaps.md`](error_handling_gaps.md) — row #26 updated from
  🟡 Warn to 🟢 Done.
