# Work Log — `trajectoryPlots_ausplots.R` Error Handling

> End-to-end record of every step taken to find, test, fix, and document all
> error-handling gaps in `R/trajectoryPlots_ausplots.R`.

---

## Step 1 — Investigate all gaps in `trajectoryPlots_ausplots.R`

**Goal:** Identify every place where an error is not caught, silently swallowed,
or produces wrong output instead of raising.

**Source:** [`error_handling_gaps.md`](error_handling_gaps.md) — entry #27.

**Findings:**

| Location | Problem |
|----------|---------|
| `as.dist(as.matrix(d)[…, …])` — NULL `d` or mismatched sizes | Crashes uncaught with cryptic matrix error |
| `cmdscale(D2, …, k=nrow(as.matrix(D2))-1)` | Degenerate distance matrix → `cmdscale` crash propagates uncaught |
| `cmd_D2$points[,axes[1]]` / `[,axes[2]]` | If `cmdscale` produces fewer columns than `axes[2]`, subscript crash uncaught |
| `plot(x, y, …)` / `arrows(…)` | Graphics device errors propagate uncaught |
| `trajectoryPlot_ausplots`: `x[sites %in% selIDs, axes[1]]` on NULL | NULL `x` → subscript crash uncaught |

**Note:** Both functions make no API calls. A top-level `tryCatch` on each is
fully sufficient.

---

## Step 2 — Create possible test cases for the gaps found

**No existing test file found.** Tests focus on NULL inputs.

**Test file:** `tests/testthat/test-trajectoryPlots_ausplots.R`

**Tests planned:**

| Test name | Gap it covers | Caught or silent? |
|-----------|---------------|-------------------|
| `"trajectoryPCoA_ausplots: NULL d is caught and wrapped"` | NULL d → crash | Caught |
| `"trajectoryPCoA_ausplots: mismatched d and sites is caught and wrapped"` | Size mismatch → crash | Caught |
| `"trajectoryPCoA_ausplots: wrapped error preserves original cause"` | Original message survives | Caught |
| `"trajectoryPlot_ausplots: NULL x is caught and wrapped"` | NULL x → crash | Caught |

**Mock pattern used:** None — all triggered by invalid inputs.

---

## Step 3 — Root fix

No root fix required. Neither function calls `.ausplots_api()`.

---

## Step 4 — Wrap both function bodies in `tryCatch`; add all tests

**Fix applied to `R/trajectoryPlots_ausplots.R`:**

```r
trajectoryPCoA_ausplots <- function(d, sites, …) {
  tryCatch({
    # ... entire original body unchanged ...
  }, error = function(e) {
    stop(paste0("Failed to generate trajectory PCoA plot: ", conditionMessage(e)), call. = FALSE)
  })
}

trajectoryPlot_ausplots <- function(x, sites, …) {
  tryCatch({
    # ... entire original body unchanged ...
  }, error = function(e) {
    stop(paste0("Failed to generate trajectory plot: ", conditionMessage(e)), call. = FALSE)
  })
}
```

**Tests added to `tests/testthat/test-trajectoryPlots_ausplots.R`:** 4 tests (0
happy path, 4 error path).

---

## Step 5 — Run tests

All 4 tests pass:

```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 4 ]
```

---

## Step 6 — Update documentation

- [`error_coverage.md`](error_coverage.md) — new `trajectoryPlots_ausplots.R`
  section added.
- [`error_handling_gaps.md`](error_handling_gaps.md) — row #27 updated from
  🟡 Warn to 🟢 Done.
