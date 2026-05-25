# Work Log — `ausplots_visual.R` Error Handling

> End-to-end record of every step taken to find, test, fix, and document all
> error-handling gaps in `R/ausplots_visual.R`.

---

## Step 1 — Investigate all gaps in `ausplots_visual.R`

**Goal:** Identify every place where an error is not caught, silently swallowed,
or produces wrong output instead of raising.

**Source:** [`error_handling_gaps.md`](error_handling_gaps.md) — entry #16.

**Findings:**

| Location | Problem |
|----------|---------|
| `get_ausplots()` call (when `my.ausplots.object` missing) | Network/API failure propagates completely uncaught to the caller |
| `fractional_cover(veg.PI)` | Already wrapped in `tryCatch` internally, but a failure still propagates up to `ausplots_visual()` since it is called without a surrounding `try()` |
| `growth_form_table(veg.PI, ...)` | Same as above — called without `try()` |
| `species_table(veg.PI, ...)` | Same as above |
| `fraction_pie(frac, i)` | Called inside `try()` — errors silently swallowed; `try-error` object returned and ignored |
| `growthform_pie(GF[i,], GF_col)` | Same — `try()` wraps it; errors silently swallowed |
| `cumulative_cover(subset(...))` | Same — `try()` wraps it |
| `whitt.plot(sppBYsites[i,])` | Same — `try()` wraps it |
| `map_ausplots(my.ausplots.object)` | Called without `try()` — map/ggplot errors propagate uncaught |
| `map_attribute(my.ausplots.object)` | Same |
| `pdf(outfile, ...)` | If `outfile` path is unwritable, `pdf()` throws; propagates uncaught |
| Explicit validation `stop()` calls at top | Already present (all outputs FALSE, missing veg.PI, missing site.info) but propagate undecorated to caller |

**Note:** `ausplots_visual()` does not call `.ausplots_api()` directly — it
delegates to `get_ausplots()` when `my.ausplots.object` is absent. A top-level
`tryCatch` is fully sufficient for all catchable error paths. No root fix
dependency required.

**Note on `try()` sub-plots:** The `try()` calls around individual site plots
(`fraction_pie`, `growthform_pie`, `cumulative_cover`, `whitt.plot`) swallow
errors silently and return `try-error` objects that are never checked. This is
an intentional design choice in the original code — failed plots for one site
should not abort the whole visualisation. The outer `tryCatch` does not change
this behaviour; it only catches errors from the setup and orchestration code
that runs outside of those `try()` blocks.

---

## Step 2 — Create possible test cases for the gaps found

**Goal:** Write tests for each gap — both explicit-stop paths (caught by tryCatch)
and the `get_ausplots()` propagation path (mocked).

**No existing test file found.** Tests focus on the error paths that tryCatch
now covers. Happy-path tests for `ausplots_visual()` are not included because
they require a full API call or mocking of all downstream plot functions, which
is disproportionately complex relative to the coverage gain.

**Test file:** `tests/testthat/test-ausplots_visual.R`

**Tests planned:**

| Test name | Gap it covers | Caught or silent? |
|-----------|---------------|-------------------|
| `"ausplots_visual: all outputs FALSE is caught and wrapped"` | Explicit validation `stop()` | Caught |
| `"ausplots_visual: missing veg.PI is caught and wrapped"` | Explicit validation `stop()` | Caught |
| `"ausplots_visual: missing site.info is caught and wrapped"` | Explicit validation `stop()` | Caught |
| `"ausplots_visual: get_ausplots failure is caught and wrapped"` | `get_ausplots()` propagation | Caught |

**Mock pattern used for `get_ausplots`:**

```r
mockery::stub(
  ausplots_visual, "get_ausplots",
  function(...) stop("network failure")
)
```

---

## Step 3 — Root fix

No root fix required. `ausplots_visual()` orchestrates existing functions; it
does not have an equivalent of the `.ausplots_api()` silent-error propagation
path. A top-level `tryCatch` is fully sufficient.

---

## Step 4 — Wrap `ausplots_visual()` body in `tryCatch`; add all tests

**Fix applied to `R/ausplots_visual.R`:**

```r
ausplots_visual <- function(my.ausplots.object = NULL, map = TRUE,
                            map.attribute = TRUE, fraction.pie = TRUE,
                            growthform.pie = TRUE, cumulative.cover = TRUE,
                            whittaker = TRUE, outfile = NULL, max.plots = 5) {
  tryCatch({

    #############################
    #reset graphical defaults
    oldpar <- par(no.readonly = TRUE)
    on.exit(suppressWarnings(par(oldpar)))

    # ... entire original body unchanged ...

  }, error = function(e) {
    stop(paste0("Failed to generate ausplots visualisations: ", conditionMessage(e)), call. = FALSE)
  })
} #end function
```

**Design decisions:**
- Same as `ausplots_trajectory.R`: `on.exit()` inside tryCatch but registered
  on the function frame, so par reset always fires.
- Validation `stop()` calls already in the function are caught and re-wrapped
  with the prefix. Existing error messages are preserved via `conditionMessage`.
- The `try()` wrappers around individual site plots are intentionally left
  unchanged — they implement per-site fault tolerance, not function-level
  error handling.
- `call. = FALSE` keeps internal function names out of the user-visible error.

**All 4 tests created in `tests/testthat/test-ausplots_visual.R`** (new file):
- 0 happy-path tests (skipped — require full API + plot infrastructure)
- 4 caught-path tests (all outputs FALSE, missing veg.PI, missing site.info,
  get_ausplots failure via mock)

---

## Step 5 — Recheck all findings against `error_handling_gaps.md`

| Gap entry | Status after fix |
|-----------|-----------------|
| `get_ausplots()` network failure | ✅ Caught by `tryCatch`. Test exists (mocked). |
| `fractional_cover()` failure | ✅ Caught by `tryCatch`. (No separate test — fractional_cover has its own tests) |
| `growth_form_table()` failure | ✅ Caught by `tryCatch`. (No separate test) |
| `species_table()` failure | ✅ Caught by `tryCatch`. (No separate test) |
| `fraction_pie/growthform_pie/cumulative_cover/whitt.plot` via `try()` | ⚠️ Silent — `try()` swallows; outer tryCatch does not change this | Documented |
| `map_ausplots/map_attribute` failure | ✅ Caught by `tryCatch`. (No separate test) |
| `pdf(outfile)` write failure | ✅ Caught by `tryCatch`. (No separate test) |
| Explicit validation `stop()` calls | ✅ Caught by `tryCatch`. 3 tests exist. |

**Conclusion:** All non-try() gaps for `ausplots_visual.R` are covered. The
per-site `try()` swallowing is intentional fault tolerance, not a gap.

> **Template note:** After final rechecking, **`ausplots_visual_work_log.md`**
> is created with all the proposed steps and order exactly.

---

## Step 6 — Update `error_coverage.md` with full `ausplots_visual.R` information

**Section added to `.analysis/error_coverage.md`:**

| # | Rows | Notes |
|---|------|-------|
| 1–4 | Caught by tryCatch | Explicit validation stops + get_ausplots failure |
| 5–8 | Silent — `try()` swallows | fraction_pie, growthform_pie, cumulative_cover, whitt.plot |
