# Work Log — `map_attribute.R` Error Handling

> End-to-end record of every step taken to find, test, fix, and document all
> error-handling gaps in `R/map_attribute.R`.

---

## Step 1 — Investigate all gaps in `map_attribute.R`

**Goal:** Identify every place where an error is not caught, silently swallowed,
or produces wrong output instead of raising.

**Source:** [`error_handling_gaps.md`](error_handling_gaps.md) — entry #21.

**Findings:**

| Location | Problem |
|----------|---------|
| `my.ausplots.object$veg.PI$growth_form` | If `my.ausplots.object` is NULL or missing `$veg.PI`, `$growth_form` access throws with no context |
| `ggplot2::map_data("world2Hires", "Australia")` | If `mapdata` package is missing or data unavailable, throws with no context |
| `single_cover_value(my.ausplots.object$veg.PI)` | Any failure in `single_cover_value()` propagates uncaught |
| `merge(…)` / column access | Missing `site_unique`, `longitude`, `latitude` columns crash with no context |
| `ggplot2::ggplot(…)` / `geom_*` calls | Any ggplot error propagates uncaught |

**Note:** `map_attribute()` makes no API calls. A top-level `tryCatch` is fully
sufficient for all catchable error paths. No root fix dependency required.

---

## Step 2 — Create possible test cases for the gaps found

**No existing test file found.** Tests written in two groups: happy-path first,
then error-path.

**Test file:** `tests/testthat/test-map_attribute.R`

**Tests planned:**

| Test name | Gap it covers | Caught or silent? |
|-----------|---------------|-------------------|
| `"map_attribute: no-tree input skips plotting without error"` | Happy path — no trees guard | Happy path |
| `"map_attribute: NULL input is caught and wrapped"` | NULL object → `$veg.PI` crash | Caught |
| `"map_attribute: wrapped error preserves original cause"` | Original message survives | Caught |
| `"map_attribute: missing veg.PI is caught and wrapped"` | Missing `$veg.PI` | Caught |

**Mock pattern used:** None — `map_attribute()` takes in-memory objects.

---

## Step 3 — Root fix

No root fix required. `map_attribute()` does not call `.ausplots_api()`.

---

## Step 4 — Wrap `map_attribute()` body in `tryCatch`; add all tests

**Fix applied to `R/map_attribute.R`:**

```r
map_attribute <- function(my.ausplots.object) {
  tryCatch({
    # ... entire original body unchanged ...
  }, error = function(e) {
    stop(paste0("Failed to generate attribute map: ", conditionMessage(e)), call. = FALSE)
  })
}
```

**Tests added to `tests/testthat/test-map_attribute.R`:** 4 tests (1 happy path,
3 error path).

---

## Step 5 — Run tests

All 4 tests pass:

```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 4 ]
```

---

## Step 6 — Update documentation

- [`error_coverage.md`](error_coverage.md) — new `map_attribute.R` section added.
- [`error_handling_gaps.md`](error_handling_gaps.md) — row #21 updated from
  🟡 Warn to 🟢 Done.
