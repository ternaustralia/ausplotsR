# Error Handling Gaps — ausplotsR

> Rechecked against full codebase. Three original entries were wrong; two new
> functions added; and a critical finding identified that makes top-level
> `tryCatch` on callers **insufficient** without a prior fix inside
> `.ausplots_api()`.

---

## Summary table — what a top-level `tryCatch` would catch

| # | File | Function | Top-level `tryCatch` sufficient? | What it would catch | Status | Sentry capture |
|---|------|----------|----------------------------------|---------------------|--------|----------------|
| 1 | `R/ausplots_queries.r` | `.ausplots_api()` | **No** — root fix required first | Network errors, JSON parse failures, pagination crashes | 🟢 Done | 🟢 Done |
| 2 | `R/data_afa_static.R` | `get_afa_static()` | **No** — depends on #1 fix | API failure + column-rename crash | 🟢 Done | \ud83d\udfe2 Done |
| 3 | `R/get_nvis.R` | `get_nvis()` | **No** — depends on #1 fix | API failure + processing errors | 🟢 Done | 🟡 Pending |
| 4 | `R/data_trait_data_pp.R` | `get_trait_data_pp()` | **No** — depends on #1 fix | API failure + processing errors | 🟢 Done | 🟡 Pending |
| 5 | `R/basal_area.R` | `basal_area()` | **Yes** | Missing column errors from `plyr`/`aggregate` | 🟢 Done | 🟡 Pending |
| 6 | `R/fractional_cover.R` | `fractional_cover()` | **Partial** — silent wrong output uncatchable | Loop errors, missing columns; but all-NA fraction case produces wrong output silently | 🟢 Done | 🟡 Pending |
| 7 | `R/species_table.R` | `species_table()` | **Yes** | Missing column errors, `ma_ausplot_ma()` failures | 🟢 Done | 🟡 Pending |
| 8 | `R/growth_form_table.R` | `growth_form_table()` | **Partial** — silent wrong output uncatchable | Column-access errors; but empty-frame path produces wrong output silently | 🟢 Done | 🟡 Pending |
| 9 | `R/species_list.R` | `species_list()` | **Partial** — `append_family` silent NA uncatchable | `write.table()` failures when `writefile=TRUE` | 🟢 Done | 🟡 Pending |
| 10 | `R/invasion_info.R` | `invasion_info()` | **Yes** | `APCalign::align_taxa()` failures | 🟢 Done | 🟡 Pending |
| 11 | `R/biodiversity_metrics.R` | `biodiversity_metrics()` | **Yes** | Errors propagating up from `species_table()` | 🟢 Done | 🟡 Pending |
| 12 | `R/species_traits.R` | `species_traits()` | **No** — depends on #1 fix | `get_trait_data_pp()` returning bogus data silently | 🟢 Done | 🟡 Pending |
| 13 | `R/community_traits.R` | `community_traits()` | **No** — depends on #1 fix | Same as above | 🟢 Done | 🟡 Pending |
| 14 | `R/data_NVIS.R` | *(static dataset, no function)* | **N/A** | — | 🟢 Done | N/A |
| 15 | `R/ausplots_trajectory.R` | `ausplots_trajectory()` | **Yes** | PCoA/diversity computation errors | 🟢 Done | 🟡 Pending |
| 16 | `R/ausplots_visual.R` | `ausplots_visual()` | **Yes** | Errors from any of the sub-plot functions | 🟢 Done | 🟡 Pending |
| 17 | `R/cumulative_cover.R` | `cumulative_cover()` | **Yes** | Column-access errors on `veg.PI` | 🟢 Done | 🟡 Pending |
| 18 | `R/fraction_pie.R` | `fraction_pie()` | **Yes** | Bad input to plotting calls | 🟢 Done | 🟡 Pending |
| 19 | `R/growthform_pie.R` | `growthform_pie()` | **Yes** | Bad input to plotting calls | 🟢 Done | 🟡 Pending |
| 20 | `R/map_attribute.R` | `map_attribute()` | **Yes** | Map/ggplot errors | 🟢 Done | 🟡 Pending |
| 21 | `R/map_ausplots.R` | `map_ausplots()` | **Yes** | Map/ggplot errors | 🟢 Done | 🟡 Pending |
| 22 | `R/point_intercept_visual.R` | `point_intercept_visual()` | **Yes** | Plot/column-access errors | 🟢 Done | 🟡 Pending |
| 23 | `R/set_auth.R` | `set_auth()` | **Yes** | Credential storage errors | 🟢 Done | 🟡 Pending |
| 24 | `R/ma_ausplot_ma.R` | `ma_ausplot_ma()` | **Yes** | Internal reshape errors | 🟢 Done | 🟡 Pending |
| 25 | `R/single_cover_value.R` | `single_cover_value()` | **Yes** | Column-access errors on `veg.PI` | 🟢 Done | 🟡 Pending |
| 26 | `R/species_optimisation.R` | `optim_species()` | **Yes** | Optimisation/matrix computation errors | 🟢 Done | 🟡 Pending |
| 27 | `R/trajectoryPlots_ausplots.R` | `trajectoryPCoA_ausplots()` | **Yes** | Distance/ordination computation errors | 🟢 Done | 🟡 Pending |
| 28 | `R/whitt.plot.R` | `whitt.plot()` | **Yes** | Plot errors on abundance vector | 🟢 Done | 🟡 Pending |

---

## Critical root cause

### `.ausplots_api()` paginated path — `try()` silently swallows the error

`R/ausplots_queries.r`, pagination path:

```r
result <- try(jsonlite::fromJSON(finalResp, simplifyDataFrame = TRUE))
return(result)   # ← no inherits(result, "try-error") check
```

`try()` catches the error and stores it as a `try-error` object, then
`return(result)` hands that object to every caller. No condition is ever raised,
so a `tryCatch` in the caller sees nothing. Compare: the non-paginated path
below correctly does:

```r
result <- try(jsonlite::fromJSON(...))
if(inherits(result, "try-error")) {
  stop("Data extraction aborted due to database connection issue.")
}
```

**This missing `inherits` check is the root cause of all "bogus data" failures
in `get_afa_static()`, `get_nvis()`, `get_trait_data_pp()`, `species_traits()`,
and `community_traits()`.**

When `.ausplots_api()` returns a `try-error` object, `as.data.frame(try-error)`
doesn't throw — a `try-error` is a character vector, so it produces a 1-column
data frame containing the error message string. Every downstream function then
fails with a confusing column-not-found error, or silently produces wrong output.

---

## Detailed findings

### 1. `.ausplots_api()` — `R/ausplots_queries.r`
- **Root fix required here.** `try_GET()` (internal HTTP helper) is a bare
  `httr::GET` with no `tryCatch` — network/DNS/SSL errors propagate uncaught to
  every caller.
- `api_info()`: `content_length > 0` — `content_length` is `character` from
  `strsplit`; comparison coerces to `NA` if the `Content-Range` header is absent,
  causing `progress_bar$new(total=NA)` to crash.
- **Paginated path:** `try(jsonlite::fromJSON(finalResp))` catches the error into
  a `try-error` object but then `return(result)` unconditionally returns it to
  the caller with no message and no throw. Fix: add
  `if(inherits(result, "try-error")) stop(...)` before `return`.

### 2. `get_afa_static()` — `R/data_afa_static.R`
- No guard on `.ausplots_api()` return. After the root fix in #1, a top-level
  `tryCatch` here becomes sufficient.

### 3. `get_nvis()` — `R/get_nvis.R`
- Same as #2.

### 4. `get_trait_data_pp()` — `R/data_trait_data_pp.R`
- Same as #2.

### 5. `basal_area()` — `R/basal_area.R`
- No check that `veg.basal` contains required columns (`site_unique`, `point_id`,
  `basal_area`, `hits`, `standardised_name`, `herbarium_determination`,
  `genus_species`, `genus`). `plyr::count()` and `stats::aggregate()` throw
  cryptic internal errors if any are missing. A top-level `tryCatch` is sufficient
  to catch these.

### 6. `fractional_cover()` — `R/fractional_cover.R`
- No column existence check for `growth_form`, `in_canopy_sky`, `substrate`,
  `dead`, `hits_unique`, `height` — column-access errors are catchable.
- **Uncatchable silent gap:** if all hits have non-standard substrate codes, all
  entries in `fraction` remain `NA`. The function still returns a matrix with
  100% "other" — no error, no warning, just wrong output. Needs an explicit check
  or `warning()` after the classification loop.

### 7. `species_table()` — `R/species_table.R`
- No check that `veg.PI` contains `site_unique`, `standardised_name`,
  `herbarium_determination`, `hits_unique`, `in_canopy_sky`. Column-access errors
  are catchable with a top-level `tryCatch`.

### 8. `growth_form_table()` — `R/growth_form_table.R`
- Validates `growth_form` and `site_unique` but not `in_canopy_sky`, `hits_unique`,
  `standardised_name`, `height`. Column-access errors are catchable.
- **Corrected from original analysis:** `form_rows[-which(duplicated(form_rows)),]`
  when `form_rows` is 0 rows: `which(logical(0))` → `integer(0)` →
  `df[-integer(0),]` returns the same 0-row frame (no crash). A 0-row frame
  through `ma_ausplot_ma()` → `reshape()` produces a 0-column empty matrix
  silently — **wrong output, not an error**. Not catchable by `tryCatch`.

### 9. `species_list()` — `R/species_list.R`
- `write.table()` in the `writefile=TRUE` branch (bad path, permissions) — no
  `tryCatch`. Catchable.
- **Uncatchable silent gap:** in the `append_family=TRUE` path,
  `veg.vouch[match(x, veg.vouch$standardised_name), "family"]` silently produces
  all-NA values if the `family` column is absent or has no matching rows. No
  error is raised.

### 10. `invasion_info()` — `R/invasion_info.R`
- **Corrected from original analysis:** `requireNamespace("APCalign",
  quietly=TRUE)` guard **already exists** at the top of the function.
- Real remaining gap: `APCalign::align_taxa()` is called with no `tryCatch` — any
  failure from the alignment (network, schema change, unexpected input) is
  uncaught. A top-level `tryCatch` is sufficient to catch this.

### 11. `biodiversity_metrics()` — `R/biodiversity_metrics.R`
- `species_table()` called with no `tryCatch` — column errors from inside
  `species_table()` surface with no context linking back to
  `biodiversity_metrics()`. Top-level `tryCatch` is sufficient.

### 12. `species_traits()` — `R/species_traits.R` *(new)*
- Calls `get_trait_data_pp()` at line 18 without any guard. Has a downstream
  check `if (!"species_name" %in% names(trait_table)) stop(...)`, which covers the
  case where the API throws, but **not** the case where `.ausplots_api()` returns
  a `try-error` object disguised as a 1-column data frame (produces a confusing
  column-not-found error). Depends on the root fix in #1.

### 13. `community_traits()` — `R/community_traits.R` *(new)*
- Same pattern as `species_traits()`. Has `if (!is.data.frame(trait_table))
  stop(...)` which catches a thrown error but not a silently bogus data frame.
  Depends on the root fix in #1.
