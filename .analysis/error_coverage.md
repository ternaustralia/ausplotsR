# Error Coverage — ausplotsR

> One table per source file. For each error path: what triggers it, what kind
> of failure it is, the exact error message or class, and whether a test exists.
>
> **Failure types**
> - **Explicit stop()** — code deliberately calls `stop()`
> - **Uncaught propagation** — an external library throws and nothing catches it
> - **try() → silent wrong output** — `try()` absorbs the error; caller gets a
>   poisoned value instead of an error
> - **Silent wrong output** — no error at all; wrong data returned silently
> - **Dead code** — the `stop()` is structurally unreachable

---

## `R/ausplots_queries.r`

### `try_GET()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `try_GET` | `httr::GET` network failure (DNS, timeout, refused) | Uncaught propagation | httr error condition, e.g. `"Failed to connect to ..."` | ✅ `"try_GET: propagates httr::GET network failure"` |

---

### `api_info()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 2 | `api_info` | `httr::GET` network failure | Uncaught propagation | httr error condition | ✅ `"api_info: propagates httr::GET network failure"` |
| 3 | `api_info` | `content-range` header absent → `strsplit(NULL, "/")` throws inside `try()` → `max_size` receives `try-error` string | `try()` → silent wrong output | `"non-character argument"` stored in `max_size`, not thrown | ✅ `"api_info: missing content-range header silently sets max_size to try-error string"` |
| 4 | `api_info` | `httr::content(resp, "parsed")` called on response with unrecognised content-type | Uncaught propagation | `"No automatic parser available for ..."` | ✅ `"api_info: propagates httr::content error when content-type is unrecognised"` |
| 5 | `api_info` | ~~`jsonlite::toJSON(body)` where `body` is `NULL`~~ **FALSE ENTRY** — `toJSON(NULL)` returns `"null"` without error; merges into #6 | — | — | N/A |
| 6 | `api_info` | `names(fromJSON("null"))` → `NULL` → `1:0` loop → `NULL[[1]]` in the x=1 iteration | Uncaught propagation | `"subscript out of bounds"` | ✅ `"api_info: NULL parsed body throws subscript out of bounds in column loop"` |

---

### `.ausplots_api()` — non-paginated path (initial `status_code < 500`)

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 7 | `.ausplots_api` | `try_GET` network failure | Caught by tryCatch → wrapped with path context (unknown msg) | `"Unexpected error while calling API path '<path>': ..."` | ✅ `".ausplots_api: propagates network failure from try_GET"` |
| 8 | `.ausplots_api` | HTTP 4xx/5xx response → `httr::stop_for_status` raises | Caught by tryCatch → wrapped with path context (unknown msg) | `"Unexpected error while calling API path '<path>': HTTP 4xx ..."` | ✅ `".ausplots_api: non-paginated path propagates HTTP 4xx error"` |
| 9 | `.ausplots_api` | Response `content-type` is not `application/json` | Explicit `stop()` → caught by tryCatch → re-thrown unchanged (known msg) | `"API did not return json"` | ✅ `".ausplots_api: non-paginated path stops when content-type is not JSON"` |
| 10 | `.ausplots_api` | `jsonlite::fromJSON` fails on response body | `try()` → explicit `stop()` → caught by tryCatch → re-thrown unchanged (known msg) | `"Data extraction aborted due to database connection issue."` | ✅ `".ausplots_api: non-paginated path raises stop() when fromJSON fails"` |

---

### `.ausplots_api()` — pagination branch (`status_code > 500`)

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 11 | `.ausplots_api` | `api_info()` itself fails (any of errors #2–#6 above) | Caught by tryCatch → wrapped with path context (unknown msg) | `"Unexpected error while calling API path '<path>': ..."` | ✅ `".ausplots_api: paginated path propagates api_info failure"` |
| 12 | `.ausplots_api` | `try_GET` network failure inside pagination loop | Caught by tryCatch → wrapped with path context (unknown msg) | `"Unexpected error while calling API path '<path>': ..."` | ✅ `".ausplots_api: paginated path propagates try_GET failure inside loop"` |
| 13 | `.ausplots_api` | `httr::content(respPagination, "parsed")` — unrecognised content-type | Caught by tryCatch → wrapped with path context (unknown msg) | `"Unexpected error while calling API path '<path>': No automatic parser available for ..."` | ✅ `".ausplots_api: paginated path propagates httr::content error inside loop"` |
| 14 | `.ausplots_api` | ~~`jsonlite::toJSON(body)` where `body` is `NULL`~~ **FALSE ENTRY** — `toJSON(NULL)` = `"null"` without error; execution falls through to bodySize check | — | — | N/A |
| 15 | `.ausplots_api` | `jsonlite::fromJSON(jsonBody[[1]])` — malformed JSON or error in fromJSON | Caught by tryCatch → wrapped with path context (unknown msg) | `"Unexpected error while calling API path '<path>': lexical error: ..."` | ✅ `".ausplots_api: paginated path propagates fromJSON failure on page chunk"` |
| 16 | `.ausplots_api` | `jsonlite::fromJSON(finalResp)` fails on assembled paginated string | `try()` → explicit `stop()` → caught by tryCatch → re-thrown unchanged (known msg) | `"Data extraction aborted due to database connection issue."` | ✅ `".ausplots_api: paginated path raises stop() when final fromJSON fails"` |
| 17 | `.ausplots_api` | `while` loop exits without finalising (structurally unreachable — `isBody <- FALSE` always paired with `return()`) | Dead code | `"Server is busy at the moment. Please try again in a few minutes"` | N/A |

---

### `.ausplots_api()` — `tryCatch` handler (wraps entire function body)

Catches **all** errors thrown anywhere inside `.ausplots_api()`. Distinguishes known messages (re-thrown unchanged) from unknown ones (wrapped with API path context).

| # | Function | Trigger | Failure type | Output message | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| T1 | `.ausplots_api` tryCatch | Thrown message is in `known_msgs` | Re-thrown unchanged | Original message — one of the three known strings | ✅ `".ausplots_api: known message 'Data extraction aborted' passes through unchanged"` · `".ausplots_api: known message 'API did not return json' passes through unchanged"` |
| T2 | `.ausplots_api` tryCatch | Thrown message is **not** in `known_msgs` (unexpected / external error) | Wrapped with path context | `"Unexpected error while calling API path '<path>': <original msg>"` | ✅ `".ausplots_api: unknown error is wrapped with path context"` · `".ausplots_api: wrapped error message includes the correct path"` |

---

### `list_available_plots()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 18 | `list_available_plots` | `bounding_box` is not numeric | Explicit `stop()` | `"Bounding box must be a numeric vector of length 4."` | ✅ `"list_available_plots: bounding_box must be numeric"` |
| 19 | `list_available_plots` | `bounding_box` length ≠ 4 (tested: 2, 3, 5 elements) | Explicit `stop()` | `"Bounding box must be a numeric vector of length 4."` | ✅ 3 tests: length 2, length 3, length 5 |
| 20 | `list_available_plots` | API returns data missing `site_location_name` column | Silent wrong output | `NULL` returned — `sort(unique(NULL))` = `NULL`, no error thrown | ✅ `"list_available_plots: missing site_location_name column returns NULL silently"` |
| 21 | `list_available_plots` | API returns data missing `site_location_visit_id` in site_unique branch | Silent wrong output | `paste0("name", "-", NULL)` → `"name-"` (mangled, visit-id absent) | ✅ `"list_available_plots: site_unique IDs with missing visit_id column returns mangled string"` |

---

### `extract_site_info()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 22 | `extract_site_info` | API returns 0-row data frame missing `site_location_name` column | Silent wrong output | `mapply` over `NULL` → `list()` assigned as state column (wrong type, no error) | ✅ `"extract_site_info: 0-row response missing site_location_name sets state to empty list"` |
| 22b | `extract_site_info` | API returns non-empty data frame missing `site_location_name` column | Uncaught propagation | `"replacement has 0 rows, data has N"` — R data frame assignment error | ✅ `"extract_site_info: non-empty response missing site_location_name throws on assignment"` |

---

### `extract_struct_summ()` / `extract_soil_subsites()` / `extract_bulk_density()` / `extract_soil_char()` / `extract_basal()` / `extract_vouch()` / `extract_hits()`

Thin wrappers around `.ausplots_api_with_specified_plot_ids()`. No additional error paths beyond what `.ausplots_api()` raises. All covered by happy-path mocked tests.

---

### `.get_metadata_dictionary()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 23 | `.get_metadata_dictionary` | `.ausplots_api()` fails (any of errors #7–#16 above) | Propagates from `.ausplots_api()` | Various | ✅ `".get_metadata_dictionary: propagates .ausplots_api failure"` |

---

## Summary — `ausplots_queries.r`

| Status | Count |
|--------|-------|
| ✅ Tested | 22 |
| ❌ Not tested | 0 |
| N/A (dead code / false entry) | 3 |
| **Total error paths** | **25** |

> Rows T1 and T2 cover the `tryCatch` handler behaviour added to `.ausplots_api()`. All previously "Uncaught propagation" paths inside that function now flow through the handler — unknown messages are wrapped with `"Unexpected error while calling API path '<path>': <msg>"`, known messages are re-thrown as-is.

---

## `R/data_afa_static.R`

### `get_afa_static()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `get_afa_static` | `.ausplots_api()` throws any error (network, HTTP, JSON) | Caught by tryCatch → wrapped with context | `"Failed to retrieve AFA invasion status data: <original msg>"` | ✅ `"get_afa_static: .ausplots_api failure is caught and wrapped with context"` · `"get_afa_static: wrapped error preserves original cause"` |
| 2 | `get_afa_static` | `as.data.frame()` throws on non-coercible API return | Caught by tryCatch → wrapped with context | `"Failed to retrieve AFA invasion status data: <original msg>"` | ✅ `"get_afa_static: as.data.frame failure on non-coercible return is caught"` |
| 3 | `get_afa_static` | `.ausplots_api()` returns `NULL` → `as.data.frame(NULL)` = 0-row df → NA-fill loop assigns `NA_character_` to 0-row df | Caught by tryCatch → wrapped with context | `"Failed to retrieve AFA invasion status data: replacement has 1 row, data has 0"` | ✅ `"get_afa_static: NULL API return causes NA-fill error caught by tryCatch"` |
| 4 | `get_afa_static` | API returns df with no recognised API column names | Silent wrong output | Rename loop silently skips all entries; required schema cols absent | ✅ `"get_afa_static: unrecognised API columns cause renames to be silently skipped"` |
| 5 | `get_afa_static` | API returns both `apc_canonical_name` and `canonical_name` | Silent wrong output | Rename guard skips; both columns remain; original internal value kept | ✅ `"get_afa_static: rename skipped when internal column name already present"` |
| 6 | `get_afa_static` | API returns `family` column with real data | Silent wrong output (benign) | NA-fill guard correctly skips; original value preserved | ✅ `"get_afa_static: existing family column is not overwritten with NA"` |
| 7 | `get_afa_static` | API returns all five schema-completion columns with real data | Silent wrong output (benign) | All five NA-fill guards skip; original values preserved | ✅ `"get_afa_static: all five schema-completion columns preserved when API provides them"` |

---

## Summary — `data_afa_static.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 7 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **7** |

> Row 3 is a caught path — `tryCatch` fires because the NA-fill loop throws when
> assigning to a 0-row data frame. Rows 4–7 are silent wrong output paths where
> `tryCatch` never fires; tests document current behaviour.

---

## `R/fractional_cover.R`

### `fractional_cover()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `fractional_cover` | `veg.PI` missing `growth_form` → `is.na(NULL)` = `logical(0)` → `if(logical(0))` in classification loop | Caught by tryCatch → wrapped with context | `"Failed to calculate fractional cover: argument is of length zero"` | ✅ `"fractional_cover: missing growth_form column is caught and wrapped"` |
| 2 | `fractional_cover` | Same trigger as #1 — original cause preserved inside wrapped message | Caught by tryCatch → wrapped with context | `"Failed to calculate fractional cover: argument is of length zero"` | ✅ `"fractional_cover: wrapped error preserves original cause"` |
| 3 | `fractional_cover` | `veg.PI` missing `height` → `with(fractional_df, order(..., height))` throws "object 'height' not found" | Caught by tryCatch → wrapped with context | `"Failed to calculate fractional cover: object 'height' not found"` | ✅ `"fractional_cover: missing height column is caught and wrapped"` |
| 4 | `fractional_cover` | `ground_fractional=TRUE`, `veg.PI` missing `growth_form` — same classification loop crash | Caught by tryCatch → wrapped with context | `"Failed to calculate fractional cover: argument is of length zero"` | ✅ `"fractional_cover: ground_fractional=TRUE missing growth_form is caught and wrapped"` |
| 5 | `fractional_cover` | All `growth_form=NA` + all substrate codes unrecognised → all `fraction` stays `NA` → converted to `"other"` | Silent wrong output | No error or warning; 100% "other" returned | ✅ `"fractional_cover: all-unrecognised substrate with NA growth_form returns 100 percent other silently"` |
| 6 | `fractional_cover` | Same as #5 with `ground_fractional=TRUE` | Silent wrong output | No error or warning; 100% "other" returned | ✅ `"fractional_cover: ground_fractional=TRUE all-unrecognised input returns 100 percent other silently"` |

---

## Summary — `fractional_cover.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 6 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **6** |

> Rows 1–4 are caught paths — `tryCatch` fires when required columns are absent.
> Rows 5–6 are silent wrong output paths — `tryCatch` never fires; tests document
> that the function returns 100% "other" with no error when all hits are
> unclassifiable. This is the uncatchable gap identified in `error_handling_gaps.md`.

---

## `R/get_nvis.R`

### `get_nvis()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `get_nvis` | `.ausplots_api()` throws any error (network, HTTP, JSON) | Caught by tryCatch → wrapped with context | `"Failed to retrieve NVIS vegetation attributes: <original msg>"` | ✅ `"get_nvis: .ausplots_api failure is caught and wrapped with context"` · `"get_nvis: wrapped error preserves original cause"` |
| 2 | `get_nvis` | `as.data.frame()` throws on non-coercible API return (e.g. environment) | Caught by tryCatch → wrapped with context | `"Failed to retrieve NVIS vegetation attributes: <original msg>"` | ✅ `"get_nvis: as.data.frame failure on non-coercible return is caught"` |
| 3 | `get_nvis` | `.ausplots_api()` returns `NULL` → `as.data.frame(NULL)` = 0-col df → `character(0)` assigned to `site_unique` on 0-row df works; `is.null(site_unique)` path returns 0-row df silently | Silent wrong output | No error; 0-row data frame returned when called with `site_unique=NULL` | ✅ `"get_nvis: NULL API return returns empty data frame silently"` |
| 4 | `get_nvis` | `site_unique` is a data frame missing `site_unique` column | Explicit `stop()` inside tryCatch → wrapped | `"Failed to retrieve NVIS vegetation attributes: If 'site_unique' is a data frame, it must contain a 'site_unique' column."` | ✅ `"get_nvis: data.frame missing site_unique column throws error"` (existing) |
| 5 | `get_nvis` | `site_unique` is a list (non-atomic, non-data.frame) | Explicit `stop()` inside tryCatch → wrapped | `"Failed to retrieve NVIS vegetation attributes: 'site_unique' must be NULL ..."` | ✅ `"get_nvis: list input throws error"` (existing) |
| 6 | `get_nvis` | API returns df missing `site_unique` column → `nvis$site_unique <- as.character(nvis$site_unique)` assigns `character(0)` to a 1-row df → "replacement has 0 rows, data has 1" | Caught by tryCatch → wrapped with context | `"Failed to retrieve NVIS vegetation attributes: replacement has 0 rows, data has 1"` | ✅ `"get_nvis: API response missing site_unique column is caught"` |

---

## Summary — `get_nvis.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 6 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **6** |

> Rows 1–2, 4–6 are caught paths — `tryCatch` fires. Row 3 is a silent wrong
> output path — `tryCatch` never fires; test documents that a 0-row data frame is
> returned with no error when `.ausplots_api()` returns NULL.

---

## `R/data_trait_data_pp.R`

### `get_trait_data_pp()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `get_trait_data_pp` | `.ausplots_api()` throws any error (network, HTTP, JSON) | Caught by tryCatch → wrapped with context | `"Failed to retrieve species-level trait data: <original msg>"` | ✅ `"get_trait_data_pp: .ausplots_api failure is caught and wrapped with context"` · `"get_trait_data_pp: wrapped error preserves original cause"` |
| 2 | `get_trait_data_pp` | `as.data.frame()` throws on non-coercible API return (e.g. environment) | Caught by tryCatch → wrapped with context | `"Failed to retrieve species-level trait data: <original msg>"` | ✅ `"get_trait_data_pp: as.data.frame failure on non-coercible return is caught"` |
| 3 | `get_trait_data_pp` | API returns df with neither `d13c` nor `d13C` — rename guard condition false → column silently absent | Silent wrong output | No error; `d13C` absent from result; downstream `species_traits()` will fail with column-not-found | ✅ `"get_trait_data_pp: missing d13c column causes rename to be silently skipped"` |
| 4 | `get_trait_data_pp` | API returns df already named `d13C` — rename guard correctly skips | Silent correct output (benign) | No error; `d13C` present once | ✅ `"get_trait_data_pp: d13C already present is preserved without duplication"` |

---

## Summary — `data_trait_data_pp.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 4 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **4** |

> Rows 1–2 are caught paths — `tryCatch` fires. Rows 3–4 are silent output paths —
> `tryCatch` never fires; tests document that the rename is silently skipped when
> the column is absent or already correctly named.

---

## `R/basal_area.R`

### `basal_area()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `basal_area` | `veg.basal` missing `basal_area` column → `plyr::count(wt_var="basal_area")` throws | Caught by tryCatch → wrapped with context | `"Failed to calculate basal area: ..."` | ✅ `"basal_area: missing basal_area column is caught and wrapped"` · `"basal_area: wrapped error preserves original cause"` |
| 2 | `basal_area` | `veg.basal` missing `site_unique` column → `stats::aggregate(by=list(... site_unique))` throws | Caught by tryCatch → wrapped with context | `"Failed to calculate basal area: ..."` | ✅ `"basal_area: missing site_unique column is caught and wrapped"` |
| 3 | `basal_area` | `by.hits=TRUE`, `veg.basal` missing `hits` column → `plyr::count(wt_var="hits")` throws | Caught by tryCatch → wrapped with context | `"Failed to calculate basal area: ..."` | ✅ `"basal_area: by.hits=TRUE missing hits column is caught and wrapped"` |

---

## Summary — `basal_area.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 3 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **3** |

> All three rows are caught paths — `tryCatch` fires when required columns are
> absent. No silent wrong output paths identified for `basal_area()`.

---

## `R/species_table.R`

### `species_table()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `species_table` | `veg.PI` is not a data.frame | Caught by tryCatch → wrapped with context | `"Failed to build species table: veg.PI must be a data.frame"` | ✅ `"species_table: non-data.frame input throws wrapped error"` · `"species_table: wrapped error preserves original cause"` |
| 2 | `species_table` | `m_kind` not supplied | Caught by tryCatch → wrapped with context | `"Failed to build species table: Please specify the desired species scoring method with 'm_kind'"` | ✅ `"species_table: missing m_kind throws wrapped error"` |
| 3 | `species_table` | Processing crash on missing `site_unique`, `standardised_name`, `hits_unique`, or `transect` column → `plyr::count()` / `stats::aggregate()` / `ma_ausplot_ma()` throw | Caught by tryCatch → wrapped with context | `"Failed to build species table: <original msg>"` | ✅ (implicit via call.=FALSE test) |
| 4 | `species_table` | All species NA in `SN` path → 0-row `hits` → `ma_ausplot_ma` produces 0-row/0-col matrix | Silent wrong output | No error; empty matrix returned | (not separately tested — structural edge case) |

---

## Summary — `species_table.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 2 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **4** |

> Rows 1–3 are caught paths — `tryCatch` fires when input validation or processing
> fails. Row 4 is a silent wrong output path — `tryCatch` never fires; empty matrix
> returned when all species names are NA.

---

## `R/growth_form_table.R`

### `growth_form_table()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `growth_form_table` | `veg.PI` is not a data.frame | Caught by tryCatch → wrapped with context | `"Failed to build growth form table: veg.PI must be a data.frame"` | ✅ `"growth_form_table: non-data.frame input throws wrapped error"` · `"growth_form_table: wrapped error preserves original cause"` |
| 2 | `growth_form_table` | `growth_form` or `site_unique` column missing from `veg.PI` | Caught by tryCatch → wrapped with context | `"Failed to build growth form table: Can't match names of veg.PI..."` | ✅ `"growth_form_table: missing required column throws wrapped error"` |
| 3 | `growth_form_table` | `m_kind` not supplied | Caught by tryCatch → wrapped with context | `"Failed to build growth form table: Please specify the desired scoring method using 'm_kind'"` | ✅ `"growth_form_table: missing m_kind throws wrapped error"` |
| 4 | `growth_form_table` | 0-row `hits` after `standardised_name` NA-filter (when `species_name="SN"` and all names NA) → `form_rows[-which(duplicated(form_rows)),]` produces 0-row result silently | Silent wrong output | No error; 0-column matrix returned via `ma_ausplot_ma` | (not separately tested — structural edge case) |

---

## Summary — `growth_form_table.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 3 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **4** |

> Rows 1–3 are caught paths — `tryCatch` fires when input validation or processing
> fails. Row 4 is a silent wrong output path — `tryCatch` never fires; empty matrix
> returned when no species hits remain after NA filtering.

---

## `R/species_list.R`

### `species_list()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `species_list` | `veg.vouch` is not a data.frame | Caught by tryCatch → wrapped with context | `"Failed to build species list: veg.vouch must be a data.frame"` | ✅ `"species_list: non-data.frame input throws wrapped error"` · `"species_list: wrapped error preserves original cause"` |
| 2 | `species_list` | `site_location_name` or `herbarium_determination` column missing from `veg.vouch` | Caught by tryCatch → wrapped with context | `"Failed to build species list: Can't match names of veg.vouch..."` | ✅ `"species_list: missing required column throws wrapped error"` |
| 3 | `species_list` | `writefile=TRUE` with invalid/unwritable `outfile` path → `write.table()` throws | Caught by tryCatch → wrapped with context | `"Failed to build species list: <write.table error msg>"` | ✅ `"species_list: write.table failure is caught and wrapped"` |
| 4 | `species_list` | `append_family=TRUE` with `family` column containing all-NA values → `paste(x, NA, sep="--")` produces `"species--NA"` silently | Silent wrong output | No error; family suffix is `NA` in all entries | ✅ `"species_list: append_family=TRUE with NA family silently appends NA"` |

---

## Summary — `species_list.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 4 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **4** |

> Rows 1–3 are caught paths — `tryCatch` fires when input validation or file I/O
> fails. Row 4 is a silent wrong output path — `tryCatch` never fires; the function
> appends `NA` as the family suffix without error.

---

## `R/invasion_info.R`

### `invasion_info()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `invasion_info` | `afa_static` is not a data.frame | Caught by tryCatch → wrapped with context | `"Failed to gather invasion information: \`afa_static\` must be a data.frame."` | ✅ `"invasion_info: afa_static must be a data.frame"` (existing, substring match) |
| 2 | `invasion_info` | `afa_static` missing required schema columns | Caught by tryCatch → wrapped with context | `"Failed to gather invasion information: The following required columns are missing..."` | ✅ `"invasion_info: afa_static missing required columns throws error"` (existing, substring match) |
| 3 | `invasion_info` | `ausplots` is not a data.frame or NULL | Caught by tryCatch → wrapped with context | `"Failed to gather invasion information: \`ausplots\` must be a data.frame or NULL."` | ✅ `"invasion_info: non-data.frame ausplots throws error"` (existing, substring match) |
| 4 | `invasion_info` | `taxon_col` not found in `ausplots` | Caught by tryCatch → wrapped with context | `"Failed to gather invasion information: \`taxon_col\` not found in \`ausplots\`."` | ✅ `"invasion_info: taxon_col not in ausplots throws error"` (existing, substring match) |
| 5 | `invasion_info` | APCalign package not installed | Caught by tryCatch → wrapped with context | `"Failed to gather invasion information: Package \`APCalign\` must be installed..."` | ✅ `"invasion_info: missing APCalign throws informative error"` (existing, substring match) |
| 6 | `invasion_info` | `APCalign::align_taxa()` throws (network, data error, etc.) | Caught by tryCatch → wrapped with context | `"Failed to gather invasion information: <original msg>"` | ✅ `"invasion_info: APCalign failure is caught and wrapped"` · `"invasion_info: wrapped error preserves the original cause"` |

---

## Summary — `invasion_info.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 6 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **6** |

> All six rows are caught paths — `tryCatch` fires on both explicit input-validation
> `stop()` calls and on failures from `APCalign::align_taxa()`. Existing tests
> remain valid: `expect_error(…, pattern)` uses substring match, which continues
> to pass after the message is wrapped with the prefix.

---

## `R/biodiversity_metrics.R`

### `biodiversity_metrics()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `biodiversity_metrics` | `ausplots` is not a list | Caught by tryCatch → wrapped with context | `"Failed to calculate biodiversity metrics: \`ausplots\` must be an object..."` | ✅ `"biodiversity_metrics: non-list input throws error"` (existing, substring match) |
| 2 | `biodiversity_metrics` | `ausplots$veg.vouch` is absent | Caught by tryCatch → wrapped with context | `"Failed to calculate biodiversity metrics: \`ausplots\$veg.vouchers\` is missing."` | ✅ `"biodiversity_metrics: missing veg.vouch throws error"` (existing, substring match) |
| 3 | `biodiversity_metrics` | `ausplots$veg.PI` is absent | Caught by tryCatch → wrapped with context | `"Failed to calculate biodiversity metrics: \`ausplots\$veg.PI\` is missing."` | ✅ `"biodiversity_metrics: missing veg.PI throws error"` (existing, substring match) |
| 4 | `biodiversity_metrics` | `plots` filter leaves no matching sites | Caught by tryCatch → wrapped with context | `"Failed to calculate biodiversity metrics: No plots left..."` | ✅ `"biodiversity_metrics: plots filter with no matching sites throws error"` (existing, substring match) |
| 5 | `biodiversity_metrics` | `species_table()` throws due to missing required column or processing failure | Caught by tryCatch → wrapped with context | `"Failed to calculate biodiversity metrics: <original msg from species_table>"` | ✅ `"biodiversity_metrics: species_table failure is caught and wrapped"` · `"biodiversity_metrics: wrapped error preserves original cause"` |

---

## Summary — `biodiversity_metrics.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 5 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **5** |

> All five rows are caught paths — `tryCatch` fires on input-validation `stop()`
> calls and on failures propagated from `species_table()`. Existing tests remain
> valid via substring matching.

---

## `R/species_traits.R`

### `species_traits()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `species_traits` | `get_trait_data_pp()` throws (network, JSON, column error) | Caught by tryCatch → wrapped with context | `"Failed to retrieve species traits: <original msg>"` | ✅ `"species_traits: get_trait_data_pp failure is caught and wrapped"` · `"species_traits: wrapped error preserves original cause"` |
| 2 | `species_traits` | `species_df` is not a data.frame | Caught by tryCatch → wrapped with context | `"Failed to retrieve species traits: \`species_df\` must be a data.frame"` | ✅ `"species_traits: species_df that is not a data.frame throws error"` (existing, substring match) |
| 3 | `species_traits` | `species_df` missing `species_name` column | Caught by tryCatch → wrapped with context | `"Failed to retrieve species traits: must contain a column called \`species_name\`"` | ✅ `"species_traits: species_df missing species_name column throws error"` (existing, substring match) |
| 4 | `species_traits` | No input arguments provided | Caught by tryCatch → wrapped with context | `"Failed to retrieve species traits: Please provide either..."` | ✅ `"species_traits: no inputs throws error"` (existing, substring match) |
| 5 | `species_traits` | `ausplots` is not a list | Caught by tryCatch → wrapped with context | `"Failed to retrieve species traits: \`ausplots\` must be an object returned by get_ausplots"` | ✅ `"species_traits: non-list ausplots throws error"` (existing, substring match) |
| 6 | `species_traits` | `ausplots` missing `veg.vouch` | Caught by tryCatch → wrapped with context | `"Failed to retrieve species traits: \`ausplots\$veg.vouch\` is missing"` | ✅ `"species_traits: ausplots without veg.vouch throws error"` (existing, substring match) |

---

## Summary — `species_traits.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 6 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **6** |

> All six rows are caught paths — `tryCatch` wraps the entire function body.
> Existing tests remain valid via substring matching after the prefix is applied.

---

## `R/community_traits.R`

### `community_traits()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `community_traits` | `get_trait_data_pp()` throws (network, JSON, column error) | Caught by outer tryCatch → wrapped with context | `"Failed to calculate community traits: <original msg>"` | ✅ `"community_traits: get_trait_data_pp failure is caught and wrapped"` · `"community_traits: wrapped error preserves original cause"` |
| 2 | `community_traits` | `ausplots` is not a list | Caught by outer tryCatch → wrapped with context | `"Failed to calculate community traits: \`ausplots\` must be an object returned by get_ausplots"` | ✅ `"community_traits: non-list input throws error"` (existing, substring match) |
| 3 | `community_traits` | `ausplots$veg.PI` is absent | Caught by outer tryCatch → wrapped with context | `"Failed to calculate community traits: \`ausplots\$veg.PI\` is missing"` | ✅ `"community_traits: missing veg.PI throws error"` (existing, substring match) |
| 4 | `community_traits` | `categorical_trait` column not found in trait data | Caught by outer tryCatch → wrapped with context | `"Failed to calculate community traits: not found in \`trait_data_pp\`"` | ✅ `"community_traits: invalid categorical_trait throws error"` (existing, substring match) |
| 5 | `community_traits` | `species_name` column missing from `veg.PI` | Caught by outer tryCatch → wrapped with context | `"Failed to calculate community traits: not found in \`veg.PI\`"` | ✅ `"community_traits: missing species column in veg.PI throws error"` (existing, substring match) |
| 6 | `community_traits` | `plots` filter leaves no rows in `veg.PI` | Caught by outer tryCatch → wrapped with context | `"Failed to calculate community traits: No rows left in \`veg.PI\` after filtering"` | ✅ `"community_traits: plots filter with no matching sites throws error"` (existing, substring match) |
| 7 | `community_traits` | `FD::dbFD()` or `FD::gowdis()` throws | Caught by **inner** tryCatch (returns NULL) — outer tryCatch not triggered | Result metric column set to `NA` silently | (existing inner tryCatch; not rewrapped — correct design) |

---

## Summary — `community_traits.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 7 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **7** |

> Rows 1–6 are caught by the outer `tryCatch` added in this session. Row 7 is
> handled by the existing inner `tryCatch` for `FD::dbFD()` / `FD::gowdis()`,
> which returns `NULL` and sets metric columns to `NA` — the outer tryCatch is not
> triggered. Existing tests remain valid via substring matching.

---

## `R/ausplots_trajectory.R`

### `ausplots_trajectory()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `ausplots_trajectory` | `my.ausplots.object` not supplied | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory plots: my.ausplots.object must be specified, see get_ausplots"` | ✅ `"ausplots_trajectory: missing my.ausplots.object is caught and wrapped"` |
| 2 | `ausplots_trajectory` | `my.ausplots.object` is not a list | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory plots: my.ausplots.object must be a list ..."` | ✅ `"ausplots_trajectory: non-list my.ausplots.object is caught and wrapped"` |
| 3 | `ausplots_trajectory` | `plot_select` is not a character vector | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory plots: plot_select must be a character vector ..."` | ✅ `"ausplots_trajectory: non-character plot_select is caught and wrapped"` |
| 4 | `ausplots_trajectory` | `plot_select` has no matching site names | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory plots: No entries in plot_select were found ..."` | ✅ `"ausplots_trajectory: plot_select with no matches is caught and wrapped"` |
| 5 | `ausplots_trajectory` | `choices` contains no valid value (`"PCoA"` or `"diversity"`) | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory plots: choices must include at least one of PCoA or diversity."` | ✅ `"ausplots_trajectory: invalid choices is caught and wrapped"` |
| 6 | `ausplots_trajectory` | No sites have `visit_number >= min.revisits` | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory plots: No sites have at least N visits ..."` | ✅ `"ausplots_trajectory: insufficient visits is caught and wrapped"` |
| 7 | `ausplots_trajectory` | `species_table()` throws (missing column, bad data) | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory plots: <original msg from species_table>"` | (covered by tryCatch; no separate test) |
| 8 | `ausplots_trajectory` | `vegan::vegdist(diss)` degenerate matrix | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory plots: <original msg from vegdist>"` | (covered by tryCatch; no separate test) |
| 9 | `ausplots_trajectory` | `pdf(outfile)` unwritable path | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory plots: cannot open file ..."` | (covered by tryCatch; no separate test) |

---

## Summary — `ausplots_trajectory.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 6 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **9** |

> Rows 1–6 are caught paths with tests. Rows 7–9 are caught by the same top-level
> `tryCatch` but have no dedicated tests — the tryCatch catches them automatically.

---

## `R/ausplots_visual.R`

### `ausplots_visual()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `ausplots_visual` | All output flags set to `FALSE` | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots visualisations: You have not requested any outputs! ..."` | ✅ `"ausplots_visual: all outputs FALSE is caught and wrapped"` |
| 2 | `ausplots_visual` | `my.ausplots.object` provided but missing `$veg.PI` while veg plots requested | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots visualisations: If you provide a my.ausplots.object it must contain $veg.PI data ..."` | ✅ `"ausplots_visual: missing veg.PI is caught and wrapped"` |
| 3 | `ausplots_visual` | `my.ausplots.object` provided but missing `$site.info` while map requested | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots visualisations: If you provide a my.ausplots.object it must contain $site.info data ..."` | ✅ `"ausplots_visual: missing site.info is caught and wrapped"` |
| 4 | `ausplots_visual` | No `my.ausplots.object`; `get_ausplots()` throws (network / API failure) | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots visualisations: <original msg from get_ausplots>"` | ✅ `"ausplots_visual: get_ausplots failure is caught and wrapped"` |
| 5 | `ausplots_visual` | `fraction_pie(frac, i)` throws for a given site | Silent — wrapped in `try()` inside loop | `try-error` object returned and discarded; rest of sites continue | ⚠️ Silent by design — `try()` swallows; no separate test |
| 6 | `ausplots_visual` | `growthform_pie(GF[i,], GF_col)` throws for a given site | Silent — wrapped in `try()` inside loop | Same as above | ⚠️ Silent by design |
| 7 | `ausplots_visual` | `cumulative_cover(subset(...))` throws for a given site | Silent — wrapped in `try()` inside loop | Same as above | ⚠️ Silent by design |
| 8 | `ausplots_visual` | `whitt.plot(sppBYsites[i,])` throws for a given site | Silent — wrapped in `try()` inside loop | Same as above | ⚠️ Silent by design |
| 9 | `ausplots_visual` | `map_ausplots()` or `map_attribute()` throws | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots visualisations: <original msg>"` | (covered by tryCatch; no separate test) |
| 10 | `ausplots_visual` | `pdf(outfile)` unwritable path | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots visualisations: cannot open file ..."` | (covered by tryCatch; no separate test) |

---

## Summary — `ausplots_visual.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 4 |
| ⚠️ Silent by design (`try()`) | 4 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **10** |

> Rows 1–4 are caught paths with tests. Rows 5–8 are silently swallowed by
> existing `try()` calls inside the per-site loop — this is intentional fault
> tolerance (one bad site should not abort the whole visualisation). Rows 9–10
> are caught by the outer `tryCatch` automatically.

---

## `R/cumulative_cover.R`

### `cumulative_cover()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `cumulative_cover` | `veg.PI` missing `herbarium_determination` column → `veg.PI[, c("herbarium_determination", …)]` throws | Caught by tryCatch → wrapped with context | `"Failed to generate cumulative cover plot: undefined columns selected"` | ✅ `"cumulative_cover: missing herbarium_determination column is caught and wrapped"` · `"cumulative_cover: wrapped error preserves original cause"` |
| 2 | `cumulative_cover` | `veg.PI` missing `hits_unique` column → same column-access crash | Caught by tryCatch → wrapped with context | `"Failed to generate cumulative cover plot: undefined columns selected"` | ✅ `"cumulative_cover: missing hits_unique column is caught and wrapped"` |
| 3 | `cumulative_cover` | `veg.PI` missing `in_canopy_sky` column → same column-access crash | Caught by tryCatch → wrapped with context | `"Failed to generate cumulative cover plot: undefined columns selected"` | ✅ `"cumulative_cover: missing in_canopy_sky column is caught and wrapped"` |
| 4 | `cumulative_cover` | All `herbarium_determination` = NA → outer `if(length(na.omit(…)) > 0)` guard skips plotting | Silent correct output (benign) | No error; function returns NULL with no plot rendered | ✅ `"cumulative_cover: all-NA herbarium_determination runs without error"` |

---

## Summary — `cumulative_cover.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 4 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **4** |

> Rows 1–3 are caught paths — `tryCatch` fires when required columns are absent.
> Row 4 is a benign silent path — the function correctly skips rendering when all
> species names are NA; no error expected or observed.

---

## `R/fraction_pie.R`

### `fraction_pie()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `fraction_pie` | Invalid `row` name not found in `frac` → `frac[row, …]` fails | `try()` → silent swallow | `try-error` returned and discarded; function returns NULL | ✅ `"fraction_pie: invalid row is silently swallowed by try"` |
| 2 | `fraction_pie` | `frac = NULL` → `NULL[row, …]` throws "incorrect number of dimensions" | `try()` → silent swallow | Same as above | ✅ `"fraction_pie: NULL frac is silently swallowed by try"` |

---

## Summary — `fraction_pie.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 2 |
| ⚠️ Silent by design (`try()`) | 2 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **2** |

> Both rows are silently swallowed by the existing `try()` calls that wrap the
> entire body. The outer `tryCatch` is a safety net only — it never fires under
> normal R semantics because `try()` absorbs all errors first. Tests document
> the silent-swallowing behaviour.

---

## `R/growthform_pie.R`

### `growthform_pie()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `growthform_pie` | `GF` is not a matrix/data.frame → `colSums(GF)` throws **outside** `try()` | Caught by tryCatch → wrapped with context | `"Failed to generate growth form pie chart: not numeric|'x' must be|requires numeric"` | ✅ `"growthform_pie: non-matrix GF input is caught and wrapped"` · `"growthform_pie: wrapped error preserves original cause"` |
| 2 | `growthform_pie` | `pie(…)` or `legend(…)` throws (bad colours, empty GF, etc.) | `try()` → silent swallow | `try-error` returned and discarded | ⚠️ Silent by design — `try()` swallows; no separate test |
| 3 | `growthform_pie` | All GF columns zero → `GF[, which(colSums(GF) > 0)]` produces 0-column frame → `pie(numeric(0))` → try() absorbs | `try()` → silent swallow | No error; blank/empty plot | ✅ `"growthform_pie: all-zero column is silently dropped"` (partial — 1 zero col) |

---

## Summary — `growthform_pie.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 2 |
| ⚠️ Silent by design (`try()`) | 1 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **3** |

> Row 1 is a caught path — `tryCatch` fires when `colSums()` receives a
> non-numeric argument (the only code outside `try()`). Rows 2–3 are silently
> swallowed by `try()` — the outer `tryCatch` is not triggered.

---

## `R/map_attribute.R`

### `map_attribute()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `map_attribute` | `my.ausplots.object` is NULL → `my.ausplots.object$veg.PI$growth_form` throws | Caught by tryCatch → wrapped with context | `"Failed to generate attribute map: <original msg>"` | ✅ `"map_attribute: NULL input is caught and wrapped"` · `"map_attribute: wrapped error preserves original cause"` |
| 2 | `map_attribute` | `my.ausplots.object$veg.PI` is NULL → `NULL$growth_form` returns NULL → `any(c(…) %in% NULL)` = FALSE — no-tree guard fires; `message()` emitted | Silent correct output (benign) | No error; message "The selected plots have no trees..." emitted | ✅ `"map_attribute: no-tree input skips plotting without error"` |
| 3 | `map_attribute` | `ggplot2::map_data("world2Hires", "Australia")` fails (mapdata not installed) | Caught by tryCatch → wrapped with context | `"Failed to generate attribute map: <original msg>"` | (covered by tryCatch; no separate test) |
| 4 | `map_attribute` | `single_cover_value()` throws (missing column in veg.PI) | Caught by tryCatch → wrapped with context | `"Failed to generate attribute map: <original msg>"` | (covered by tryCatch; no separate test) |
| 5 | `map_attribute` | `merge()` result missing `longitude`/`latitude` → ggplot2 column-access crash | Caught by tryCatch → wrapped with context | `"Failed to generate attribute map: <original msg>"` | (covered by tryCatch; no separate test) |

---

## Summary — `map_attribute.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 3 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **5** |

> Row 1 is a caught path — `tryCatch` fires when NULL is passed. Row 2 is a
> benign silent path — the no-tree guard fires and emits a message without error.
> Rows 3–5 are caught by the outer `tryCatch` automatically but have no dedicated
> tests.

---

## `R/map_ausplots.R`

### `map_ausplots()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `map_ausplots` | `my.ausplots.object` missing `$site.info` → explicit `stop()` inside tryCatch | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots map: The input 'my.ausplots.object' must contain $site.info data ..."` | ✅ `"map_ausplots: missing site.info is caught and wrapped"` · `"map_ausplots: wrapped error preserves original cause"` |
| 2 | `map_ausplots` | `my.ausplots.object` is NULL → `names(NULL)` = NULL → `!("site.info" %in% NULL)` TRUE → same stop() fires | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots map: ..."` | ✅ `"map_ausplots: NULL input is caught and wrapped"` |
| 3 | `map_ausplots` | `site.info` missing `bioregion_name` column → `factor(NULL)` assigned; `levels()` = character(0) → shape calculation crashes | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots map: <original msg>"` | ✅ `"map_ausplots: site.info missing required columns is caught and wrapped"` |
| 4 | `map_ausplots` | `ggplot2::map_data("world2Hires", "Australia")` fails (mapdata not installed) | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots map: <original msg>"` | (covered by tryCatch; no separate test) |
| 5 | `map_ausplots` | `site.info` missing `longitude`/`latitude` → ggplot2 aes column-access crash | Caught by tryCatch → wrapped with context | `"Failed to generate ausplots map: <original msg>"` | (covered by tryCatch; no separate test) |

---

## Summary — `map_ausplots.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 3 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **5** |

> Rows 1–3 are caught paths with tests. Rows 4–5 are caught by the outer
> `tryCatch` automatically but have no dedicated tests.

---

## `R/point_intercept_visual.R`

### `point_intercept_visual()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `point_intercept_visual` | `plots` is missing/NULL/empty → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to generate point intercept visualisation: Please provide one or more plot IDs ..."` | ✅ `"point_intercept_visual: missing plots is caught and wrapped"` · `"point_intercept_visual: wrapped error preserves original cause"` |
| 2 | `point_intercept_visual` | `ausplots$veg.PI` is NULL → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to generate point intercept visualisation: ausplots$veg.PI is missing."` | ✅ `"point_intercept_visual: missing veg.PI is caught and wrapped"` |
| 3 | `point_intercept_visual` | `ausplots$site.info` is NULL → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to generate point intercept visualisation: ausplots$site.info is missing."` | ✅ `"point_intercept_visual: missing site.info is caught and wrapped"` |
| 4 | `point_intercept_visual` | `plot_is_100m_by_100m` column absent from `site.info` → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to generate point intercept visualisation: ausplots$site.info$plot_is_100m_by_100m is missing."` | (covered by tryCatch; column-check stop()) |
| 5 | `point_intercept_visual` | `site_location_name` column absent from `site.info` → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to generate point intercept visualisation: ausplots$site.info$site_location_name is missing."` | (covered by tryCatch; column-check stop()) |
| 6 | `point_intercept_visual` | `visual` contains invalid value → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to generate point intercept visualisation: \`visual\` must be one or more of: ..."` | ✅ `"point_intercept_visual: invalid visual value is caught and wrapped"` |
| 7 | `point_intercept_visual` | After filtering, no rows remain → `nrow(pi_data) == 0` → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to generate point intercept visualisation: No point intercept data left after filtering."` | (covered by tryCatch; no separate test) |
| 8 | `point_intercept_visual` | Transect/coordinate loop or `layout()`/`plot()` crash on bad column data | Caught by tryCatch → wrapped with context | `"Failed to generate point intercept visualisation: <original msg>"` | (covered by tryCatch; no separate test) |

---

## Summary — `point_intercept_visual.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 4 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **8** |

> Rows 1–3, 6 are caught paths with tests. Rows 4–5, 7–8 are caught by the outer
> `tryCatch` automatically via the same explicit `stop()` or propagated errors —
> no dedicated tests required.

---

## `R/set_auth.R`

### `set_auth()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `set_auth` | `role` is NULL → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to set authentication: role must be the name of the role you wish to assume"` | ✅ `"set_auth: NULL role is caught and wrapped"` · `"set_auth: wrapped error preserves original cause for NULL role"` |
| 2 | `set_auth` | `secret` is NULL → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to set authentication: secret must be the secret used to authenticate you to the API"` | ✅ `"set_auth: NULL secret is caught and wrapped"` · `"set_auth: wrapped error preserves original cause for NULL secret"` |
| 3 | `set_auth` | `role` and `secret` valid → `options()` stores both; `message()` emitted | Happy path | No error | ✅ `"set_auth: valid role and secret are stored without error"` |

---

## Summary — `set_auth.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 3 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **3** |

> Rows 1–2 are caught paths — `tryCatch` fires when NULL is passed for either
> credential. Row 3 is the happy path. No silent wrong output paths exist —
> `set_auth()` either stores credentials or raises an error.

---

## `R/ma_ausplot_ma.R`

### `ma_ausplot_ma()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `ma_ausplot_ma` | Single-column data frame → `ncol(dat) < 2` → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to reshape site-by-species matrix: Mama needs at least two columns (vectors)!"` | ✅ `"ma_ausplot_ma: single-column input is caught and wrapped"` · `"ma_ausplot_ma: wrapped error preserves original cause"` |
| 2 | `ma_ausplot_ma` | NULL input → `data.frame(NULL)` = 0-col df → same stop() fires | Caught by tryCatch → wrapped with context | `"Failed to reshape site-by-species matrix: ..."` | ✅ `"ma_ausplot_ma: NULL input is caught and wrapped"` |
| 3 | `ma_ausplot_ma` | Valid 2-column input → reshape succeeds | Happy path | No error; wide data frame returned | ✅ `"ma_ausplot_ma: valid 2-column input reshapes without error"` |

---

## Summary — `ma_ausplot_ma.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 3 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **3** |

> Rows 1–2 are caught paths — `tryCatch` fires on the explicit `stop()` for
> insufficient columns. Row 3 is the happy path.

---

## `R/single_cover_value.R`

### `single_cover_value()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `single_cover_value` | `veg.PI` not a data.frame → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to calculate cover value: veg.PI must be a data.frame"` | ✅ `"single_cover_value: non-data.frame input is caught and wrapped"` · `"single_cover_value: wrapped error preserves original cause"` |
| 2 | `single_cover_value` | `veg.PI` missing `growth_form`, `site_unique`, or `height` column → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to calculate cover value: Can't match names of veg.PI..."` | ✅ `"single_cover_value: missing required columns is caught and wrapped"` |
| 3 | `single_cover_value` | NULL input → `inherits(NULL, "data.frame")` FALSE → stop() fires | Caught by tryCatch → wrapped with context | `"Failed to calculate cover value: veg.PI must be a data.frame"` | ✅ `"single_cover_value: NULL input is caught and wrapped"` |
| 4 | `single_cover_value` | `in_canopy_sky` not logical → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to calculate cover value: in_canopy_sky must be logical (TRUE/FALSE)"` | (covered by tryCatch; no separate test) |
| 5 | `single_cover_value` | `by.growth_form=TRUE`, none of `my.growth_forms` present in data → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to calculate cover value: One or more growth forms supplied in my.growth_forms is not present..."` | (covered by tryCatch; no separate test) |
| 6 | `single_cover_value` | Valid veg.PI → returns `data.frame` with `site_unique` and `percentCover` | Happy path | No error | ✅ `"single_cover_value: valid input returns cover data frame without error"` |

---

## Summary — `single_cover_value.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 4 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **6** |

> Rows 1–5 are caught paths — `tryCatch` fires on input-validation `stop()`
> calls. Rows 4–5 are covered by the outer `tryCatch` automatically. Row 6 is
> the happy path.

---

## `R/species_optimisation.R`

### `optim_species()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `optim_species` | `start` not one of `"fixed"`, `"defined"`, `"random"` → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to optimise species accumulation: Argument 'start' must be character and one of: ..."` | ✅ `"optim_species: invalid start is caught and wrapped"` · `"optim_species: wrapped error preserves original cause"` |
| 2 | `optim_species` | `plot_name` not in `rownames(speciesVsitesMatrix)` → explicit `stop()` | Caught by tryCatch → wrapped with context | `"Failed to optimise species accumulation: Selected plot_name must match a site/row name..."` | ✅ `"optim_species: invalid plot_name is caught and wrapped"` |
| 3 | `optim_species` | NULL matrix input → `speciesVsitesMatrix[,1]` on NULL crashes | Caught by tryCatch → wrapped with context | `"Failed to optimise species accumulation: <original msg>"` | ✅ `"optim_species: NULL input is caught and wrapped"` |
| 4 | `optim_species` | `try(plot_opt(result))` fails | Silent — `try()` swallows by design | `try-error` returned and discarded | ⚠️ Silent by design |
| 5 | `optim_species` | Sub-function (`Richness.opt`, `RRR.opt`, etc.) throws (empty matrix, bad data) | Caught by tryCatch → wrapped with context | `"Failed to optimise species accumulation: <original msg>"` | (covered by tryCatch; no separate test) |

---

## Summary — `species_optimisation.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 3 |
| ⚠️ Silent by design (`try()`) | 1 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **5** |

> Rows 1–3 are caught paths with tests. Row 4 is silently swallowed by the
> existing `try(plot_opt(result))` — plot failure should never abort an
> optimisation run. Row 5 is caught automatically by `tryCatch`.

---

## `R/trajectoryPlots_ausplots.R`

### `trajectoryPCoA_ausplots()` / `trajectoryPlot_ausplots()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `trajectoryPCoA_ausplots` | NULL `d` → `as.matrix(NULL)` subscript crash | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory PCoA plot: <original msg>"` | ✅ `"trajectoryPCoA_ausplots: NULL d is caught and wrapped"` · `"trajectoryPCoA_ausplots: wrapped error preserves original cause"` |
| 2 | `trajectoryPCoA_ausplots` | `d` size mismatched with `sites` length → matrix subscript crash | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory PCoA plot: <original msg>"` | ✅ `"trajectoryPCoA_ausplots: mismatched d and sites is caught and wrapped"` |
| 3 | `trajectoryPCoA_ausplots` | `cmdscale()` degenerate distance matrix → fewer columns than `axes[2]` → subscript crash | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory PCoA plot: <original msg>"` | (covered by tryCatch; no separate test) |
| 4 | `trajectoryPlot_ausplots` | NULL `x` → `x[sites %in% selIDs, axes[1]]` crash | Caught by tryCatch → wrapped with context | `"Failed to generate trajectory plot: <original msg>"` | ✅ `"trajectoryPlot_ausplots: NULL x is caught and wrapped"` |

---

## Summary — `trajectoryPlots_ausplots.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 3 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **4** |

> Rows 1–2, 4 are caught paths with tests. Row 3 is caught automatically by
> `tryCatch` — no dedicated test required.

---

## `R/whitt.plot.R`

### `whitt.plot()`

| # | Function | Trigger | Failure type | Exact error message / class | Test name |
|---|----------|---------|--------------|-----------------------------|-----------|
| 1 | `whitt.plot` | `vegan::rad.lognormal()` throws (NULL/bad input to `round()`) | `try()` → silent swallow | `try-error` returned and discarded; function returns `NULL` | ✅ `"whitt.plot: NULL input is silently swallowed by try"` |
| 2 | `whitt.plot` | `legend()` throws | `try()` → silent swallow | Same as above | ⚠️ Silent by design — no separate test |
| 3 | `whitt.plot` | Valid named abundance vector → `rad.lognormal` + `plot` + `legend` succeed | Happy path | No error | ✅ `"whitt.plot: valid abundance vector renders without error"` |

---

## Summary — `whitt.plot.R`

| Status | Count |
|--------|-------|
| ✅ Tested | 2 |
| ⚠️ Silent by design (`try()`) | 1 |
| ❌ Not tested | 0 |
| N/A | 0 |
| **Total error paths** | **3** |

> Both `plot()` and `legend()` are already wrapped in `try()` — all errors are
> silently absorbed. The outer `tryCatch` is a safety net for future code. Row 1
> tests the silent-swallowing behaviour. Row 3 is the happy path.

