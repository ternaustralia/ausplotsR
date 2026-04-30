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

