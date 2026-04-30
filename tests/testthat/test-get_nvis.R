library(testthat)
library(jsonlite)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# get_nvis()
#
# Fetches NVIS vegetation attributes from the AusPlots REST API
# (/nvis_ausplot_r) and returns them as a data frame.
# All tests mock .ausplots_api using fixtures/nvis_ausplot_r.json.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# FIXTURE LOADER
# ══════════════════════════════════════════════════════════════════════════════

fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name), simplifyDataFrame = TRUE)
}

# What it tests: NULL input returns all rows from the API as a data.frame
# Input:         site_unique = NULL (default)
# Expected:      data.frame with 2 rows (from fixture) containing a "site_unique" column
test_that("get_nvis: NULL returns all rows as a data.frame", {
  stub(get_nvis, ".ausplots_api", fixture("nvis_ausplot_r.json"))
  result <- get_nvis()
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2L)
  expect_true("site_unique" %in% names(result))
})

# What it tests: a known site_unique filters to exactly one matching row
# Input:         site_unique = "SAARIV0008-57097" (first fixture row)
# Expected:      data.frame with 1 row, site_unique matches input
test_that("get_nvis: character vector returns only the matching rows", {
  stub(get_nvis, ".ausplots_api", fixture("nvis_ausplot_r.json"))
  result <- get_nvis(site_unique = "SAARIV0008-57097")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1L)
  expect_equal(result$site_unique, "SAARIV0008-57097")
})

# What it tests: an unknown site_unique returns one row with all NAs (left join)
# Input:         site_unique = "DOES_NOT_EXIST_999"
# Expected:      data.frame with 1 row; all columns except site_unique are NA
test_that("get_nvis: unknown site_unique returns one row with NAs", {
  stub(get_nvis, ".ausplots_api", fixture("nvis_ausplot_r.json"))
  result <- get_nvis(site_unique = "DOES_NOT_EXIST_999")
  expect_equal(nrow(result), 1L)
  expect_equal(result$site_unique, "DOES_NOT_EXIST_999")
  other_cols <- setdiff(names(result), "site_unique")
  expect_true(all(is.na(result[, other_cols])))
})

# What it tests: a data.frame with a site_unique column is accepted as input
# Input:         data.frame(site_unique = both fixture sites)
# Expected:      data.frame with 2 rows returned
test_that("get_nvis: data.frame input with site_unique column is accepted", {
  stub(get_nvis, ".ausplots_api", fixture("nvis_ausplot_r.json"))
  df_input <- data.frame(
    site_unique = c("SAARIV0008-57097", "NSABBS0002-58555"),
    stringsAsFactors = FALSE
  )
  result <- get_nvis(site_unique = df_input)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2L)
})

# What it tests: a data.frame without a site_unique column throws an informative error
# Input:         data.frame(x = 1) — has no site_unique column
# Expected:      error containing "must contain a 'site_unique' column"
test_that("get_nvis: data.frame missing site_unique column throws error", {
  stub(get_nvis, ".ausplots_api", fixture("nvis_ausplot_r.json"))
  expect_error(
    get_nvis(site_unique = data.frame(x = 1)),
    "must contain a 'site_unique' column"
  )
})

# What it tests: a list (non-atomic, non-data.frame) input throws an informative error
# Input:         list(a = 1) — not NULL, not character, not data.frame
# Expected:      error containing "'site_unique' must be NULL"
test_that("get_nvis: list input throws error", {
  stub(get_nvis, ".ausplots_api", fixture("nvis_ausplot_r.json"))
  expect_error(
    get_nvis(site_unique = list(a = 1)),
    "'site_unique' must be NULL"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# API response schema validation
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: result contains all columns from the API schema
# Input:         fixture with all known API columns
# Expected:      all expected column names present in the returned data.frame
test_that("get_nvis: result contains all expected API columns", {
  stub(get_nvis, ".ausplots_api", fixture("nvis_ausplot_r.json"))
  result <- get_nvis()
  expected_cols <- c(
    "site_unique", "site_location_visit_id", "site_location_name",
    "class", "structural_formation", "nvis_code",
    "broad_floristic_formation", "sub_formation", "association",
    "vegetation_community_l5", "vegetation_community_l6", "sub_association",
    "upper_average_height_metre", "upper_cover_percentage",
    "mid_average_height_metre", "mid_cover_percentage",
    "ground_average_height_metre", "ground_cover_percentage",
    "MVG_VICTA", "MVG_NAME", "MVS_VICTA", "MVS_NAME"
  )
  missing <- setdiff(expected_cols, names(result))
  expect_equal(missing, character(0))
})

# What it tests: site_unique column is coerced to character
# Input:         fixture response
# Expected:      site_unique column is character class
test_that("get_nvis: site_unique column is character", {
  stub(get_nvis, ".ausplots_api", fixture("nvis_ausplot_r.json"))
  result <- get_nvis()
  expect_true(is.character(result$site_unique))
})

# What it tests: MVG_VICTA and MVS_VICTA are numeric classification codes
# Input:         fixture response
# Expected:      both columns are numeric or integer
test_that("get_nvis: MVG_VICTA and MVS_VICTA are numeric", {
  stub(get_nvis, ".ausplots_api", fixture("nvis_ausplot_r.json"))
  result <- get_nvis()
  expect_true(is.numeric(result$MVG_VICTA) || is.integer(result$MVG_VICTA))
  expect_true(is.numeric(result$MVS_VICTA) || is.integer(result$MVS_VICTA))
})

# ══════════════════════════════════════════════════════════════════════════════
# get_nvis() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: tryCatch catches and wraps .ausplots_api failure
# Input:         .ausplots_api mocked to throw
# Expected:      stop() with "Failed to retrieve NVIS vegetation attributes:" prefix
test_that("get_nvis: .ausplots_api failure is caught and wrapped with context", {
  stub(get_nvis, ".ausplots_api", function(...) stop("connection refused"))
  expect_error(get_nvis(), "Failed to retrieve NVIS vegetation attributes:")
})

# What it tests: original error message survives inside the wrapped message
# Input:         .ausplots_api mocked to throw with known message
# Expected:      wrapped error contains the original cause
test_that("get_nvis: wrapped error preserves original cause", {
  stub(get_nvis, ".ausplots_api", function(...) stop("connection refused"))
  expect_error(get_nvis(), "connection refused")
})

# What it tests: as.data.frame failure on non-coercible return is caught
# Input:         .ausplots_api returns an environment (not coercible)
# Expected:      stop() with "Failed to retrieve NVIS vegetation attributes:" prefix
test_that("get_nvis: as.data.frame failure on non-coercible return is caught", {
  stub(get_nvis, ".ausplots_api", function(...) new.env())
  expect_error(get_nvis(), "Failed to retrieve NVIS vegetation attributes:")
})

# What it tests: NULL API return — as.data.frame(NULL) produces 0-row df; site_unique
#               column is assigned character(0) on 0-row df (no error); function returns
#               empty data frame silently when called with no site_unique filter
# Input:         .ausplots_api returns NULL
# Expected:      function returns without error; result is a data frame
test_that("get_nvis: NULL API return returns empty data frame silently", {
  stub(get_nvis, ".ausplots_api", function(...) NULL)
  result <- get_nvis()
  expect_true(is.data.frame(result))
})

# ══════════════════════════════════════════════════════════════════════════════
# get_nvis() — SILENT WRONG OUTPUT (tryCatch never fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: API returns df missing site_unique column — assigning character(0)
#               to a 1-row df throws "replacement has 0 rows, data has 1";
#               this IS caught and wrapped by tryCatch (not silent)
# Input:         API returns df with no site_unique column, site_unique="X" requested
# Expected:      stop() with "Failed to retrieve NVIS vegetation attributes:" prefix
test_that("get_nvis: API response missing site_unique column is caught", {
  stub(get_nvis, ".ausplots_api", function(...) data.frame(class = "forest", stringsAsFactors = FALSE))
  expect_error(get_nvis(site_unique = "X"), "Failed to retrieve NVIS vegetation attributes:")
})