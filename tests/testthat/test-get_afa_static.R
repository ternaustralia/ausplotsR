library(testthat)

# ══════════════════════════════════════════════════════════════════════════════
# get_afa_static()
#
# Fetches the Alien Flora of Australia (AFA) invasion status table from the
# AusPlots REST API (/afa_static) and returns it with the internal column
# schema expected by invasion_info().
# All tests mock .ausplots_api to avoid requiring a live network connection.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — minimal raw API response with all API column names
# ══════════════════════════════════════════════════════════════════════════════

make_raw_api_response <- function() {
  data.frame(
    apc_canonical_name      = "Oxalis corniculata",
    apc_name                = "Oxalis corniculata L.",
    aligned_name            = "Oxalis corniculata",
    act_unified_status      = "introduced",
    nsw_unified_status      = "introduced",
    nt_unified_status       = NA_character_,
    qld_unified_status      = "introduced",
    sa_unified_status       = "introduced",
    tas_unified_status      = "introduced",
    vic_unified_status      = "introduced",
    wa_unified_status       = "introduced",
    ar                      = NA_character_,
    coi                     = NA_character_,
    cai                     = NA_character_,
    chi                     = NA_character_,
    lhi                     = NA_character_,
    ni                      = NA_character_,
    hi                      = NA_character_,
    csi                     = NA_character_,
    mdi                     = NA_character_,
    afa_unified_status      = "introduced",
    national_status         = "introduced",
    griis_alien_status      = "invasive",
    establishment_means     = "introduced",
    degree_of_establishment = "established",
    stringsAsFactors = FALSE
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# get_afa_static() — OUTPUT STRUCTURE
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: result is a data.frame
# Input:         API mocked to return one row
# Expected:      is.data.frame(result) is TRUE
test_that("get_afa_static: returns a data.frame", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) make_raw_api_response())
  result <- get_afa_static()
  expect_true(is.data.frame(result))
})

# What it tests: API columns are renamed to internal schema names
# Input:         API mocked with API column names (e.g. "apc_canonical_name")
# Expected:      result contains "canonical_name", not "apc_canonical_name"
test_that("get_afa_static: renames API columns to internal schema", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) make_raw_api_response())
  result <- get_afa_static()
  expect_true("canonical_name" %in% names(result))
  expect_false("apc_canonical_name" %in% names(result))
  expect_true("accepted_name_usage" %in% names(result))
  expect_false("aligned_name" %in% names(result))
  expect_true("status_ACT_harmonised" %in% names(result))
  expect_false("act_unified_status" %in% names(result))
  expect_true("griis_is_invasive" %in% names(result))
  expect_false("griis_alien_status" %in% names(result))
})

# What it tests: missing declared/threatened/family columns are added as NA
# Input:         API response without declared/threatened columns (as is real API)
# Expected:      result contains declared_state_details, threatened_state_details,
#                threatened_epbc_details, declared_national_details, and family,
#                all NA
test_that("get_afa_static: adds missing declared/threatened/family columns as NA", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) make_raw_api_response())
  result <- get_afa_static()
  added_cols <- c("declared_state_details", "declared_national_details",
                  "threatened_state_details", "threatened_epbc_details", "family")
  for (col in added_cols) {
    expect_true(col %in% names(result), info = paste("missing column:", col))
    expect_true(is.na(result[[col]][1]), info = paste("not NA:", col))
  }
})

# What it tests: result has all 30 columns required by invasion_info()
# Input:         API mocked with full raw response
# Expected:      all required_afa_cols present in result
test_that("get_afa_static: result satisfies invasion_info() schema (30 required columns)", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) make_raw_api_response())
  result <- get_afa_static()
  required_cols <- c(
    "scientific_name", "canonical_name", "accepted_name_usage", "family",
    "unified_AFA_status_continental", "unified_AFA_status_all_territories",
    "establishment_means", "degree_of_establishment",
    "declared_state_details", "declared_national_details",
    "threatened_state_details", "threatened_epbc_details",
    "status_ACT_harmonised", "status_NSW_harmonised", "status_NT_harmonised",
    "status_QLD_harmonised", "status_SA_harmonised", "status_TAS_harmonised",
    "status_VIC_harmonised", "status_WA_harmonised", "status_AR_harmonised",
    "status_CoI_harmonised", "status_CaI_harmonised", "status_ChI_harmonised",
    "status_LHI_harmonised", "status_NI_harmonised", "status_HI_harmonised",
    "status_CSI_harmonised", "status_MDI_harmonised", "griis_is_invasive"
  )
  missing <- setdiff(required_cols, names(result))
  expect_equal(missing, character(0))
})

# What it tests: result row count matches API response row count
# Input:         API mocked to return 1 row
# Expected:      nrow(result) == 1
test_that("get_afa_static: result has same number of rows as API response", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) make_raw_api_response())
  result <- get_afa_static()
  expect_equal(nrow(result), 1L)
})

# ══════════════════════════════════════════════════════════════════════════════
# get_afa_static() — ERROR HANDLING
# ══════════════════════════════════════════════════════════════════════════════

# ── CAUGHT paths (tryCatch fires) ─────────────────────────────────────────────

# What it tests: .ausplots_api failure is caught and message identifies the function
# Input:         .ausplots_api mocked to throw a network error
# Expected:      error message contains "Failed to retrieve AFA invasion status data:"
#                and the original cause
test_that("get_afa_static: .ausplots_api failure is caught and wrapped with context", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) {
    stop("Unexpected error while calling API path '/afa_static': connection refused")
  })
  expect_error(
    get_afa_static(),
    "Failed to retrieve AFA invasion status data:.*connection refused"
  )
})

# What it tests: wrapped error preserves the original cause from .ausplots_api
# Input:         .ausplots_api mocked to throw the known "Data extraction aborted" message
# Expected:      original message text appears inside the wrapped error
test_that("get_afa_static: wrapped error preserves original cause", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) {
    stop("Data extraction aborted due to database connection issue.")
  })
  expect_error(
    get_afa_static(),
    "Data extraction aborted due to database connection issue"
  )
})

# What it tests: as.data.frame failure on non-coercible API return is caught and wrapped
# Input:         .ausplots_api returns an environment — as.data.frame throws on it
# Expected:      tryCatch catches the error and wraps it with function context
test_that("get_afa_static: as.data.frame failure on non-coercible return is caught", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) new.env())
  expect_error(
    get_afa_static(),
    "Failed to retrieve AFA invasion status data:"
  )
})

# ── UNCAUGHT paths (silent wrong output — tryCatch never fires) ───────────────

# What it tests: NULL API return causes error in NA-fill loop, caught and wrapped by tryCatch
# Input:         .ausplots_api returns NULL → as.data.frame(NULL) = 0-row df →
#                NA-fill loop tries to assign NA_character_ to 0-row df → error
# Expected:      tryCatch catches it; message contains "Failed to retrieve AFA invasion status data:"
test_that("get_afa_static: NULL API return causes NA-fill error caught by tryCatch", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) NULL)
  expect_error(
    get_afa_static(),
    "Failed to retrieve AFA invasion status data:"
  )
})

# What it tests: unrecognised API column names cause all renames to be silently skipped
# Input:         .ausplots_api returns df with only an unrecognised column
# Expected:      rename loop skips everything; required schema cols absent; no error thrown
test_that("get_afa_static: unrecognised API columns cause renames to be silently skipped", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) {
    data.frame(unexpected_col = "value", stringsAsFactors = FALSE)
  })
  result <- get_afa_static()
  expect_false("canonical_name" %in% names(result))
  expect_false("status_ACT_harmonised" %in% names(result))
  expect_true("unexpected_col" %in% names(result))
})

# What it tests: rename is silently skipped when internal column name already present
# Input:         df with both apc_canonical_name AND canonical_name present
# Expected:      guard condition fires; both columns remain; no error thrown
test_that("get_afa_static: rename skipped when internal column name already present", {
  mockery::stub(get_afa_static, ".ausplots_api", function(...) {
    data.frame(
      apc_canonical_name = "Oxalis corniculata",
      canonical_name     = "already here",
      stringsAsFactors   = FALSE
    )
  })
  result <- get_afa_static()
  expect_true("apc_canonical_name" %in% names(result))
  expect_true("canonical_name" %in% names(result))
  expect_equal(result$canonical_name, "already here")
})

# What it tests: existing family column is not overwritten with NA
# Input:         .ausplots_api returns df that already contains family = "Oxalidaceae"
# Expected:      NA-fill guard skips family; original value is preserved
test_that("get_afa_static: existing family column is not overwritten with NA", {
  raw <- make_raw_api_response()
  raw$family <- "Oxalidaceae"
  mockery::stub(get_afa_static, ".ausplots_api", function(...) raw)
  result <- get_afa_static()
  expect_equal(result$family, "Oxalidaceae")
})

# What it tests: none of the five schema-completion columns are overwritten when API provides them
# Input:         API response already contains all five declared/threatened/family columns
# Expected:      original values preserved; none become NA
test_that("get_afa_static: all five schema-completion columns preserved when API provides them", {
  raw <- make_raw_api_response()
  raw$declared_state_details    <- "some detail"
  raw$declared_national_details <- "national detail"
  raw$threatened_state_details  <- "state threat"
  raw$threatened_epbc_details   <- "epbc detail"
  raw$family                    <- "Oxalidaceae"
  mockery::stub(get_afa_static, ".ausplots_api", function(...) raw)
  result <- get_afa_static()
  expect_equal(result$declared_state_details,    "some detail")
  expect_equal(result$declared_national_details, "national detail")
  expect_equal(result$threatened_state_details,  "state threat")
  expect_equal(result$threatened_epbc_details,   "epbc detail")
  expect_equal(result$family,                    "Oxalidaceae")
})