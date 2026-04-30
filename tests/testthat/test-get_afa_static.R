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