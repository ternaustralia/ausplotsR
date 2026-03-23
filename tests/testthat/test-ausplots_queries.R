library(testthat)
library(jsonlite)

# ══════════════════════════════════════════════════════════════════════════════
# FIXTURE LOADER
#
# Reads a JSON file from tests/testthat/fixtures/ and returns a data.frame.
# test_path() resolves correctly whether tests are run via devtools::test(),
# R CMD check, or directly from the console.
# To update mock data: edit the corresponding JSON file in fixtures/ — no R
# code changes needed.
# ══════════════════════════════════════════════════════════════════════════════

fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name), simplifyDataFrame = TRUE)
}

# Central mock dispatcher — routes by API path to its fixture file.
# Mirrors exactly what the real .ausplots_api() does: returns a data.frame
# parsed from JSON.
mock_api <- function(path, query) {
  path_to_fixture <- list(
    "search"                   = "search.json",
    "site"                     = "site.json",
    "structural_summary"       = "structural_summary.json",
    "soil_subsite"             = "soil_subsite.json",
    "soil_bulk_density"        = "soil_bulk_density.json",
    "soil_characterisation"    = "soil_characterisation.json",
    "veg_basal"                = "veg_basal.json",
    "veg_voucher"              = "veg_voucher.json",
    "veg_pi"                   = "veg_pi.json",
    "metadata-dictionary.json" = "metadata_dictionary.json"
  )
  fname <- path_to_fixture[[path]]
  if (is.null(fname)) return(data.frame())
  fixture(fname)
}

# ══════════════════════════════════════════════════════════════════════════════
# list_available_plots() — INPUT VALIDATION
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: bounding_box must be numeric — a character string is rejected
#   immediately before any API call is made
# Input:         Plot_IDs = "none", bounding_box = "invalid" (character, not numeric)
# Expected:      error "Bounding box must be a numeric vector of length 4"
test_that("list_available_plots: bounding_box must be numeric", {
  expect_error(
    ausplotsR:::list_available_plots(Plot_IDs = "none", bounding_box = "invalid"),
    "Bounding box must be a numeric vector of length 4"
  )
})

# What it tests: bounding_box must have exactly 4 elements (xmin, xmax, ymin, ymax)
# Input:         Plot_IDs = "none", bounding_box = c(120, 140) — only 2 of 4 values
# Expected:      error "Bounding box must be a numeric vector of length 4"
test_that("list_available_plots: bounding_box must have exactly 4 elements", {
  expect_error(
    ausplotsR:::list_available_plots(Plot_IDs = "none", bounding_box = c(120, 140)),
    "Bounding box must be a numeric vector of length 4"
  )
})

# What it tests: bounding_box with 5 elements is rejected — must be exactly 4
# Input:         Plot_IDs = "none", bounding_box = c(120, 140, -30, -10, 0) — 5 elements
# Expected:      error "Bounding box must be a numeric vector of length 4"
test_that("list_available_plots: bounding_box with 5 elements is rejected", {
  expect_error(
    ausplotsR:::list_available_plots(Plot_IDs = "none", bounding_box = c(120, 140, -30, -10, 0)),
    "Bounding box must be a numeric vector of length 4"
  )
})

# What it tests: bounding_box with 3 elements is rejected
# Input:         Plot_IDs = "none", bounding_box = c(120, 140, -30) — 3 elements
# Expected:      error "Bounding box must be a numeric vector of length 4"
test_that("list_available_plots: bounding_box with 3 elements is rejected", {
  expect_error(
    ausplotsR:::list_available_plots(Plot_IDs = "none", bounding_box = c(120, 140, -30)),
    "Bounding box must be a numeric vector of length 4"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# list_available_plots() — MOCKED UNIT TESTS
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: with no filters, list_available_plots returns a sorted character
#   vector of unique site_location_names from the "search" API path
# Input:         Plot_IDs = "none", mock returns fixtures/search.json
# Expected:      character vector containing "SATFLB0004"
test_that("list_available_plots: returns sorted unique site_location_names", {
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- ausplotsR:::list_available_plots(Plot_IDs = "none")
  expect_type(result, "character")
  expect_gt(length(result), 0)
  expect_true("SATFLB0004" %in% result)
})

# What it tests: when Plot_IDs are in site_unique format (hyphenated), the result
#   is a vector of "site_location_name-visit_id" strings
# Input:         Plot_IDs = c("SATFLB0004-56789"), mock returns fixtures/search.json
# Expected:      result contains a hyphenated string
test_that("list_available_plots: site_unique IDs return hyphenated results", {
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- ausplotsR:::list_available_plots(Plot_IDs = c("SATFLB0004-56789"))
  expect_type(result, "character")
  expect_true(any(grepl("-", result)))
})

# ══════════════════════════════════════════════════════════════════════════════
# .ausplots_api_with_plot_filter() — QUERY BUILDING (mocked)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: site_location_name format IDs (no hyphen) result in a query
#   that contains a site_location_name filter in PostgREST in.() syntax
# Input:         Plot_IDs = c("SATFLB0004", "QDAMGD0022")
# Expected:      query passed to .ausplots_api contains site_location_name key
#                with a value matching both plot names
test_that(".ausplots_api_with_plot_filter: site_location_name IDs build correct filter", {
  captured_query <- NULL
  local_mocked_bindings(
    .ausplots_api = function(path, query) { captured_query <<- query; data.frame() },
    .package = "ausplotsR"
  )
  ausplotsR:::.ausplots_api_with_plot_filter("site", c("SATFLB0004", "QDAMGD0022"))
  expect_true("site_location_name" %in% names(captured_query))
  expect_match(captured_query$site_location_name, "SATFLB0004")
  expect_match(captured_query$site_location_name, "QDAMGD0022")
})

# What it tests: site_unique format IDs (hyphenated) extract the visit_id part
#   and build a site_location_visit_id filter, NOT a site_location_name filter
# Input:         Plot_IDs = c("SATFLB0004-11111", "QDAMGD0022-22222")
# Expected:      query contains site_location_visit_id with "11111" and "22222",
#                does NOT contain site_location_name
test_that(".ausplots_api_with_plot_filter: site_unique IDs extract visit IDs for filter", {
  captured_query <- NULL
  local_mocked_bindings(
    .ausplots_api = function(path, query) { captured_query <<- query; data.frame() },
    .package = "ausplotsR"
  )
  ausplotsR:::.ausplots_api_with_plot_filter("site", c("SATFLB0004-11111", "QDAMGD0022-22222"))
  expect_true("site_location_visit_id" %in% names(captured_query))
  expect_false("site_location_name" %in% names(captured_query))
  expect_match(captured_query$site_location_visit_id, "11111")
  expect_match(captured_query$site_location_visit_id, "22222")
})

# What it tests: empty Plot_IDs vector results in no plot filter being added
# Input:         Plot_IDs = c()  (empty), no extra_query
# Expected:      query contains neither site_location_name nor site_location_visit_id
test_that(".ausplots_api_with_plot_filter: empty Plot_IDs adds no filter", {
  captured_query <- NULL
  local_mocked_bindings(
    .ausplots_api = function(path, query) { captured_query <<- query; data.frame() },
    .package = "ausplotsR"
  )
  ausplotsR:::.ausplots_api_with_plot_filter("site", c())
  expect_false("site_location_name" %in% names(captured_query))
  expect_false("site_location_visit_id" %in% names(captured_query))
})

# What it tests: extra_query parameters are merged into the final query
# Input:         Plot_IDs = c("SATFLB0004"), extra_query has a "herbarium_determination" filter
# Expected:      query contains both site_location_name AND herbarium_determination
test_that(".ausplots_api_with_plot_filter: extra_query is merged into the query", {
  captured_query <- NULL
  local_mocked_bindings(
    .ausplots_api = function(path, query) { captured_query <<- query; data.frame() },
    .package = "ausplotsR"
  )
  ausplotsR:::.ausplots_api_with_plot_filter(
    "search",
    c("SATFLB0004"),
    extra_query = list(herbarium_determination = "ilike.*Eucalyptus*")
  )
  expect_true("herbarium_determination" %in% names(captured_query))
  expect_true("site_location_name" %in% names(captured_query))
})

# ══════════════════════════════════════════════════════════════════════════════
# .ausplots_api_with_specified_plot_ids() — QUERY BUILDING (mocked)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: non-hyphenated Plot_IDs build a site_location_name filter
# Input:         Plot_IDs = c("SATFLB0004")
# Expected:      query contains site_location_name matching "SATFLB0004"
test_that(".ausplots_api_with_specified_plot_ids: builds site_location_name filter", {
  captured_query <- NULL
  local_mocked_bindings(
    .ausplots_api = function(path, query) { captured_query <<- query; data.frame() },
    .package = "ausplotsR"
  )
  ausplotsR:::.ausplots_api_with_specified_plot_ids("site", c("SATFLB0004"))
  expect_true("site_location_name" %in% names(captured_query))
  expect_match(captured_query$site_location_name, "SATFLB0004")
})

# What it tests: hyphenated site_unique IDs build a site_location_visit_id filter
# Input:         Plot_IDs = c("SATFLB0004-99999")
# Expected:      query contains site_location_visit_id with "99999",
#                does NOT contain site_location_name
test_that(".ausplots_api_with_specified_plot_ids: site_unique IDs build visit_id filter", {
  captured_query <- NULL
  local_mocked_bindings(
    .ausplots_api = function(path, query) { captured_query <<- query; data.frame() },
    .package = "ausplotsR"
  )
  ausplotsR:::.ausplots_api_with_specified_plot_ids("site", c("SATFLB0004-99999"))
  expect_true("site_location_visit_id" %in% names(captured_query))
  expect_false("site_location_name" %in% names(captured_query))
  expect_match(captured_query$site_location_visit_id, "99999")
})

# ══════════════════════════════════════════════════════════════════════════════
# .make_user_agent() — NO NETWORK NEEDED
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: .make_user_agent() returns a non-empty character string
# Input:         no arguments
# Expected:      result is a single character string of length > 0
test_that(".make_user_agent: returns a character string", {
  result <- ausplotsR:::.make_user_agent()
  expect_type(result, "character")
  expect_gt(nchar(result), 0)
})

# What it tests: the user-agent string identifies the ausplotsR package
# Input:         no arguments
# Expected:      result contains "ausplotsR/"
test_that(".make_user_agent: string contains ausplotsR package identifier", {
  result <- ausplotsR:::.make_user_agent()
  expect_match(result, "ausplotsR/")
})

# What it tests: the user-agent string includes the httr version
# Input:         no arguments
# Expected:      result contains "httr/"
test_that(".make_user_agent: string contains httr version", {
  result <- ausplotsR:::.make_user_agent()
  expect_match(result, "httr/")
})

# What it tests: the string is cached — two successive calls return the same value
# Input:         no arguments (called twice)
# Expected:      both results are identical strings
test_that(".make_user_agent: result is cached and consistent across calls", {
  first  <- ausplotsR:::.make_user_agent()
  second <- ausplotsR:::.make_user_agent()
  expect_identical(first, second)
})

# ══════════════════════════════════════════════════════════════════════════════
# extract_site_info() — STATE COLUMN DERIVATION (mocked)
#
# Each state test loads a single-row fixture from fixtures/site_<state>.json.
# To update: paste one real API row into the corresponding JSON file.
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: "NS" prefix is expanded to "NSW"
# Input:         fixtures/site_nsw.json — site_location_name = "NSXXXX0001"
# Expected:      result$state[[1]] == "NSW"
test_that("extract_site_info: NS prefix is expanded to NSW", {
  local_mocked_bindings(
    .ausplots_api = function(path, query) fixture("site_nsw.json"),
    .package = "ausplotsR"
  )
  result <- ausplotsR:::extract_site_info("NSXXXX0001")
  expect_equal(result$state[[1]], "NSW")
})

# What it tests: "QD" prefix is expanded to "QLD"
# Input:         fixtures/site_qld.json — site_location_name = "QDXXXX0001"
# Expected:      result$state[[1]] == "QLD"
test_that("extract_site_info: QD prefix is expanded to QLD", {
  local_mocked_bindings(
    .ausplots_api = function(path, query) fixture("site_qld.json"),
    .package = "ausplotsR"
  )
  result <- ausplotsR:::extract_site_info("QDXXXX0001")
  expect_equal(result$state[[1]], "QLD")
})

# What it tests: "TC" prefix is expanded to "TAS"
# Input:         fixtures/site_tas.json — site_location_name = "TCXXXX0001"
# Expected:      result$state[[1]] == "TAS"
test_that("extract_site_info: TC prefix is expanded to TAS", {
  local_mocked_bindings(
    .ausplots_api = function(path, query) fixture("site_tas.json"),
    .package = "ausplotsR"
  )
  result <- ausplotsR:::extract_site_info("TCXXXX0001")
  expect_equal(result$state[[1]], "TAS")
})

# What it tests: "VC" prefix is expanded to "VIC"
# Input:         fixtures/site_vic.json — site_location_name = "VCXXXX0001"
# Expected:      result$state[[1]] == "VIC"
test_that("extract_site_info: VC prefix is expanded to VIC", {
  local_mocked_bindings(
    .ausplots_api = function(path, query) fixture("site_vic.json"),
    .package = "ausplotsR"
  )
  result <- ausplotsR:::extract_site_info("VCXXXX0001")
  expect_equal(result$state[[1]], "VIC")
})

# What it tests: unrecognised prefix kept as 2-char substring ("SA")
# Input:         fixtures/site_sa.json — site_location_name = "SATFLB0004"
# Expected:      result$state[[1]] == "SA"
test_that("extract_site_info: unrecognised prefix kept as 2-char substring", {
  local_mocked_bindings(
    .ausplots_api = function(path, query) fixture("site_sa.json"),
    .package = "ausplotsR"
  )
  result <- ausplotsR:::extract_site_info("SATFLB0004")
  expect_equal(result$state[[1]], "SA")
})

# What it tests: mixed states from multiple sites all derived correctly
# Input:         fixtures/site_multi_state.json — 5 rows, one per state
# Expected:      each row's state column matches the expected expansion
test_that("extract_site_info: multiple states all derived correctly in one call", {
  multi <- fixture("site_multi_state.json")
  local_mocked_bindings(
    .ausplots_api = function(path, query) multi,
    .package = "ausplotsR"
  )
  result <- ausplotsR:::extract_site_info(multi$site_location_name)
  expect_equal(unlist(result$state[result$site_location_name == "SATFLB0004"]), "SA")
  expect_equal(unlist(result$state[result$site_location_name == "NSAMDD0016"]), "NSW")
  expect_equal(unlist(result$state[result$site_location_name == "QDAMGD0025"]), "QLD")
  expect_equal(unlist(result$state[result$site_location_name == "TCATNM0003"]), "TAS")
  expect_equal(unlist(result$state[result$site_location_name == "VCAMDD0002"]), "VIC")
})

# ══════════════════════════════════════════════════════════════════════════════
# extract_*() — ALL DATA MODULES (mocked)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: extract_struct_summ() returns a data.frame with site_location_name
# Input:         fixtures/structural_summary.json
# Expected:      result is a data.frame with site_location_name column
test_that("extract_struct_summ: returns data.frame from structural_summary path", {
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- ausplotsR:::extract_struct_summ("SATFLB0004")
  expect_s3_class(result, "data.frame")
  expect_true("site_location_name" %in% names(result))
})

# What it tests: extract_soil_subsites() returns a data.frame with site_location_name
# Input:         fixtures/soil_subsite.json
# Expected:      result is a data.frame with site_location_name column
test_that("extract_soil_subsites: returns data.frame from soil_subsite path", {
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- ausplotsR:::extract_soil_subsites("SATFLB0004")
  expect_s3_class(result, "data.frame")
  expect_true("site_location_name" %in% names(result))
})

# What it tests: extract_bulk_density() returns a data.frame with site_location_name
# Input:         fixtures/soil_bulk_density.json
# Expected:      result is a data.frame with site_location_name column
test_that("extract_bulk_density: returns data.frame from soil_bulk_density path", {
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- ausplotsR:::extract_bulk_density("SATFLB0004")
  expect_s3_class(result, "data.frame")
  expect_true("site_location_name" %in% names(result))
})

# What it tests: extract_soil_char() returns a data.frame with site_location_name
# Input:         fixtures/soil_characterisation.json
# Expected:      result is a data.frame with site_location_name column
test_that("extract_soil_char: returns data.frame from soil_characterisation path", {
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- ausplotsR:::extract_soil_char("SATFLB0004")
  expect_s3_class(result, "data.frame")
  expect_true("site_location_name" %in% names(result))
})

# What it tests: extract_hits() returns a data.frame with transect and point_number
# Input:         fixtures/veg_pi.json
# Expected:      result is a data.frame containing transect and point_number
test_that("extract_hits: returns data.frame with transect and point_number", {
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- ausplotsR:::extract_hits("SATFLB0004")
  expect_s3_class(result, "data.frame")
  expect_true("transect" %in% names(result))
  expect_true("point_number" %in% names(result))
})

# What it tests: extract_vouch() returns a data.frame with voucher columns
# Input:         fixtures/veg_voucher.json
# Expected:      result contains herbarium_determination and standardised_name columns
test_that("extract_vouch: returns data.frame with voucher columns", {
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- ausplotsR:::extract_vouch(
    "SATFLB0004",
    herbarium_determination_search = "Eucalyptus"
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("site_location_name", "herbarium_determination", "standardised_name") %in% names(result)))
})

# What it tests: extract_basal() returns a data.frame with basal area columns
# Input:         fixtures/veg_basal.json
# Expected:      result contains herbarium_determination, basal_area, point_id columns
test_that("extract_basal: returns data.frame with basal area columns", {
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- ausplotsR:::extract_basal(
    "SATFLB0004",
    herbarium_determination_search = "Eucalyptus"
  )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("site_location_name", "herbarium_determination", "basal_area", "point_id") %in% names(result)))
})

# ══════════════════════════════════════════════════════════════════════════════
# .get_metadata_dictionary() — CACHING BEHAVIOUR (mocked)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: returns a data.frame with table_name, variable, description columns
# Input:         fixtures/metadata_dictionary.json
# Expected:      result is a data.frame with those 3 columns
test_that(".get_metadata_dictionary: returns data.frame and caches result", {
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  cache_env <- get("cache", envir = asNamespace("ausplotsR"))
  cache_env$metadata_dictionary <- NULL

  result <- ausplotsR:::.get_metadata_dictionary()
  expect_s3_class(result, "data.frame")
  expect_true(all(c("table_name", "variable", "description") %in% names(result)))
})

# What it tests: second call uses cache, not the API again — call count stays at 1
# Input:         mock called twice; second should hit cache
# Expected:      .ausplots_api called exactly once
test_that(".get_metadata_dictionary: second call uses cache, not the API again", {
  call_count <- 0
  local_mocked_bindings(
    .ausplots_api = function(path, query) {
      call_count <<- call_count + 1
      fixture("metadata_dictionary.json")
    },
    .package = "ausplotsR"
  )
  cache_env <- get("cache", envir = asNamespace("ausplotsR"))
  cache_env$metadata_dictionary <- NULL

  ausplotsR:::.get_metadata_dictionary()
  ausplotsR:::.get_metadata_dictionary()
  expect_equal(call_count, 1)
})

# What it tests: force_refresh=TRUE bypasses the cache — call count reaches 2
# Input:         called twice; second call has force_refresh = TRUE
# Expected:      .ausplots_api called exactly twice
test_that(".get_metadata_dictionary: force_refresh=TRUE bypasses cache", {
  call_count <- 0
  local_mocked_bindings(
    .ausplots_api = function(path, query) {
      call_count <<- call_count + 1
      fixture("metadata_dictionary.json")
    },
    .package = "ausplotsR"
  )
  cache_env <- get("cache", envir = asNamespace("ausplotsR"))
  cache_env$metadata_dictionary <- NULL

  ausplotsR:::.get_metadata_dictionary()
  ausplotsR:::.get_metadata_dictionary(force_refresh = TRUE)
  expect_equal(call_count, 2)
})

# ══════════════════════════════════════════════════════════════════════════════
# INTEGRATION TESTS (require live API — skipped automatically if offline)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: extract_site_info() against real API returns data.frame with state
# Input:         Plot_IDs = "SATFLB0004" (real plot)
# Expected:      result is a data.frame with state and site_location_name columns
test_that("extract_site_info: integration — returns data.frame with state column", {
  skip_if_offline()
  result <- ausplotsR:::extract_site_info("SATFLB0004")
  expect_s3_class(result, "data.frame")
  expect_true("state" %in% names(result))
  expect_true("site_location_name" %in% names(result))
})

# What it tests: list_available_plots() against real API returns matching plot names
# Input:         plot_search = "SATFLB", Plot_IDs = "none"
# Expected:      character vector of length > 0
test_that("list_available_plots: integration — returns character vector of plot names", {
  skip_if_offline()
  result <- ausplotsR:::list_available_plots(Plot_IDs = "none", plot_search = "SATFLB")
  expect_type(result, "character")
  expect_gt(length(result), 0)
})