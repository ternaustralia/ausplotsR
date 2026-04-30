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

# ══════════════════════════════════════════════════════════════════════════════
# .ausplots_api() — ERROR CASE UNIT TESTS (mocked)
#
# These tests exercise the error-handling paths identified in the gap analysis:
#   1. Non-paginated path: fromJSON failure raises stop().
#   2. Non-paginated path: wrong content-type raises stop().
#   3. Non-paginated path: HTTP 4xx propagates out.
#   4. Paginated path: all chunks returning 503 hits "Server is busy" stop().
#   5. Paginated path: fromJSON failure on assembled finalResp raises stop()
#      (covers the root fix — inherits(result, "try-error") before return()).
# ══════════════════════════════════════════════════════════════════════════════

# Helpers shared across the error-case tests below.
fake_api_info_empty <- function(path, auth_header, ...) {
  m <- r2r::hashmap()
  m[["max_size"]] <- 0
  m[["columns"]] <- NULL
  m
}

# What it tests: non-paginated path — when httr::content returns invalid JSON,
#   fromJSON throws and .ausplots_api() must stop() with the abort message rather
#   than returning a try-error object to the caller.
#   Covers: result <- try(...); if(inherits(result, "try-error")) stop(...)
# Input:         try_GET returns 200; httr::content("text") returns broken JSON.
# Expected:      error "Data extraction aborted due to database connection issue."
test_that(".ausplots_api: non-paginated path raises stop() when fromJSON fails", {
  fake_resp_200 <- structure(
    list(
      status_code = 200,
      headers = list("content-type" = "application/json"),
      url = "http://fake",
      content = charToRaw("BROKEN{{{JSON")
    ),
    class = "response"
  )

  local_mocked_bindings(
    try_GET = function(path, query, auth_header, ...) fake_resp_200,
    .package = "ausplotsR"
  )

  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::.ausplots_api("/site", list()),
        "Data extraction aborted due to database connection issue."
      )
    },
    stop_for_status = function(resp, ...) invisible(resp),
    http_type       = function(resp) "application/json",
    content         = function(resp, as = "text", ...) "BROKEN{{{JSON",
    .package = "httr"
  )
})

# What it tests: non-paginated path — a non-JSON content-type stops immediately
#   before any JSON parsing is attempted.
# Input:         try_GET returns 200 with content-type "text/html".
# Expected:      error "API did not return json"
test_that(".ausplots_api: non-paginated path stops when content-type is not JSON", {
  fake_resp_html <- structure(
    list(
      status_code = 200,
      headers = list("content-type" = "text/html"),
      url = "http://fake",
      content = charToRaw("<html></html>")
    ),
    class = "response"
  )

  local_mocked_bindings(
    try_GET = function(path, query, auth_header, ...) fake_resp_html,
    .package = "ausplotsR"
  )

  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::.ausplots_api("/site", list()),
        "API did not return json"
      )
    },
    stop_for_status = function(resp, ...) invisible(resp),
    http_type       = function(resp) "text/html",
    .package = "httr"
  )
})

# What it tests: non-paginated path — a 4xx HTTP error from stop_for_status
#   propagates out of .ausplots_api() as a thrown condition.
# Input:         try_GET returns 404; httr::stop_for_status raises.
# Expected:      any error is raised
test_that(".ausplots_api: non-paginated path propagates HTTP 4xx error", {
  fake_resp_404 <- structure(
    list(
      status_code = 404,
      headers = list("content-type" = "application/json"),
      url = "http://fake",
      content = charToRaw("Not Found")
    ),
    class = "response"
  )

  local_mocked_bindings(
    try_GET = function(path, query, auth_header, ...) fake_resp_404,
    .package = "ausplotsR"
  )

  with_mocked_bindings(
    {
      expect_error(ausplotsR:::.ausplots_api("/site", list()))
    },
    stop_for_status = function(resp, ...) stop("HTTP 404 Not Found"),
    http_type       = function(resp) "application/json",
    .package = "httr"
  )
})

# What it tests: paginated path — when fromJSON on the assembled finalResp string
#   fails, .ausplots_api() must stop() rather than silently returning the try-error
#   object to callers. This is the root fix added to ausplots_queries.r.
#   Covers: result <- try(...); if(inherits(result, "try-error")) stop(...)
#           in the paginated finalising block.
# Input:         first try_GET → 503 (triggers pagination); second → valid page body
#               (accumulated into finalResp); third → NULL body (triggers finalising).
#               httr::content("text") returns corrupted JSON on the second text call
#               so that fromJSON on finalResp throws.
# Expected:      error "Data extraction aborted due to database connection issue."
test_that(".ausplots_api: paginated path raises stop() when final fromJSON fails", {
  call_n <- 0L

  page_body <- '[{"site_location_name":"SATFLB0004"}]'

  fake_resp_503 <- structure(
    list(status_code = 503, headers = list("Content-Type" = "application/json"),
         url = "http://fake/site", content = charToRaw("[]")),
    class = "response"
  )
  fake_resp_page <- structure(
    list(status_code = 200, headers = list("Content-Type" = "application/json"),
         url = "http://fake/site", content = charToRaw(page_body)),
    class = "response"
  )
  fake_resp_null <- structure(
    list(status_code = 200, headers = list("Content-Type" = "application/json"),
         url = "http://fake/site", content = charToRaw("null")),
    class = "response"
  )

  local_mocked_bindings(
    try_GET = function(path, query, auth_header, ...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) fake_resp_503       # triggers pagination branch
      else if (call_n == 2L) fake_resp_page  # first valid chunk
      else fake_resp_null                    # NULL body -> triggers finalising
    },
    api_info = fake_api_info_empty,
    .package = "ausplotsR"
  )

  # Sys.sleep is in base, not httr — mock it separately
  local_mocked_bindings(
    Sys.sleep = function(s) invisible(NULL),
    .package = "base"
  )

  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::.ausplots_api("/site", list()),
        "Data extraction aborted due to database connection issue."
      )
    },
    # content("parsed"): used to compute bodySize inside the loop.
    #   Returns a data frame for the page chunk (bodySize = 1) and NULL for null.
    # content("text"): used to accumulate chunks into finalResp.
    #   Returns corrupt JSON for the page chunk so that the final fromJSON() fails.
    content = function(resp, as = "parsed", encoding = "UTF-8", ...) {
      raw_txt <- rawToChar(resp$content)
      if (identical(as, "text")) {
        if (raw_txt == "null") return("null")
        return("CORRUPT{{JSON")   # corrupt finalResp so final fromJSON() throws
      }
      if (identical(as, "parsed")) {
        if (raw_txt == "null") return(NULL)
        return(jsonlite::fromJSON(raw_txt))  # valid parse for bodySize check
      }
    },
    .package = "httr"
  )
})

# What it tests: api_info() — when the API response has no content-range header
#   (PostgREST omits it for small result sets), the try() around strsplit returns
#   a try-error object (message: "non-character argument"), because
#   `headers[["content-range"]]` is NULL and strsplit requires a character vector.
#   The unguarded `if (content_length > 0)` does a lexicographic comparison
#   ("Error in ..." > "0" → TRUE), so max_size is silently set to the try-error
#   string instead of 0. This is silent wrong output — gap item A in the analysis.
# Input:         httr::GET returns 200 with no content-range header;
#               httr::content("parsed") is mocked to return a valid list.
# Expected:      api_info() returns normally; max_size is a try-error object
#               whose message contains "non-character argument".
test_that("api_info: missing content-range header silently sets max_size to try-error string", {
  fake_resp <- structure(
    list(
      status_code  = 200,
      url          = "http://fake/site",
      # all_headers has no content-range entry — simulates a small result set
      all_headers  = list(list(
        status  = 200L,
        headers = list("content-type" = "application/json")  # no content-range
      )),
      headers = list("content-type" = "application/json"),
      content = charToRaw('[{"col1":"val1"}]')
    ),
    class = "response"
  )

  with_mocked_bindings(
    {
      result <- ausplotsR:::api_info("/site", "")
      # max_size should be 0 for a response with no range header, but instead
      # it receives the try-error object because the unguarded `> 0` comparison
      # treats the error string as lexicographically greater than "0".
      expect_true(inherits(result[["max_size"]], "try-error"))
      expect_match(as.character(result[["max_size"]]), "non-character argument")
    },
    GET     = function(...) fake_resp,
    content = function(resp, as = "parsed", ...) list(col1 = "val1"),
    .package = "httr"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# PREVIOUSLY UNTESTED ERROR PATHS
# Covers all ❌ entries from .analysis/error_coverage.md (#1, #2, #4, #5/#6,
# #7, #11, #12, #13, #15, #20, #21, #22, #23).
# Each test documents WHY no prior test existed.
# ══════════════════════════════════════════════════════════════════════════════

# ─── #1 ─ try_GET: httr::GET network failure ─────────────────────────────────
# Why no prior test: try_GET is a one-line wrapper; uncaught propagation was
#   considered trivially obvious and assumed covered implicitly by the
#   .ausplots_api error tests.
# Input:         httr::GET is mocked to throw a network error.
# Expected:      the error propagates out of try_GET unchanged.
test_that("try_GET: propagates httr::GET network failure", {
  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::try_GET("/site", list(), ""),
        "simulated network error"
      )
    },
    GET = function(...) stop("simulated network error"),
    .package = "httr"
  )
})

# ─── #2 ─ api_info: httr::GET network failure ────────────────────────────────
# Why no prior test: same rationale as #1. api_info calls httr::GET directly;
#   network failure is an obvious pass-through. Only the content-range missing
#   case was explored in the existing suite.
# Input:         httr::GET is mocked to throw a network error.
# Expected:      the error propagates out of api_info unchanged.
test_that("api_info: propagates httr::GET network failure", {
  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::api_info("/site", ""),
        "simulated network error"
      )
    },
    GET = function(...) stop("simulated network error"),
    .package = "httr"
  )
})

# ─── #4 ─ api_info: httr::content("parsed") throws ──────────────────────────
# Why no prior test: the api_info test suite was written only for the
#   content-range missing path. The content-type parser error path in the
#   same function was overlooked during the initial test-writing pass.
# Input:         httr::GET returns 200 with a valid content-range header
#               (so strsplit succeeds and content_length is computed);
#               httr::content("parsed") is mocked to throw.
# Expected:      the error propagates out of api_info.
test_that("api_info: propagates httr::content error when content-type is unrecognised", {
  fake_resp_with_range <- structure(
    list(
      status_code  = 200,
      url          = "http://fake/site",
      all_headers  = list(list(
        status  = 200L,
        headers = list(
          "content-type"  = "application/json",
          "content-range" = "0-0/42"  # present → strsplit succeeds
        )
      )),
      headers = list("content-type" = "application/json"),
      content = charToRaw('[{"col1":"val1"}]')
    ),
    class = "response"
  )
  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::api_info("/site", ""),
        "No automatic parser available"
      )
    },
    GET     = function(...) fake_resp_with_range,
    content = function(resp, as = "parsed", ...) {
      stop("No automatic parser available for application/octet-stream")
    },
    .package = "httr"
  )
})

# ─── #5/#6 ─ api_info: NULL parsed body → subscript out of bounds ────────────
# Why no prior test: the path was misclassified as "silent wrong output".
#   In reality: toJSON(NULL) = "null" (no error); fromJSON("null") = NULL;
#   names(NULL) = NULL; inside the 1:0 backwards loop, NULL[[1]] throws
#   "subscript out of bounds" — an uncaught propagation, not a silent return.
# Input:         httr::GET returns 200 with a valid content-range header;
#               httr::content("parsed") returns NULL (empty table).
# Expected:      error "subscript out of bounds" propagates out of api_info.
test_that("api_info: NULL parsed body throws subscript out of bounds in column loop", {
  fake_resp_with_range <- structure(
    list(
      status_code  = 200,
      url          = "http://fake/site",
      all_headers  = list(list(
        status  = 200L,
        headers = list(
          "content-type"  = "application/json",
          "content-range" = "0-0/5"
        )
      )),
      headers = list("content-type" = "application/json"),
      content = charToRaw("null")
    ),
    class = "response"
  )
  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::api_info("/site", ""),
        "subscript out of bounds"
      )
    },
    GET     = function(...) fake_resp_with_range,
    content = function(resp, as = "parsed", ...) NULL,
    .package = "httr"
  )
})

# ─── #7 ─ .ausplots_api: try_GET throws ──────────────────────────────────────
# Why no prior test: same rationale as #1. The interesting explicit stop()
#   paths (.ausplots_api content-type check, fromJSON failure) were tested;
#   the raw network failure propagation from try_GET was not.
# Input:         ausplotsR:::try_GET is mocked to throw.
# Expected:      the error propagates out of .ausplots_api.
test_that(".ausplots_api: propagates network failure from try_GET", {
  local_mocked_bindings(
    try_GET = function(...) stop("simulated network error"),
    .package = "ausplotsR"
  )
  expect_error(
    ausplotsR:::.ausplots_api("/site", list()),
    "simulated network error"
  )
})

# ─── #11 ─ .ausplots_api paginated: api_info throws ──────────────────────────
# Why no prior test: required a two-layer mock (503 on the first try_GET to
#   trigger pagination, then api_info throwing). Was informally assumed covered
#   by the dedicated api_info error tests, even though the propagation path
#   through .ausplots_api was not directly verified.
# Input:         try_GET always returns 503 (triggers pagination branch);
#               api_info is mocked to throw.
# Expected:      the api_info error propagates out of .ausplots_api.
test_that(".ausplots_api: paginated path propagates api_info failure", {
  fake_resp_503 <- structure(
    list(
      status_code = 503,
      headers     = list("Content-Type" = "application/json"),
      url         = "http://fake",
      content     = charToRaw("")
    ),
    class = "response"
  )
  local_mocked_bindings(
    try_GET  = function(...) fake_resp_503,
    api_info = function(...) stop("api_info failed"),
    .package = "ausplotsR"
  )
  local_mocked_bindings(
    Sys.sleep = function(s) invisible(NULL),
    .package  = "base"
  )
  expect_error(
    ausplotsR:::.ausplots_api("/site", list()),
    "api_info failed"
  )
})

# ─── #12 ─ .ausplots_api paginated: try_GET inside the while loop throws ─────
# Why no prior test: required a stateful mock (call 1 = 503, call 2 = throw).
#   The existing paginated test only exercised the final fromJSON failure path.
# Input:         try_GET: call 1 → 503 (triggers pagination); call 2 → throws.
# Expected:      the network error propagates out of .ausplots_api.
test_that(".ausplots_api: paginated path propagates try_GET failure inside loop", {
  call_n <- 0L
  fake_resp_503 <- structure(
    list(
      status_code = 503,
      headers     = list("Content-Type" = "application/json"),
      url         = "http://fake",
      content     = charToRaw("")
    ),
    class = "response"
  )
  local_mocked_bindings(
    try_GET = function(...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) fake_resp_503   # triggers paginated branch
      else stop("simulated network error")  # fails inside while loop
    },
    api_info = fake_api_info_empty,
    .package = "ausplotsR"
  )
  local_mocked_bindings(
    Sys.sleep = function(s) invisible(NULL),
    .package  = "base"
  )
  expect_error(
    ausplotsR:::.ausplots_api("/site", list()),
    "simulated network error"
  )
})

# ─── #13 ─ .ausplots_api paginated: httr::content("parsed") throws in loop ───
# Why no prior test: same as #12 — only the final fromJSON failure path was
#   covered; individual per-chunk failures inside the loop were not tested.
# Input:         try_GET: call 1 → 503; call 2 → 200.
#               httr::content("parsed") is mocked to throw on the chunk call.
# Expected:      the content-type error propagates out of .ausplots_api.
test_that(".ausplots_api: paginated path propagates httr::content error inside loop", {
  call_n <- 0L
  fake_resp_503 <- structure(
    list(
      status_code = 503,
      headers     = list("Content-Type" = "application/json"),
      url         = "http://fake",
      content     = charToRaw("")
    ),
    class = "response"
  )
  fake_resp_200 <- structure(
    list(
      status_code = 200,
      headers     = list("Content-Type" = "application/json"),
      url         = "http://fake",
      content     = charToRaw('[{"a":1}]')
    ),
    class = "response"
  )
  local_mocked_bindings(
    try_GET = function(...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) fake_resp_503 else fake_resp_200
    },
    api_info = fake_api_info_empty,
    .package = "ausplotsR"
  )
  local_mocked_bindings(
    Sys.sleep = function(s) invisible(NULL),
    .package  = "base"
  )
  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::.ausplots_api("/site", list()),
        "No automatic parser available"
      )
    },
    content = function(resp, as = "parsed", encoding = "UTF-8", ...) {
      stop("No automatic parser available for application/octet-stream")
    },
    .package = "httr"
  )
})

# ─── #15 ─ .ausplots_api paginated: fromJSON on page chunk throws ─────────────
# Why no prior test: required mocking jsonlite::fromJSON in its own namespace
#   while leaving httr::content mocked separately to provide a valid parsed
#   body. The added layering complexity caused this path to be skipped.
#   Note: table entry #14 (toJSON(body=NULL) throws) is a false entry —
#   jsonlite::toJSON(NULL) returns "null" without error; it flows into #15/#16.
# Input:         try_GET: call 1 → 503; call 2 → 200.
#               httr::content("parsed") returns a valid R list.
#               jsonlite::fromJSON is mocked to throw on the bodySize call.
# Expected:      the fromJSON error propagates out of .ausplots_api.
test_that(".ausplots_api: paginated path propagates fromJSON failure on page chunk", {
  call_n <- 0L
  fake_resp_503 <- structure(
    list(
      status_code = 503,
      headers     = list("Content-Type" = "application/json"),
      url         = "http://fake",
      content     = charToRaw("")
    ),
    class = "response"
  )
  fake_resp_200 <- structure(
    list(
      status_code = 200,
      headers     = list("Content-Type" = "application/json"),
      url         = "http://fake",
      content     = charToRaw('[{"a":1}]')
    ),
    class = "response"
  )
  local_mocked_bindings(
    try_GET = function(...) {
      call_n <<- call_n + 1L
      if (call_n == 1L) fake_resp_503 else fake_resp_200
    },
    api_info = fake_api_info_empty,
    .package = "ausplotsR"
  )
  local_mocked_bindings(
    Sys.sleep = function(s) invisible(NULL),
    .package  = "base"
  )
  local_mocked_bindings(
    fromJSON = function(txt, ...) stop("lexical error: invalid char in json text"),
    .package = "jsonlite"
  )
  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::.ausplots_api("/site", list()),
        "lexical error"
      )
    },
    content = function(resp, as = "parsed", encoding = "UTF-8", ...) list(a = 1),
    .package = "httr"
  )
})

# ─── #20 ─ list_available_plots: API response missing site_location_name ──────
# Why no prior test: silent wrong output — returns NULL silently with no error,
#   so the bug is invisible when testing only happy paths and thrown conditions.
# Input:         .ausplots_api returns a data frame that has no
#               site_location_name column; non-site_unique path taken.
# Expected:      result is NULL (sort(unique(NULL)) = NULL) — no error thrown.
test_that("list_available_plots: missing site_location_name column returns NULL silently", {
  api_result <- data.frame(latitude = c(-33.5), longitude = c(151.2))
  local_mocked_bindings(
    .ausplots_api = function(path, query) api_result,
    .package = "ausplotsR"
  )
  result <- ausplotsR:::list_available_plots(Plot_IDs = c("SATFLB0004"))
  expect_null(result)
})

# ─── #21 ─ list_available_plots: site_unique IDs, API response missing
#           site_location_visit_id column ──────────────────────────────────────
# Why no prior test: same as #20 — silent wrong output. paste0() with NULL
#   drops the NULL argument, producing a mangled "name-" string rather than
#   an error. (Table entry said character(0); actual behavior is a length-1
#   string with the visit-id part absent.)
# Input:         .ausplots_api returns a data frame with site_location_name but
#               no site_location_visit_id; site_unique path taken.
# Expected:      result is a length-1 character vector ending with "-" —
#               no error thrown.
test_that("list_available_plots: site_unique IDs with missing visit_id column returns mangled string", {
  api_result <- data.frame(site_location_name = c("SATFLB0004"), stringsAsFactors = FALSE)
  local_mocked_bindings(
    .ausplots_api = function(path, query) api_result,
    .package = "ausplotsR"
  )
  result <- ausplotsR:::list_available_plots(Plot_IDs = c("SATFLB0004-11111"))
  expect_type(result, "character")
  expect_length(result, 1)
  expect_match(result, "-$")
})

# ─── #22 ─ extract_site_info: API response missing site_location_name ─────────
# Why no prior test: the table described this as "silent wrong output", but the
#   actual behavior depends on row count. With a 0-row data frame (column absent
#   from the schema), mapply over NULL returns list(), which is silently assigned
#   as the state column — wrong type, no error. This test documents that path.
#   (With a non-empty data frame the assignment throws; see below.)
# Input:         .ausplots_api returns a 0-row data frame with no
#               site_location_name column.
# Expected:      extract_site_info returns normally; the state column is list()
#               (wrong type — silent wrong output).
test_that("extract_site_info: 0-row response missing site_location_name sets state to empty list", {
  api_result <- data.frame(other_col = character(0), stringsAsFactors = FALSE)
  local_mocked_bindings(
    .ausplots_api = function(path, query) api_result,
    .package = "ausplotsR"
  )
  result <- ausplotsR:::extract_site_info(c("SATFLB0004"))
  expect_true("state" %in% names(result))
  expect_length(result$state, 0)
})

# What it tests: with a non-empty data frame and no site_location_name column,
#   mapply returns list() (0 elements) but the data frame has rows, so assigning
#   it throws — this path is NOT a silent wrong output.
# Input:         .ausplots_api returns a 1-row data frame with no
#               site_location_name column.
# Expected:      an error is thrown during column assignment.
test_that("extract_site_info: non-empty response missing site_location_name throws on assignment", {
  api_result <- data.frame(other_col = c("val1"), stringsAsFactors = FALSE)
  local_mocked_bindings(
    .ausplots_api = function(path, query) api_result,
    .package = "ausplotsR"
  )
  expect_error(ausplotsR:::extract_site_info(c("SATFLB0004")))
})

# ─── #23 ─ .get_metadata_dictionary: .ausplots_api failure propagates ─────────
# Why no prior test: treated as a trivially thin caching wrapper — error
#   propagation was assumed obvious and implicitly covered by the .ausplots_api
#   error tests. No direct test was written.
# Input:         force_refresh = TRUE (bypasses cache); .ausplots_api is mocked
#               to throw.
# Expected:      the error propagates out of .get_metadata_dictionary.
test_that(".get_metadata_dictionary: propagates .ausplots_api failure", {
  cache_env <- get("cache", envir = asNamespace("ausplotsR"))
  cache_env$metadata_dictionary <- NULL
  local_mocked_bindings(
    .ausplots_api = function(path, query) stop("API connection failed"),
    .package = "ausplotsR"
  )
  expect_error(
    ausplotsR:::.get_metadata_dictionary(force_refresh = TRUE),
    "API connection failed"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# .ausplots_api() — tryCatch error handler: known vs unknown messages
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: known error "Data extraction aborted..." passes through unchanged.
# Input:         try_GET returns 200; httr::content("text") returns broken JSON.
# Expected:      exact message "Data extraction aborted due to database connection issue."
test_that(".ausplots_api: known message 'Data extraction aborted' passes through unchanged", {
  fake_resp <- structure(
    list(
      status_code = 200,
      headers = list("content-type" = "application/json"),
      url = "http://fake",
      content = charToRaw("BROKEN{{{JSON")
    ),
    class = "response"
  )
  local_mocked_bindings(
    try_GET = function(...) fake_resp,
    .package = "ausplotsR"
  )
  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::.ausplots_api("/site", list()),
        "^Data extraction aborted due to database connection issue\\.$"
      )
    },
    stop_for_status = function(resp, ...) invisible(resp),
    http_type       = function(resp) "application/json",
    content         = function(resp, as = "text", ...) "BROKEN{{{JSON",
    .package = "httr"
  )
})

# What it tests: known message "API did not return json" passes through unchanged.
# Input:         try_GET returns 200 with content-type "text/html".
# Expected:      exact message "API did not return json"
test_that(".ausplots_api: known message 'API did not return json' passes through unchanged", {
  fake_resp <- structure(
    list(
      status_code = 200,
      headers = list("content-type" = "text/html"),
      url = "http://fake",
      content = charToRaw("<html></html>")
    ),
    class = "response"
  )
  local_mocked_bindings(
    try_GET = function(...) fake_resp,
    .package = "ausplotsR"
  )
  with_mocked_bindings(
    {
      expect_error(
        ausplotsR:::.ausplots_api("/site", list()),
        "^API did not return json$"
      )
    },
    stop_for_status = function(resp, ...) invisible(resp),
    http_type       = function(resp) "text/html",
    .package = "httr"
  )
})

# What it tests: unknown error is wrapped with path context.
#   Any error NOT in known_msgs gets prefix "Unexpected error while calling API path '<path>': <msg>".
# Input:         try_GET throws an arbitrary unknown error; path = "/site".
# Expected:      error message contains both path and original message.
test_that(".ausplots_api: unknown error is wrapped with path context", {
  local_mocked_bindings(
    try_GET = function(...) stop("some unexpected low-level failure"),
    .package = "ausplotsR"
  )
  expect_error(
    ausplotsR:::.ausplots_api("/site", list()),
    "Unexpected error while calling API path '/site': some unexpected low-level failure"
  )
})

# What it tests: the path in the wrapped message reflects the actual path argument.
# Input:         try_GET throws; path = "veg_pi".
# Expected:      error message contains "veg_pi".
test_that(".ausplots_api: wrapped error message includes the correct path", {
  local_mocked_bindings(
    try_GET = function(...) stop("network timeout"),
    .package = "ausplotsR"
  )
  expect_error(
    ausplotsR:::.ausplots_api("veg_pi", list()),
    "Unexpected error while calling API path 'veg_pi': network timeout"
  )
})