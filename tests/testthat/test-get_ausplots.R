library(testthat)
library(mockery)
library(jsonlite)

# ══════════════════════════════════════════════════════════════════════════════
# INPUT VALIDATION: my.Plot_IDs
#
# get_ausplots() accepts plot IDs as a character vector. These tests verify
# that wrong types or logically inconsistent inputs are rejected immediately,
# before any network call is made.
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: passing a numeric value instead of a character vector
# Input:         my.Plot_IDs = 123  (integer, not character)
# Expected:      error message "my.Plot_IDs must be provided as a character vector"
test_that("my.Plot_IDs must be a character vector", {
  expect_error(
    get_ausplots(my.Plot_IDs = 123),
    "my.Plot_IDs must be provided as a character vector"
  )
})

# What it tests: mixing two different ID formats in the same call is not allowed.
#   - "SATFLB0001"       is a site_location_name  (no hyphen)
#   - "SATFLB0001-56789" is a site_unique          (hyphenated name + visit ID)
# Input:         my.Plot_IDs = c("SATFLB0001", "SATFLB0001-56789")
# Expected:      error message telling user to use one format only
test_that("my.Plot_IDs cannot mix site names and hyphenated site_unique IDs", {
  expect_error(
    get_ausplots(my.Plot_IDs = c("SATFLB0001", "SATFLB0001-56789")),
    "must list either site_location_name OR site_unique"
  )
})

# What it tests: you cannot use both my.Plot_IDs and plot_search at the same time —
#   they are alternative ways to filter plots, not complementary
# Input:         my.Plot_IDs = "SATFLB0001", plot_search = "satflb"
# Expected:      error asking user to choose one or the other
test_that("cannot supply both my.Plot_IDs and plot_search", {
  expect_error(
    get_ausplots(my.Plot_IDs = "SATFLB0001", plot_search = "satflb"),
    "Please provide EITHER my.Plot_IDs OR plot_search"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# INPUT VALIDATION: plot_search
#
# plot_search is a partial/wildcard text filter on site names.
# It must be exactly one character string.
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: plot_search must be a single string, not a vector or non-character
# Input 1:       plot_search = c("abc", "def")  — vector of two strings
# Input 2:       plot_search = 42               — numeric, not character
# Expected:      error message for both cases
test_that("plot_search must be a single character string", {
  expect_error(
    get_ausplots(plot_search = c("abc", "def")),
    "plot_search must be a single element character vector"
  )
  expect_error(
    get_ausplots(plot_search = 42),
    "plot_search must be a single element character vector"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# INPUT VALIDATION: species/family search filters
#
# Three optional search filters exist: herbarium_determination_search,
# family_search, and standardised_name_search. Each must be a single string,
# and only one of the three may be used per call.
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: herbarium_determination_search must be a single string
# Input 1:       vector of two genus names  — only one allowed
# Input 2:       numeric 123               — wrong type
# Expected:      error message for both cases
test_that("herbarium_determination_search must be a single element character vector", {
  expect_error(
    get_ausplots(herbarium_determination_search = c("Eucalyptus", "Acacia")),
    "herbarium_determination_search must be a single element character vector"
  )
  expect_error(
    get_ausplots(herbarium_determination_search = 123),
    "herbarium_determination_search must be a single element character vector"
  )
})

# What it tests: family_search must be a single string
# Input 1:       vector of two family names — only one allowed
# Input 2:       logical TRUE              — wrong type
# Expected:      error message for both cases
test_that("family_search must be a single element character vector", {
  expect_error(
    get_ausplots(family_search = c("Myrtaceae", "Fabaceae")),
    "family_search must be a single element character vector"
  )
  expect_error(
    get_ausplots(family_search = TRUE),
    "family_search must be a single element character vector"
  )
})

# What it tests: standardised_name_search must be a single string
# Input 1:       vector of two species names — only one allowed
# Input 2:       numeric 42                  — wrong type
# Expected:      error message for both cases
test_that("standardised_name_search must be a single element character vector", {
  expect_error(
    get_ausplots(standardised_name_search = c("Eucalyptus camaldulensis", "Acacia aneura")),
    "standardised_name_search must be a single element character vector"
  )
  expect_error(
    get_ausplots(standardised_name_search = 42),
    "standardised_name_search must be a single element character vector"
  )
})

# What it tests: only one of the three search filters may be used per call
#   Combo 1: family_search + herbarium_determination_search
#   Combo 2: family_search + standardised_name_search
#   Combo 3: herbarium_determination_search + standardised_name_search
# Input:    two search filters supplied simultaneously for each combo
# Expected: error message "you can specify one of either" for all three combos
test_that("cannot supply two search filters at once", {
  expect_error(
    get_ausplots(family_search = "Myrtaceae", herbarium_determination_search = "Eucalyptus"),
    "you can specify one of either"
  )
  expect_error(
    get_ausplots(family_search = "Myrtaceae", standardised_name_search = "Eucalyptus camaldulensis"),
    "you can specify one of either"
  )
  expect_error(
    get_ausplots(herbarium_determination_search = "Eucalyptus",
                 standardised_name_search = "Eucalyptus camaldulensis"),
    "you can specify one of either"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# HAPPY PATH: valid inputs must not trigger validation errors
#
# These tests confirm that well-formed inputs pass validation cleanly.
# Internet is mocked as unavailable so the function exits early with NULL
# rather than making a real network call — we only care that no error is thrown.
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: default call with no arguments is always valid
# Input:         no arguments (all defaults)
# Expected:      no error thrown; returns NULL because internet is mocked off
test_that("default call does not throw input validation error (no internet)", {
  mockery::stub(get_ausplots, "curl::has_internet", FALSE)
  expect_no_error(get_ausplots())
})

# What it tests: a single valid string is an acceptable plot_search value
# Input:         plot_search = "satflb"  (partial site name)
# Expected:      no error thrown
test_that("valid single-string plot_search does not throw input validation error", {
  mockery::stub(get_ausplots, "curl::has_internet", FALSE)
  expect_no_error(get_ausplots(plot_search = "satflb"))
})

# What it tests: a single genus/species string is a valid herbarium_determination_search
# Input:         herbarium_determination_search = "Eucalyptus"
# Expected:      no error thrown
test_that("valid single herbarium_determination_search does not error on input validation", {
  mockery::stub(get_ausplots, "curl::has_internet", FALSE)
  expect_no_error(get_ausplots(herbarium_determination_search = "Eucalyptus"))
})

# What it tests: a single family name string is a valid family_search value
# Input:         family_search = "Myrtaceae"
# Expected:      no error thrown
test_that("valid single family_search does not error on input validation", {
  mockery::stub(get_ausplots, "curl::has_internet", FALSE)
  expect_no_error(get_ausplots(family_search = "Myrtaceae"))
})

# What it tests: a single APC-matched species name is a valid standardised_name_search
# Input:         standardised_name_search = "Eucalyptus camaldulensis"
# Expected:      no error thrown
test_that("valid single standardised_name_search does not error on input validation", {
  mockery::stub(get_ausplots, "curl::has_internet", FALSE)
  expect_no_error(get_ausplots(standardised_name_search = "Eucalyptus camaldulensis"))
})

# What it tests: the default value "none" for bounding_box is always valid
# Input:         bounding_box = "none"  (the default)
# Expected:      no error thrown
test_that("bounding_box='none' default is accepted without error", {
  mockery::stub(get_ausplots, "curl::has_internet", FALSE)
  expect_no_error(get_ausplots(bounding_box = "none"))
})

# ══════════════════════════════════════════════════════════════════════════════
# OFFLINE BEHAVIOUR
#
# Per CRAN policy, the package must fail gracefully when the internet or API
# is unavailable — returning NULL with a message, never throwing an error.
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: when curl::has_internet() returns FALSE the function returns
#   NULL silently rather than crashing
# Input:         no arguments; curl::has_internet mocked to return FALSE
# Expected:      return value is NULL
test_that("returns NULL invisibly when no internet is available", {
  mockery::stub(get_ausplots, "curl::has_internet", FALSE)
  result <- get_ausplots()
  expect_null(result)
})

# What it tests: when no internet is available, a human-readable message is
#   emitted — not an error or warning — so calling code is not broken
# Input:         no arguments; curl::has_internet mocked to return FALSE
# Expected:      a message containing "No internet connection" is emitted
test_that("emits an informative message (not an error) when no internet", {
  mockery::stub(get_ausplots, "curl::has_internet", FALSE)
  expect_message(get_ausplots(), "No internet connection")
})

# What it tests: when the API server is reachable but the GET response is not
#   an httr response object (e.g. timeout or DNS failure returns a string),
#   the function returns NULL gracefully instead of crashing
# Input:         curl::has_internet mocked TRUE; httr::GET mocked to return a
#                plain string "Could not connect to server" (not an httr response)
# Expected:      return value is NULL
test_that("returns NULL when API URL is unreachable", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", "Could not connect to server")
  result <- get_ausplots()
  expect_null(result)
})

# ══════════════════════════════════════════════════════════════════════════════
# MOCKED INTEGRATION TEST HELPERS
#
# These helpers are used by all mocked integration tests below.
# mock_api() returns fixture data for any .ausplots_api path.
# make_ok_response() returns a minimal httr response that passes the
#   inherits(resp, "response") and !httr::http_error(resp) checks in get_ausplots().
# with_mocks() wraps a test body with the three stubs needed to bypass the
#   internet check, the try_GET reachability check, and the API calls.
# ══════════════════════════════════════════════════════════════════════════════

fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name), simplifyDataFrame = TRUE)
}

mock_api <- function(path, query) {
  path_to_fixture <- list(
    "search"                  = "search.json",
    "site"                    = "site.json",
    "structural_summary"      = "structural_summary.json",
    "soil_subsite"            = "soil_subsite.json",
    "soil_bulk_density"       = "soil_bulk_density.json",
    "soil_characterisation"   = "soil_characterisation.json",
    "veg_basal"               = "veg_basal.json",
    "veg_voucher"             = "veg_voucher.json",
    "veg_pi"                  = "veg_pi.json",
    "metadata-dictionary.json" = "metadata_dictionary.json"
  )
  fname <- path_to_fixture[[path]]
  if (is.null(fname)) return(data.frame())
  fixture(fname)
}

make_ok_response <- function() {
  structure(
    list(
      url         = "http://swarmapi.ausplots.aekos.org.au:80",
      status_code = 200L,
      headers     = structure(
        list(`content-type` = "application/json"),
        class = c("insensitive", "list")
      ),
      content = raw(0)
    ),
    class = "response"
  )
}

with_mocks <- function(expr) {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(
    .ausplots_api = mock_api,
    .package = "ausplotsR",
    {
      force(expr)
    }
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# MOCKED INTEGRATION TESTS — core return structure
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: the return value of get_ausplots() is always a list
# Input:         my.Plot_IDs = "SATFLB0004"
# Expected:      result is of type "list"
test_that("result is a list", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004")
  expect_type(result, "list")
})

# What it tests: every result includes a $citation string crediting ausplotsR
#   The citation is auto-generated with the current year and access date.
# Input:         my.Plot_IDs = "SATFLB0004"
# Expected:      result$citation exists, is a character string, contains "ausplotsR"
test_that("result always contains a $citation string mentioning ausplotsR", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004")
  expect_true("citation" %in% names(result))
  expect_type(result$citation, "character")
  expect_match(result$citation, "ausplotsR")
})

# What it tests: $citation encodes the year of extraction so data provenance
#   can be traced; the year is formatted using Sys.Date() inside the function
# Input:         my.Plot_IDs = "SATFLB0004"
# Expected:      result$citation contains the current calendar year as a string
test_that("$citation contains the current year", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004")
  expect_match(result$citation, as.character(format(Sys.Date(), "%Y")))
})

# ══════════════════════════════════════════════════════════════════════════════
# MOCKED INTEGRATION TESTS — site_info module
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: site_info=TRUE (the default) populates the $site.info element
# Input:         my.Plot_IDs = "SATFLB0004", site_info = TRUE
# Expected:      result$site.info exists and is a data.frame
test_that("site_info=TRUE (default) returns $site.info as a data frame", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", site_info = TRUE)
  expect_true("site.info" %in% names(result))
  expect_s3_class(result$site.info, "data.frame")
})

# What it tests: setting site_info=FALSE completely omits $site.info from the result
# Input:         my.Plot_IDs = "SATFLB0004", site_info = FALSE
# Expected:      "site.info" is NOT a key in the returned list
test_that("site_info=FALSE returns result without $site.info", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", site_info = FALSE)
  expect_false("site.info" %in% names(result))
})

# What it tests: $site.info contains all the core columns downstream functions depend on
# Input:         my.Plot_IDs = "SATFLB0004"
# Expected:      all of site_unique, site_location_name, latitude, longitude,
#                visit_date, visit_number are present as column names
test_that("$site.info contains expected columns", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004")
  expected_cols <- c("site_unique", "site_location_name", "latitude", "longitude",
                     "visit_date", "visit_number")
  expect_true(all(expected_cols %in% names(result$site.info)))
})

# What it tests: site_unique is correctly formatted as "site_location_name-visit_id"
#   e.g. "SATFLB0004-12345"; downstream functions rely on the hyphen separator
# Input:         my.Plot_IDs = "SATFLB0004"
# Expected:      every value in site_unique contains a hyphen
test_that("site_unique column is formatted as 'name-visit_id'", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004")
  expect_true(all(grepl("-", result$site.info$site_unique)))
})

# What it tests: visit_date is converted from raw ISO string "2019-03-15T00:00:00"
#   to a date-like value inside the function — this is needed for date arithmetic
#   and chronological ordering of revisits
# Input:         my.Plot_IDs = "SATFLB0004"
# Expected:      result$site.info$visit_date contains parseable dates (year > 2000)
test_that("$site.info$visit_date is of class Date", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004")
  visit_dates <- as.Date(result$site.info$visit_date, origin = "1970-01-01")
  expect_true(all(!is.na(visit_dates)))
  expect_true(all(as.numeric(format(visit_dates, "%Y")) > 2000))
})

# What it tests: visit_number is built from scratch by ranking visits chronologically;
#   every plot must have a visit number of at least 1 (first visit = 1)
# Input:         my.Plot_IDs = "SATFLB0004"
# Expected:      visit_number column values are all >= 1 when coerced to numeric
test_that("$site.info$visit_number is numeric and >= 1", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004")
  visit_nums <- as.numeric(result$site.info$visit_number)
  expect_true(all(!is.na(visit_nums)))
  expect_true(all(visit_nums >= 1))
})

# What it tests: site.json fixture has 2 rows for SATFLB0004 (two visits);
#   the function returns both, not just one
# Input:         my.Plot_IDs = "SATFLB0004"  — site.json contains visit IDs 58658 and 53705
# Expected:      result$site.info has 2 rows; both visit IDs appear in site_unique
test_that("both visits in fixture are returned in site.info", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004")
  expect_equal(nrow(result$site.info), 2L)
  expect_true(any(grepl("58658", result$site.info$site_unique)))
  expect_true(any(grepl("53705", result$site.info$site_unique)))
})

# ══════════════════════════════════════════════════════════════════════════════
# MOCKED INTEGRATION TESTS — veg.PI module
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: veg.PI=TRUE populates $veg.PI with point-intercept data and
#   all the columns required by functions like species_table() and fractional_cover()
# Input:         my.Plot_IDs = "SATFLB0004", veg.PI = TRUE, site_info = FALSE
# Expected:      result$veg.PI exists and contains site_unique, hits_unique,
#                growth_form, height, in_canopy_sky columns
test_that("veg.PI=TRUE returns $veg.PI with expected columns", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", veg.PI = TRUE, site_info = FALSE)
  expect_true("veg.PI" %in% names(result))
  expected_cols <- c("site_unique", "hits_unique", "growth_form", "height", "in_canopy_sky")
  expect_true(all(expected_cols %in% names(result$veg.PI)))
})

# What it tests: herbarium_determination in $veg.PI is cleaned to title case
#   (e.g. "eucalyptus camaldulensis" → "Eucalyptus camaldulensis") by the
#   capitalize() + tolower() pipeline in the source code
# Input:         my.Plot_IDs = "SATFLB0004", veg.PI = TRUE
# Expected:      all non-NA herbarium_determination values match their title-cased form
test_that("$veg.PI$herbarium_determination values are title-cased", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", veg.PI = TRUE, site_info = FALSE)
  non_na <- na.omit(result$veg.PI$herbarium_determination)
  expect_true(all(non_na == R.utils::capitalize(tolower(non_na))))
})

# ══════════════════════════════════════════════════════════════════════════════
# MOCKED INTEGRATION TESTS — remaining data modules
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: structural_summaries=TRUE populates $struct.summ with
#   vegetation structural summary data (canopy height, cover class etc.)
# Input:         my.Plot_IDs = "SATFLB0004", structural_summaries = TRUE,
#                site_info = FALSE
# Expected:      result$struct.summ exists and is a data.frame
test_that("structural_summaries=TRUE returns $struct.summ as a data frame", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", structural_summaries = TRUE,
                         site_info = FALSE)
  expect_true("struct.summ" %in% names(result))
  expect_s3_class(result$struct.summ, "data.frame")
})

# What it tests: veg.vouchers=TRUE populates $veg.vouch with herbarium voucher
#   records including species determinations and silica-dried tissue sample IDs.
# Input:         my.Plot_IDs = "SATFLB0004", veg.vouchers = TRUE, site_info = FALSE
# Expected:      result$veg.vouch exists and contains site_unique,
#                herbarium_determination, standardised_name columns
test_that("veg.vouchers=TRUE returns $veg.vouch with expected columns", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", veg.vouchers = TRUE, site_info = FALSE)
  expect_true("veg.vouch" %in% names(result))
  expected_cols <- c("site_unique", "herbarium_determination", "standardised_name")
  expect_true(all(expected_cols %in% names(result$veg.vouch)))
})

# What it tests: basal.wedge=TRUE populates $veg.basal with raw basal wedge hit
#   data needed by the basal_area() function to calculate tree basal area.
# Input:         my.Plot_IDs = "SATFLB0004", basal.wedge = TRUE, site_info = FALSE
# Expected:      result$veg.basal exists and is a data.frame
test_that("basal.wedge=TRUE returns $veg.basal as a data frame", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", basal.wedge = TRUE, site_info = FALSE)
  expect_true("veg.basal" %in% names(result))
  expect_s3_class(result$veg.basal, "data.frame")
})

# What it tests: soil_subsites=TRUE populates $soil.subsites with details of
#   soil sampling locations and their barcode IDs across the 9 plot subsites
# Input:         my.Plot_IDs = "SATFLB0004", soil_subsites = TRUE, site_info = FALSE
# Expected:      result$soil.subsites exists and is a data.frame
test_that("soil_subsites=TRUE returns $soil.subsites as a data frame", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", soil_subsites = TRUE,
                         site_info = FALSE)
  expect_true("soil.subsites" %in% names(result))
  expect_s3_class(result$soil.subsites, "data.frame")
})

# What it tests: soil_bulk_density=TRUE populates $soil.bulk with raw bulk
#   density measurements taken at the plot
# Input:         my.Plot_IDs = "SATFLB0004", soil_bulk_density = TRUE,
#                site_info = FALSE
# Expected:      result$soil.bulk exists and is a data.frame
test_that("soil_bulk_density=TRUE returns $soil.bulk as a data frame", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", soil_bulk_density = TRUE,
                         site_info = FALSE)
  expect_true("soil.bulk" %in% names(result))
  expect_s3_class(result$soil.bulk, "data.frame")
})

# What it tests: soil_character=TRUE populates $soil.char with soil
#   characterisation data recorded at 10 cm depth increments to 1 m
# Input:         my.Plot_IDs = "SATFLB0004", soil_character = TRUE,
#                site_info = FALSE
# Expected:      result$soil.char exists and is a data.frame
test_that("soil_character=TRUE returns $soil.char as a data frame", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", soil_character = TRUE,
                         site_info = FALSE)
  expect_true("soil.char" %in% names(result))
  expect_s3_class(result$soil.char, "data.frame")
})

# ══════════════════════════════════════════════════════════════════════════════
# MOCKED INTEGRATION TESTS — dictionary module
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: dictionary=TRUE populates $metadata.dictionary with a variable
#   reference table describing fields across all data frames.
# Input:         my.Plot_IDs = "SATFLB0004", dictionary = TRUE
# Expected:      result$metadata.dictionary exists and is a data.frame
test_that("dictionary=TRUE returns $metadata.dictionary as a data frame", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  result <- get_ausplots(my.Plot_IDs = "SATFLB0004", dictionary = TRUE)
  expect_true("metadata.dictionary" %in% names(result))
  expect_s3_class(result$metadata.dictionary, "data.frame")
})

# ══════════════════════════════════════════════════════════════════════════════
# MOCKED INTEGRATION TESTS — warning and error behaviour for bad Plot_IDs
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: supplying a Plot_ID that does not exist in the search fixture
#   raises a stop() error — search returns SATFLB0004 only, so XXXXXX9999 is unmatched
# Input:         my.Plot_IDs = "XXXXXX9999"
# Expected:      error containing "None of the user-supplied Plot_IDs match"
test_that("entirely non-existent Plot_IDs raises an error", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  expect_error(
    get_ausplots(my.Plot_IDs = "XXXXXX9999"),
    "None of the user-supplied Plot_IDs match available plot names"
  )
})

# What it tests: a mix of one valid and one invalid Plot_ID results in a warning
#   and the function continues using only the matching plot
# Input:         my.Plot_IDs = c("SATFLB0004", "XXXXXX9999")
# Expected:      warning "Not all user-supplied Plot_IDs match";
#                result$site.info contains only SATFLB0004
test_that("partially invalid Plot_IDs triggers a warning and uses only matching plots", {
  mockery::stub(get_ausplots, "curl::has_internet", TRUE)
  mockery::stub(get_ausplots, "httr::GET", function(...) make_ok_response(), depth = 2)
  local_mocked_bindings(.ausplots_api = mock_api, .package = "ausplotsR")
  expect_warning(
    result <- get_ausplots(my.Plot_IDs = c("SATFLB0004", "XXXXXX9999")),
    "Not all user-supplied Plot_IDs match"
  )
  expect_true("SATFLB0004" %in% result$site.info$site_location_name)
})