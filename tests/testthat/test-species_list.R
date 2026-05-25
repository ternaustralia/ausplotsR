library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# species_list()
#
# Builds a species list from veg.vouch data by site, visit, or collapsed.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — build a minimal veg.vouch data frame for testing
# ══════════════════════════════════════════════════════════════════════════════

make_veg_vouch_sl <- function() {
  data.frame(
    site_location_name      = c("SITE001", "SITE001", "SITE002"),
    site_unique             = c("SITE001-1", "SITE001-1", "SITE002-1"),
    herbarium_determination = c("Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura"),
    standardised_name       = c("Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura"),
    genus                   = c("Acacia", "Eucalyptus", "Acacia"),
    genus_species           = c("Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura"),
    family                  = c("Fabaceae", "Myrtaceae", "Fabaceae"),
    stringsAsFactors        = FALSE
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# species_list() — HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: grouping='by_site', species_name='SN' returns a named list
# Input:         minimal veg.vouch with 2 sites and 2 species
# Expected:      named list with one entry per site_location_name
test_that("species_list: grouping='by_site' returns a named list", {
  vv <- make_veg_vouch_sl()
  result <- species_list(vv, grouping = "by_site", species_name = "SN")
  expect_true(is.list(result))
  expect_true("SITE001" %in% names(result))
  expect_true("SITE002" %in% names(result))
})

# What it tests: grouping='by_visit' returns a list keyed on site_unique
# Input:         minimal veg.vouch with 2 visit-level site_unique values
# Expected:      named list with keys SITE001-1 and SITE002-1
test_that("species_list: grouping='by_visit' returns a list keyed on site_unique", {
  vv <- make_veg_vouch_sl()
  result <- species_list(vv, grouping = "by_visit", species_name = "SN")
  expect_true(is.list(result))
  expect_true("SITE001-1" %in% names(result))
})

# What it tests: grouping='collapse' returns a character vector with species names
# Input:         minimal veg.vouch
# Expected:      result has length >= 1; contains "Acacia aneura"
test_that("species_list: grouping='collapse' returns a character vector", {
  vv <- make_veg_vouch_sl()
  result <- species_list(vv, grouping = "collapse", species_name = "SN")
  expect_true(length(result) >= 1L)
  expect_true("Acacia aneura" %in% as.character(result))
})

# What it tests: species_name='HD' uses herbarium_determination column
# Input:         veg.vouch with distinct herbarium_determination values
# Expected:      result for SITE001 includes "Acacia aneura"
test_that("species_list: species_name='HD' uses herbarium_determination", {
  vv <- make_veg_vouch_sl()
  result <- species_list(vv, grouping = "by_site", species_name = "HD")
  expect_true("Acacia aneura" %in% as.character(result[["SITE001"]]))
})

# What it tests: append_family=TRUE appends '--Family' suffix to species names
# Input:         veg.vouch with family column; grouping='collapse'
# Expected:      result entries contain "--Fabaceae" substring
test_that("species_list: append_family=TRUE appends family to species names", {
  vv <- make_veg_vouch_sl()
  result <- species_list(vv, grouping = "collapse", species_name = "SN", append_family = TRUE)
  expect_true(any(grepl("--Fabaceae", as.character(result))))
})

# ══════════════════════════════════════════════════════════════════════════════
# species_list() — SILENT BEHAVIOUR (undetectable wrong output)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: append_family=TRUE with all-NA family column silently appends NA
# Input:         veg.vouch with family = NA_character_; grouping='collapse'
# Expected:      no error is thrown; result entries contain "--NA"
test_that("species_list: append_family=TRUE with NA family silently appends NA", {
  vv <- make_veg_vouch_sl()
  vv$family <- NA_character_
  result <- species_list(vv, grouping = "collapse", species_name = "SN", append_family = TRUE)
  expect_true(any(grepl("--NA", as.character(result))))
})

# ══════════════════════════════════════════════════════════════════════════════
# species_list() — ERROR HANDLING
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: non-data.frame veg.vouch is caught and wrapped
# Input:         veg.vouch = "not a df"
# Expected:      error containing "Failed to build species list:"
test_that("species_list: non-data.frame input throws wrapped error", {
  expect_error(
    species_list("not a df", grouping = "by_site"),
    "Failed to build species list:"
  )
})

# What it tests: wrapped error preserves the original cause
# Input:         veg.vouch = "not a df"
# Expected:      error message also contains "veg.vouch must be a data.frame"
test_that("species_list: wrapped error preserves original cause", {
  expect_error(
    species_list("not a df", grouping = "by_site"),
    "veg.vouch must be a data.frame"
  )
})

# What it tests: write.table failure with invalid path is caught and wrapped
# Input:         valid veg.vouch; writefile=TRUE; outfile="/nonexistent/path/file.txt"
# Expected:      error containing "Failed to build species list:"
test_that("species_list: write.table failure is caught and wrapped", {
  vv <- make_veg_vouch_sl()
  expect_error(
    species_list(vv, grouping = "by_site", writefile = TRUE,
                 outfile = "/nonexistent/path/that/cannot/be/created.txt"),
    "Failed to build species list:"
  )
})

# What it tests: missing required column throws wrapped error
# Input:         veg.vouch missing herbarium_determination column
# Expected:      error containing "Failed to build species list:"
test_that("species_list: missing required column throws wrapped error", {
  vv <- make_veg_vouch_sl()
  vv$herbarium_determination <- NULL
  expect_error(
    species_list(vv, grouping = "by_site"),
    "Failed to build species list:"
  )
})

# What it tests: wrapped error has call.=FALSE
# Input:         veg.vouch = NULL
# Expected:      error is raised; err$call is NULL
test_that("species_list: wrapped error has call.=FALSE", {
  err <- tryCatch(species_list(NULL, grouping = "by_site"), error = function(e) e)
  expect_null(err$call)
})

# ══ SENTRY CAPTURE ══

# What it tests: when veg.vouch is NULL, .sentry_capture_exception is called
# Input:         NULL veg.vouch; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("species_list: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- species_list
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn(NULL, grouping = "by_site"))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid veg.vouch; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("species_list: capture_message 'info' on successful build", {
  captured_levels <- character(0)
  fn <- species_list
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  fn(make_veg_vouch_sl(), grouping = "by_site")
  expect_true("info" %in% captured_levels)
})
