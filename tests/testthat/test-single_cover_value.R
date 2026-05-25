library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# single_cover_value()
#
# Calculates percent cover per site from point-intercept data. Has several
# explicit stop() calls for bad inputs. No API calls — takes veg.PI directly.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — minimal valid veg.PI data frame
# ══════════════════════════════════════════════════════════════════════════════

make_veg_pi <- function() {
  data.frame(
    site_unique  = c("SITE001-1", "SITE001-1", "SITE001-1"),
    hits_unique  = c("H1", "H2", "H3"),
    growth_form  = c("Tree/Palm", "Shrub", "Tree/Palm"),
    height       = c(10, 2, 8),
    in_canopy_sky = c(FALSE, FALSE, FALSE),
    dead         = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# single_cover_value() — OUTPUT / HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: valid veg.PI returns a data frame with site_unique and percentCover
# Input:         minimal valid veg.PI
# Expected:      no error; data frame with expected columns
test_that("single_cover_value: valid input returns cover data frame without error", {
  result <- expect_no_error(single_cover_value(make_veg_pi()))
  expect_s3_class(result, "data.frame")
  expect_true("site_unique" %in% names(result))
  expect_true("percentCover" %in% names(result))
})

# ══════════════════════════════════════════════════════════════════════════════
# single_cover_value() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: non-data.frame input is caught and wrapped
# Input:         a list instead of a data.frame
# Expected:      stop() with "Failed to calculate cover value:" prefix
test_that("single_cover_value: non-data.frame input is caught and wrapped", {
  expect_error(single_cover_value(list(a = 1)), "Failed to calculate cover value:")
})

# What it tests: original error message survives inside the wrapped message
# Input:         non-data.frame
# Expected:      error contains the original stop() cause
test_that("single_cover_value: wrapped error preserves original cause", {
  expect_error(single_cover_value(list(a = 1)), "veg.PI must be a data.frame")
})

# What it tests: missing required columns is caught and wrapped
# Input:         data frame lacking growth_form, site_unique, height
# Expected:      stop() with "Failed to calculate cover value:" prefix
test_that("single_cover_value: missing required columns is caught and wrapped", {
  df <- data.frame(x = 1)
  expect_error(single_cover_value(df), "Failed to calculate cover value:")
})

# What it tests: NULL input is caught and wrapped
# Input:         NULL
# Expected:      stop() with "Failed to calculate cover value:" prefix
test_that("single_cover_value: NULL input is caught and wrapped", {
  expect_error(single_cover_value(NULL), "Failed to calculate cover value:")
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid, .sentry_capture_exception is called
# Input:         NULL; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("single_cover_value: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- single_cover_value
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn(NULL))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid veg.PI data frame; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("single_cover_value: capture_message 'info' on successful calculation", {
  captured_levels <- character(0)
  fn <- single_cover_value
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  ap <- make_ausplots_from_fixtures()
  fn(ap$veg.PI, my.growth_forms = c("Hummock grass", "Shrub"))
  expect_true("info" %in% captured_levels)
})
