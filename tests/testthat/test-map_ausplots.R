library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# map_ausplots()
#
# Renders a ggplot2 map of AusPlots site locations coloured by bioregion.
# Has one explicit stop() for missing $site.info. All other errors are caught
# by the outer tryCatch. No API calls — takes an ausplots object directly.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# map_ausplots() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: missing site.info is caught and wrapped
# Input:         ausplots object without $site.info
# Expected:      stop() with "Failed to generate ausplots map:" prefix
test_that("map_ausplots: missing site.info is caught and wrapped", {
  expect_error(
    map_ausplots(list()),
    "Failed to generate ausplots map:"
  )
})

# What it tests: original error message survives inside the wrapped message
# Input:         ausplots object without $site.info
# Expected:      error contains the original explicit stop() message
test_that("map_ausplots: wrapped error preserves original cause", {
  expect_error(
    map_ausplots(list()),
    "must contain \\$site\\.info"
  )
})

# What it tests: NULL input is caught and wrapped
# Input:         NULL (causes $ indexing on NULL)
# Expected:      stop() with "Failed to generate ausplots map:" prefix
test_that("map_ausplots: NULL input is caught and wrapped", {
  expect_error(map_ausplots(NULL), "Failed to generate ausplots map:")
})

# What it tests: site.info missing required columns is caught and wrapped
# Input:         ausplots object with empty site.info (no longitude/latitude/bioregion_name)
# Expected:      stop() with "Failed to generate ausplots map:" prefix
test_that("map_ausplots: site.info missing required columns is caught and wrapped", {
  obj <- list(site.info = data.frame(x = 1))
  expect_error(map_ausplots(obj), "Failed to generate ausplots map:")
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid, .sentry_capture_exception is called
# Input:         "not a list"; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("map_ausplots: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- map_ausplots
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn("not a list"))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid ausplots object with site.info; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("map_ausplots: capture_message 'info' on successful map", {
  skip_if_not_installed("sf")
  captured_levels <- character(0)
  fn <- map_ausplots
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  fn(make_ausplots_from_fixtures())
  expect_true("info" %in% captured_levels)
})
