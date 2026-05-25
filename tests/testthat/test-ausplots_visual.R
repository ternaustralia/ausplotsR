library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# ausplots_visual()
#
# Generates a multi-panel visualisation (maps, pie charts, cumulative cover,
# Whittaker plots) for an ausplots object. Writes output to a PDF.
# Individual sub-plots are wrapped in try() internally; the outer tryCatch
# catches errors from setup, data-extraction, and PDF init steps.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# ausplots_visual() — ERROR HANDLING (tryCatch wraps entire body)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: all output flags FALSE is caught and wrapped
# Input:         any ausplots object; all output flags set to FALSE
# Expected:      error message contains "Failed to generate ausplots visualisations"
#                and "You have not requested any outputs"
test_that("ausplots_visual: all outputs FALSE is caught and wrapped", {
  expect_error(
    ausplots_visual(
      my.ausplots.object = list(site.info = data.frame(), veg.PI = data.frame()),
      map            = FALSE,
      map.attribute  = FALSE,
      fraction.pie   = FALSE,
      growthform.pie = FALSE,
      cumulative.cover = FALSE,
      whittaker      = FALSE
    ),
    "Failed to generate ausplots visualisations"
  )
  expect_error(
    ausplots_visual(
      my.ausplots.object = list(site.info = data.frame(), veg.PI = data.frame()),
      map            = FALSE,
      map.attribute  = FALSE,
      fraction.pie   = FALSE,
      growthform.pie = FALSE,
      cumulative.cover = FALSE,
      whittaker      = FALSE
    ),
    "You have not requested any outputs"
  )
})

# What it tests: object missing veg.PI when veg plots requested is caught and wrapped
# Input:         my.ausplots.object without $veg.PI; default flags (fraction.pie = TRUE etc.)
# Expected:      error contains "Failed to generate ausplots visualisations"
#                and "must contain $veg.PI data"
test_that("ausplots_visual: missing veg.PI is caught and wrapped", {
  obj <- list(site.info = data.frame(x = 1))
  expect_error(
    ausplots_visual(my.ausplots.object = obj),
    "Failed to generate ausplots visualisations"
  )
  expect_error(
    ausplots_visual(my.ausplots.object = obj),
    "must contain \\$veg\\.PI data"
  )
})

# What it tests: object missing site.info when map requested is caught and wrapped
# Input:         my.ausplots.object without $site.info; map = TRUE; veg flags FALSE
# Expected:      error contains "Failed to generate ausplots visualisations"
#                and "must contain $site.info data"
test_that("ausplots_visual: missing site.info is caught and wrapped", {
  obj <- list(veg.PI = data.frame(site_location_name = "SITE_A"))
  expect_error(
    ausplots_visual(
      my.ausplots.object = obj,
      map            = TRUE,
      map.attribute  = FALSE,
      fraction.pie   = FALSE,
      growthform.pie = FALSE,
      cumulative.cover = FALSE,
      whittaker      = FALSE
    ),
    "Failed to generate ausplots visualisations"
  )
  expect_error(
    ausplots_visual(
      my.ausplots.object = obj,
      map            = TRUE,
      map.attribute  = FALSE,
      fraction.pie   = FALSE,
      growthform.pie = FALSE,
      cumulative.cover = FALSE,
      whittaker      = FALSE
    ),
    "must contain \\$site\\.info data"
  )
})

# What it tests: errors from get_ausplots() when no object is provided are wrapped
# Input:         no my.ausplots.object; get_ausplots mocked to throw
# Expected:      error contains "Failed to generate ausplots visualisations"
test_that("ausplots_visual: get_ausplots failure is caught and wrapped", {
  mockery::stub(
    ausplots_visual, "get_ausplots",
    function(...) stop("network failure")
  )
  expect_error(
    ausplots_visual(),
    "Failed to generate ausplots visualisations"
  )
  expect_error(
    ausplots_visual(),
    "network failure"
  )
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid, .sentry_capture_exception is called
# Input:         all outputs FALSE; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("ausplots_visual: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- ausplots_visual
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  ap <- make_ausplots_from_fixtures()
  expect_error(fn(
    my.ausplots.object = ap,
    map = FALSE, map.attribute = FALSE, fraction.pie = FALSE,
    growthform.pie = FALSE, cumulative.cover = FALSE, whittaker = FALSE
  ))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid ausplots object; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("ausplots_visual: capture_message 'info' on successful visualisation", {
  skip_if_not_installed("sf")
  captured_levels <- character(0)
  fn <- ausplots_visual
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  ap <- make_ausplots_from_fixtures()
  fn(my.ausplots.object = ap)
  expect_true("info" %in% captured_levels)
})
