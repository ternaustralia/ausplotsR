library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# point_intercept_visual()
#
# Renders spatial point intercept plots for 100x100m plots coloured by
# substrate, growth form, or dominant species. Has several explicit stop()
# calls for missing inputs. All other errors are caught by the outer tryCatch.
# No API calls — takes an ausplots object directly.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# point_intercept_visual() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: missing plots argument is caught and wrapped
# Input:         valid ausplots object; plots not supplied
# Expected:      stop() with "Failed to generate point intercept visualisation:" prefix
test_that("point_intercept_visual: missing plots is caught and wrapped", {
  obj <- list(
    site.info = data.frame(
      site_location_name   = "SITE001",
      plot_is_100m_by_100m = TRUE,
      stringsAsFactors     = FALSE
    ),
    veg.PI = data.frame(
      site_unique          = "SITE001-1",
      site_location_name   = "SITE001",
      stringsAsFactors     = FALSE
    )
  )
  expect_error(
    point_intercept_visual(obj, plots = NULL),
    "Failed to generate point intercept visualisation:"
  )
})

# What it tests: original error message survives inside the wrapped message
# Input:         plots = NULL
# Expected:      error contains the original stop() cause
test_that("point_intercept_visual: wrapped error preserves original cause", {
  obj <- list(
    site.info = data.frame(
      site_location_name   = "SITE001",
      plot_is_100m_by_100m = TRUE,
      stringsAsFactors     = FALSE
    ),
    veg.PI = data.frame(
      site_unique        = "SITE001-1",
      site_location_name = "SITE001",
      stringsAsFactors   = FALSE
    )
  )
  expect_error(
    point_intercept_visual(obj, plots = NULL),
    "plot IDs"
  )
})

# What it tests: missing veg.PI is caught and wrapped
# Input:         ausplots object without $veg.PI
# Expected:      stop() with "Failed to generate point intercept visualisation:" prefix
test_that("point_intercept_visual: missing veg.PI is caught and wrapped", {
  obj <- list(
    site.info = data.frame(
      site_location_name   = "SITE001",
      plot_is_100m_by_100m = TRUE,
      stringsAsFactors     = FALSE
    )
  )
  expect_error(
    point_intercept_visual(obj, plots = "SITE001-1"),
    "Failed to generate point intercept visualisation:"
  )
})

# What it tests: missing site.info is caught and wrapped
# Input:         ausplots object without $site.info
# Expected:      stop() with "Failed to generate point intercept visualisation:" prefix
test_that("point_intercept_visual: missing site.info is caught and wrapped", {
  obj <- list(
    veg.PI = data.frame(
      site_unique        = "SITE001-1",
      site_location_name = "SITE001",
      stringsAsFactors   = FALSE
    )
  )
  expect_error(
    point_intercept_visual(obj, plots = "SITE001-1"),
    "Failed to generate point intercept visualisation:"
  )
})

# What it tests: invalid visual value is caught and wrapped
# Input:         valid ausplots object; visual = "bad_visual"
# Expected:      stop() with "Failed to generate point intercept visualisation:" prefix
test_that("point_intercept_visual: invalid visual value is caught and wrapped", {
  obj <- list(
    site.info = data.frame(
      site_location_name   = "SITE001",
      plot_is_100m_by_100m = TRUE,
      stringsAsFactors     = FALSE
    ),
    veg.PI = data.frame(
      site_unique        = "SITE001-1",
      site_location_name = "SITE001",
      stringsAsFactors   = FALSE
    )
  )
  expect_error(
    point_intercept_visual(obj, plots = "SITE001-1", visual = "bad_visual"),
    "Failed to generate point intercept visualisation:"
  )
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid, .sentry_capture_exception is called
# Input:         invalid ausplots object; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("point_intercept_visual: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- point_intercept_visual
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn("not a list"))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid ausplots object; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("point_intercept_visual: capture_message 'info' on successful visualisation", {
  captured_levels <- character(0)
  fn <- point_intercept_visual
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  ap <- make_ausplots_from_fixtures()
  plots <- unique(ap$veg.PI$site_unique)[1]
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  fn(ap, plots = plots)
  expect_true("info" %in% captured_levels)
})
