library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# trajectoryPCoA_ausplots() / trajectoryPlot_ausplots()
#
# Trajectory plot functions adapted from ecotraj. Take a distance matrix `d`
# and site/survey vectors. No API calls — take in-memory objects.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# trajectoryPCoA_ausplots() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: NULL distance matrix is caught and wrapped
# Input:         d = NULL
# Expected:      stop() with "Failed to generate trajectory PCoA plot:" prefix
test_that("trajectoryPCoA_ausplots: NULL d is caught and wrapped", {
  expect_error(
    trajectoryPCoA_ausplots(d = NULL, sites = c("A", "A", "B", "B")),
    "Failed to generate trajectory PCoA plot:"
  )
})

# What it tests: mismatched d and sites lengths is caught and wrapped
# Input:         dist object for 2 sites; sites vector length 4
# Expected:      stop() with "Failed to generate trajectory PCoA plot:" prefix
test_that("trajectoryPCoA_ausplots: mismatched d and sites is caught and wrapped", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  d <- dist(matrix(c(1, 0, 0, 1), nrow = 2))
  expect_error(
    trajectoryPCoA_ausplots(d = d, sites = c("A", "A", "B", "B")),
    "Failed to generate trajectory PCoA plot:"
  )
})

# What it tests: original error message survives inside the wrapped message
# Input:         NULL d
# Expected:      error contains an R-level cause (not just the prefix)
test_that("trajectoryPCoA_ausplots: wrapped error preserves original cause", {
  err <- tryCatch(
    trajectoryPCoA_ausplots(d = NULL, sites = c("A", "B")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "Failed to generate trajectory PCoA plot:")
  expect_true(nchar(err) > nchar("Failed to generate trajectory PCoA plot:"))
})

# ══════════════════════════════════════════════════════════════════════════════
# trajectoryPlot_ausplots() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: NULL x matrix is caught and wrapped
# Input:         x = NULL
# Expected:      stop() with "Failed to generate trajectory plot:" prefix
test_that("trajectoryPlot_ausplots: NULL x is caught and wrapped", {
  expect_error(
    trajectoryPlot_ausplots(x = NULL, sites = c("A", "B")),
    "Failed to generate trajectory plot:"
  )
})

# ══ SENTRY CAPTURE ══

# What it tests: when d input is invalid, .sentry_capture_exception is called in trajectoryPCoA_ausplots
# Input:         d = NULL; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("trajectoryPCoA_ausplots: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- trajectoryPCoA_ausplots
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn(d = NULL, sites = c("A", "B")))
  expect_true(capture_exception_called)
})

# What it tests: when x input is invalid, .sentry_capture_exception is called in trajectoryPlot_ausplots
# Input:         x = NULL; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("trajectoryPlot_ausplots: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- trajectoryPlot_ausplots
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn(x = NULL, sites = c("A", "B")))
  expect_true(capture_exception_called)
})
