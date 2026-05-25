library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# whitt.plot()
#
# Renders a Whittaker rank-abundance diagram using vegan::rad.lognormal().
# Both plot() and legend() are already wrapped in try() internally.
# The outer tryCatch catches errors that escape try() (e.g. pre-plot crashes).
# No API calls — takes an abundance vector directly.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# whitt.plot() — OUTPUT / HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: valid abundance vector renders without error
# Input:         named numeric vector (as would come from species_table row)
# Expected:      no error
test_that("whitt.plot: valid abundance vector renders without error", {
  m <- matrix(c(3, 1, 0, 2), nrow = 1,
              dimnames = list("SITE001-1", c("sp1", "sp2", "sp3", "sp4")))
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(whitt.plot(m))
})

# ══════════════════════════════════════════════════════════════════════════════
# whitt.plot() — SILENT PATHS (try() swallows internally)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: NULL input is silently swallowed by try() — no error raised
# Input:         NULL
# Expected:      no error (try() absorbs the crash)
test_that("whitt.plot: NULL input is silently swallowed by try", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(whitt.plot(NULL))
})

# ══ SENTRY CAPTURE ══

# What it tests: when input causes an error (non-try path), .sentry_capture_exception is called
# Input:         invalid abund_vect forcing error; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("whitt.plot: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- whitt.plot
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  mockery::stub(fn, "try", function(...) stop("forced error"))
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_error(fn(c(5, 3, 2, 1)))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid abundance vector; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("whitt.plot: capture_message 'info' on successful plot", {
  captured_levels <- character(0)
  fn <- whitt.plot
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  fn(c(5, 3, 2, 1))
  expect_true("info" %in% captured_levels)
})
