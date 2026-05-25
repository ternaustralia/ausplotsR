library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# fraction_pie()
#
# Renders a pie chart of fractional cover categories (bare, brown, green,
# other) for a single site row. Both internal calls are already wrapped in
# try(), so virtually all errors are silently swallowed. The outer tryCatch
# is a safety net for unexpected failures.
# No API calls — takes the output of fractional_cover() directly.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — minimal valid fractional cover data frame
# ══════════════════════════════════════════════════════════════════════════════

make_frac <- function() {
  f <- data.frame(
    site_unique = "SITE001",
    bare        = 25,
    brown       = 25,
    green       = 25,
    other       = 25,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )
  rownames(f) <- f$site_unique
  f
}

# ══════════════════════════════════════════════════════════════════════════════
# fraction_pie() — OUTPUT / HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: function runs without error on valid fractional cover data
# Input:         1-row frac data frame; row = "SITE001"
# Expected:      no error thrown
test_that("fraction_pie: runs without error on valid data", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(fraction_pie(make_frac(), "SITE001"))
})

# What it tests: function is callable and returns without error
# Input:         valid frac data frame; row = "SITE001"
# Expected:      no error thrown
test_that("fraction_pie: returns without error", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(fraction_pie(make_frac(), "SITE001"))
})

# ══════════════════════════════════════════════════════════════════════════════
# fraction_pie() — SILENT SWALLOWING (try() absorbs, tryCatch never fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: invalid row name is silently swallowed by the inner try()
# Input:         valid frac data frame; row = "NO_SUCH_ROW"
# Expected:      no error thrown (try() absorbs the subscript error)
test_that("fraction_pie: invalid row is silently swallowed by try", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(fraction_pie(make_frac(), "NO_SUCH_ROW"))
})

# What it tests: NULL frac is silently swallowed by the inner try()
# Input:         frac = NULL; row = 1
# Expected:      no error thrown (try() absorbs the indexing error)
test_that("fraction_pie: NULL frac is silently swallowed by try", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(fraction_pie(NULL, 1))
})

# ══ SENTRY CAPTURE ══

# What it tests: when input causes an error, .sentry_capture_exception is called
# Input:         frac=data.frame with valid cols, row=1; fractional cover stubbed to throw
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("fraction_pie: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- fraction_pie
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  # Force an error by providing a non-try-wrapped internal call
  mockery::stub(fn, "try", function(...) stop("forced error"))
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  frac <- data.frame(site = "A", bare = 0.3, brown = 0.3, green = 0.3, other = 0.1)
  expect_error(fn(frac, 1))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid frac data; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("fraction_pie: capture_message 'info' on successful plot", {
  captured_levels <- character(0)
  fn <- fraction_pie
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  frac <- data.frame(site = "A", bare = 0.3, brown = 0.3, green = 0.3, other = 0.1)
  fn(frac, 1)
  expect_true("info" %in% captured_levels)
})
