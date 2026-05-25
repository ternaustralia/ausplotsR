library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# ma_ausplot_ma()
#
# Internal function that reshapes a long-format site/species data frame into
# a wide presence-absence matrix. Used by species_table() and
# growth_form_table(). Has one explicit stop() for too few columns.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# ma_ausplot_ma() — OUTPUT / HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: valid 2-column input is reshaped without error
# Input:         data frame with plot and species columns
# Expected:      no error; result is a data frame
test_that("ma_ausplot_ma: valid 2-column input reshapes without error", {
  df <- data.frame(
    plot = c("A", "A", "B"),
    spec = c("sp1", "sp2", "sp1"),
    stringsAsFactors = FALSE
  )
  expect_no_error(ma_ausplot_ma(df))
  result <- ma_ausplot_ma(df)
  expect_s3_class(result, "data.frame")
})

# ══════════════════════════════════════════════════════════════════════════════
# ma_ausplot_ma() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: single-column input triggers explicit stop() inside tryCatch
# Input:         data frame with only 1 column
# Expected:      stop() with "Failed to reshape site-by-species matrix:" prefix
test_that("ma_ausplot_ma: single-column input is caught and wrapped", {
  df <- data.frame(plot = c("A", "B"), stringsAsFactors = FALSE)
  expect_error(ma_ausplot_ma(df), "Failed to reshape site-by-species matrix:")
})

# What it tests: original error message survives inside the wrapped message
# Input:         single-column data frame
# Expected:      error contains the original stop() cause
test_that("ma_ausplot_ma: wrapped error preserves original cause", {
  df <- data.frame(plot = c("A", "B"), stringsAsFactors = FALSE)
  expect_error(ma_ausplot_ma(df), "at least two columns")
})

# What it tests: NULL input is caught and wrapped
# Input:         NULL
# Expected:      stop() with "Failed to reshape site-by-species matrix:" prefix
test_that("ma_ausplot_ma: NULL input is caught and wrapped", {
  expect_error(ma_ausplot_ma(NULL), "Failed to reshape site-by-species matrix:")
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid, .sentry_capture_exception is called
# Input:         NULL; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("ma_ausplot_ma: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- ma_ausplot_ma
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn(NULL))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid 2-column data.frame; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("ma_ausplot_ma: capture_message 'info' on successful reshape", {
  captured_levels <- character(0)
  fn <- ma_ausplot_ma
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  df <- data.frame(
    site_unique = c("A", "A", "B"),
    herbarium_determination = c("sp1", "sp2", "sp1"),
    cover = c(0.1, 0.2, 0.3),
    stringsAsFactors = FALSE
  )
  fn(df)
  expect_true("info" %in% captured_levels)
})
