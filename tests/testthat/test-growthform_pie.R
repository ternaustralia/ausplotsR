library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# growthform_pie()
#
# Renders a pie chart of growth form cover for a single site row. Has one
# line of real logic before the try()-wrapped plot calls:
#   GF <- GF[, which(colSums(GF) > 0), drop=FALSE]
# Errors in that line escape try() and are caught by the outer tryCatch.
# No API calls — takes a single row from growth_form_table() output.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — minimal valid GF row and matching colour vector
# ══════════════════════════════════════════════════════════════════════════════

make_gf_row <- function() {
  m <- data.frame(
    "Tree/Palm" = 50,
    Shrub       = 30,
    Grass       = 20,
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(m) <- "SITE001"
  m
}

make_gf_colours <- function() {
  c("Tree/Palm" = "darkgreen", "Shrub" = "yellow", "Grass" = "lightgreen")
}

# ══════════════════════════════════════════════════════════════════════════════
# growthform_pie() — OUTPUT / HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: function runs without error on valid GF row
# Input:         1-row numeric data frame + matching named colour vector
# Expected:      no error thrown
test_that("growthform_pie: runs without error on valid data", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(growthform_pie(make_gf_row(), make_gf_colours()))
})

# What it tests: function is callable and returns without error
# Input:         valid GF row + colour vector
# Expected:      no error thrown
test_that("growthform_pie: returns without error", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(growthform_pie(make_gf_row(), make_gf_colours()))
})

# What it tests: all-zero columns are dropped before plotting without error
# Input:         GF row where one column is 0
# Expected:      no error thrown (zero column silently dropped)
test_that("growthform_pie: all-zero column is silently dropped", {
  gf <- make_gf_row()
  gf$Grass <- 0
  col <- c(make_gf_colours(), Grass = "brown")
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(growthform_pie(gf, col))
})

# ══════════════════════════════════════════════════════════════════════════════
# growthform_pie() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: non-matrix/non-data.frame GF causes colSums() to throw,
#               which escapes try() and is caught by the outer tryCatch
# Input:         GF = "not a matrix"
# Expected:      stop() with "Failed to generate growth form pie chart:" prefix
test_that("growthform_pie: non-matrix GF input is caught and wrapped", {
  expect_error(
    growthform_pie("not a matrix", make_gf_colours()),
    "Failed to generate growth form pie chart:"
  )
})

# What it tests: original error message survives inside the wrapped message
# Input:         GF = "not a matrix"
# Expected:      error message contains the original R cause from colSums()
test_that("growthform_pie: wrapped error preserves original cause", {
  expect_error(
    growthform_pie("not a matrix", make_gf_colours()),
    "not numeric|'x' must be|requires numeric"
  )
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid, .sentry_capture_exception is called
# Input:         "not a matrix"; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("growthform_pie: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- growthform_pie
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn("not a matrix", make_gf_colours()))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid GF matrix; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("growthform_pie: capture_message 'info' on successful plot", {
  captured_levels <- character(0)
  fn <- growthform_pie
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  fn(make_gf_row(), make_gf_colours())
  expect_true("info" %in% captured_levels)
})
