library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# cumulative_cover()
#
# Plots cumulative canopy cover across an increasing number of point intercepts
# for a single site. Side-effect only — returns NULL invisibly and renders a
# base R plot to the active device. No API calls — takes veg.PI directly.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — minimal valid veg.PI for cumulative_cover()
# ══════════════════════════════════════════════════════════════════════════════

make_veg_pi_cc <- function() {
  # 10 unique intercepts so the inner k-loop (10, 20, …) executes at least once.
  # Hit IDs begin with "N1-S1 " so transect ordering logic runs without error.
  data.frame(
    herbarium_determination = c(rep("Acacia aneura", 8), NA, NA),
    hits_unique             = paste0("N1-S1 ", 1:10),
    in_canopy_sky           = rep(FALSE, 10),
    stringsAsFactors        = FALSE
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# cumulative_cover() — OUTPUT / HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: function runs without error on valid data and renders a plot
# Input:         10-hit veg.PI with one named species
# Expected:      no error thrown
test_that("cumulative_cover: runs without error on valid data", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(cumulative_cover(make_veg_pi_cc()))
})

# What it tests: function is callable and returns without error
# Input:         10-hit veg.PI
# Expected:      no error thrown
test_that("cumulative_cover: returns without error", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(cumulative_cover(make_veg_pi_cc()))
})

# What it tests: all-NA herbarium_determination skips the species loop entirely
# Input:         veg.PI where all herbarium_determination == NA
# Expected:      no error thrown (outer if-guard skips plotting)
test_that("cumulative_cover: all-NA herbarium_determination runs without error", {
  vpi <- make_veg_pi_cc()
  vpi$herbarium_determination <- NA_character_
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_no_error(cumulative_cover(vpi))
})

# ══════════════════════════════════════════════════════════════════════════════
# cumulative_cover() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: missing herbarium_determination column is caught and wrapped
# Input:         veg.PI with herbarium_determination column removed
# Expected:      stop() with "Failed to generate cumulative cover plot:" prefix
test_that("cumulative_cover: missing herbarium_determination column is caught and wrapped", {
  vpi <- make_veg_pi_cc()
  vpi$herbarium_determination <- NULL
  expect_error(cumulative_cover(vpi), "Failed to generate cumulative cover plot:")
})

# What it tests: original error message survives inside the wrapped message
# Input:         veg.PI with herbarium_determination column removed
# Expected:      error message contains the original R cause
test_that("cumulative_cover: wrapped error preserves original cause", {
  vpi <- make_veg_pi_cc()
  vpi$herbarium_determination <- NULL
  expect_error(cumulative_cover(vpi), "undefined columns selected|not found|subscript out of bounds")
})

# What it tests: missing hits_unique column is caught and wrapped
# Input:         veg.PI with hits_unique column removed
# Expected:      stop() with "Failed to generate cumulative cover plot:" prefix
test_that("cumulative_cover: missing hits_unique column is caught and wrapped", {
  vpi <- make_veg_pi_cc()
  vpi$hits_unique <- NULL
  expect_error(cumulative_cover(vpi), "Failed to generate cumulative cover plot:")
})

# What it tests: missing in_canopy_sky column is caught and wrapped
# Input:         veg.PI with in_canopy_sky column removed
# Expected:      stop() with "Failed to generate cumulative cover plot:" prefix
test_that("cumulative_cover: missing in_canopy_sky column is caught and wrapped", {
  vpi <- make_veg_pi_cc()
  vpi$in_canopy_sky <- NULL
  expect_error(cumulative_cover(vpi), "Failed to generate cumulative cover plot:")
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid, .sentry_capture_exception is called
# Input:         in_canopy_sky column removed; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("cumulative_cover: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- cumulative_cover
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  vpi <- make_veg_pi_cc()
  vpi$in_canopy_sky <- NULL
  expect_error(fn(vpi))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid veg.PI; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("cumulative_cover: capture_message 'info' on successful plot", {
  captured_levels <- character(0)
  fn <- cumulative_cover
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  fn(make_veg_pi_cc())
  expect_true("info" %in% captured_levels)
})
