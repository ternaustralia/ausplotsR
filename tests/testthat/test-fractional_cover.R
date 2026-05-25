library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# fractional_cover()
#
# Classifies point intercept hits as green/brown/bare and returns a per-site
# fractional cover matrix. Supports both canopy (default) and ground cover.
# No API calls — all inputs come from veg.PI passed by the caller.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — minimal valid point intercept data frame
# ══════════════════════════════════════════════════════════════════════════════

make_veg_pi <- function() {
  data.frame(
    site_unique   = c("SITE001", "SITE001", "SITE001", "SITE001"),
    hits_unique   = c("H1", "H1", "H2", "H3"),
    growth_form   = c("Tree", "Shrub", NA_character_, NA_character_),
    in_canopy_sky = c(FALSE, FALSE, FALSE, FALSE),
    substrate     = c(NA_character_, NA_character_, "Bare", "Litter"),
    dead          = c(FALSE, FALSE, FALSE, FALSE),
    height        = c(10.0, 2.0, 0.0, 0.0),
    stringsAsFactors = FALSE
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# fractional_cover() — OUTPUT STRUCTURE (happy path)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: result is a data.frame
# Input:         minimal valid veg.PI
# Expected:      is.data.frame(result) is TRUE
test_that("fractional_cover: returns a data frame", {
  result <- fractional_cover(make_veg_pi())
  expect_true(is.data.frame(result))
})

# What it tests: site_unique column present in result
# Input:         minimal valid veg.PI
# Expected:      "site_unique" column exists
test_that("fractional_cover: site_unique column is present in result", {
  result <- fractional_cover(make_veg_pi())
  expect_true("site_unique" %in% names(result))
})

# What it tests: fractional percentages sum to 100 for each site
# Input:         minimal valid veg.PI
# Expected:      sum of all non-site_unique numeric columns = 100
test_that("fractional_cover: fractional percentages sum to 100 for each site", {
  result <- fractional_cover(make_veg_pi())
  numeric_cols <- setdiff(names(result), "site_unique")
  row_sums <- rowSums(result[, numeric_cols, drop = FALSE], na.rm = TRUE)
  expect_true(all(round(row_sums, 0) == 100))
})

# What it tests: ground_fractional=TRUE branch returns a data frame
# Input:         minimal valid veg.PI, ground_fractional=TRUE
# Expected:      is.data.frame(result) is TRUE
test_that("fractional_cover: ground_fractional=TRUE returns a data frame", {
  result <- fractional_cover(make_veg_pi(), ground_fractional = TRUE)
  expect_true(is.data.frame(result))
})

# What it tests: in_canopy_sky=TRUE flag runs without error
# Input:         minimal valid veg.PI, in_canopy_sky=TRUE
# Expected:      no error thrown
test_that("fractional_cover: in_canopy_sky=TRUE runs without error", {
  expect_no_error(fractional_cover(make_veg_pi(), in_canopy_sky = TRUE))
})

# ══════════════════════════════════════════════════════════════════════════════
# fractional_cover() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: tryCatch catches and wraps classification loop error
# Input:         veg.PI with growth_form column removed
# Expected:      stop() with "Failed to calculate fractional cover:" prefix
test_that("fractional_cover: missing growth_form column is caught and wrapped", {
  vpi <- make_veg_pi()
  vpi$growth_form <- NULL
  expect_error(fractional_cover(vpi), "Failed to calculate fractional cover:")
})

# What it tests: original error message survives inside the wrapped message
# Input:         veg.PI with growth_form column removed
# Expected:      error message contains the original R cause
test_that("fractional_cover: wrapped error preserves original cause", {
  vpi <- make_veg_pi()
  vpi$growth_form <- NULL
  expect_error(fractional_cover(vpi), "argument is of length zero")
})

# What it tests: missing height column caught during the sort step
# Input:         veg.PI with height column removed
# Expected:      stop() with "Failed to calculate fractional cover:" prefix
test_that("fractional_cover: missing height column is caught and wrapped", {
  vpi <- make_veg_pi()
  vpi$height <- NULL
  expect_error(fractional_cover(vpi), "Failed to calculate fractional cover:")
})

# What it tests: ground_fractional=TRUE branch also covered by tryCatch
# Input:         veg.PI with growth_form removed, ground_fractional=TRUE
# Expected:      stop() with "Failed to calculate fractional cover:" prefix
test_that("fractional_cover: ground_fractional=TRUE missing growth_form is caught and wrapped", {
  vpi <- make_veg_pi()
  vpi$growth_form <- NULL
  expect_error(fractional_cover(vpi, ground_fractional = TRUE),
               "Failed to calculate fractional cover:")
})

# ══════════════════════════════════════════════════════════════════════════════
# fractional_cover() — SILENT WRONG OUTPUT (tryCatch never fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: all-unrecognised substrate + NA growth_form returns 100%
#               "other" silently — no error or warning raised
# Input:         veg.PI where all growth_form=NA and substrate="ZZZ" (unknown)
# Expected:      function returns without error; result has an "other" column
test_that("fractional_cover: all-unrecognised substrate with NA growth_form returns 100 percent other silently", {
  vpi <- make_veg_pi()
  vpi$growth_form <- NA_character_
  vpi$substrate   <- "ZZZ"
  result <- fractional_cover(vpi)
  expect_true("other" %in% colnames(result))
})

# What it tests: same silent path in the ground_fractional=TRUE branch
# Input:         veg.PI where all growth_form=NA and substrate="ZZZ", ground_fractional=TRUE
# Expected:      function returns without error; result has an "other" column
test_that("fractional_cover: ground_fractional=TRUE all-unrecognised input returns 100 percent other silently", {
  vpi <- make_veg_pi()
  vpi$growth_form <- NA_character_
  vpi$substrate   <- "ZZZ"
  result <- fractional_cover(vpi, ground_fractional = TRUE)
  expect_true("other" %in% colnames(result))
})

# ══ SENTRY CAPTURE ══

# What it tests: when growth_form is missing, .sentry_capture_exception is called
# Input:         veg.PI with growth_form removed; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("fractional_cover: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- fractional_cover
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  vpi <- make_veg_pi()
  vpi$growth_form <- NULL
  expect_error(fn(vpi))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid veg.PI; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("fractional_cover: capture_message 'info' on successful computation", {
  captured_levels <- character(0)
  fn <- fractional_cover
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  fn(make_veg_pi())
  expect_true("info" %in% captured_levels)
})
