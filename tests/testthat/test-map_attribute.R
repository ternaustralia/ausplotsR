library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# map_attribute()
#
# Renders a ggplot2 map of AusPlots sites scaled by tree canopy cover.
# Calls single_cover_value() internally. No API calls — takes an ausplots
# object directly. Guarded by an if-block that skips all plotting when no
# Tree/Palm or Tree Mallee hits are present.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — minimal ausplots object with no tree growth forms
# ══════════════════════════════════════════════════════════════════════════════

make_no_tree_obj <- function() {
  list(
    site.info = data.frame(
      site_unique       = "SITE001-1",
      site_location_name = "SITE001",
      bioregion_name    = "BioA",
      longitude         = 138.0,
      latitude          = -34.0,
      stringsAsFactors  = FALSE
    ),
    veg.PI = data.frame(
      site_unique   = "SITE001-1",
      hits_unique   = "H1",
      growth_form   = "Shrub",
      in_canopy_sky = FALSE,
      stringsAsFactors = FALSE
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# map_attribute() — OUTPUT / HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: no-tree input skips the if-block and runs without error
# Input:         ausplots object with only Shrub growth_form
# Expected:      no error thrown; message about no trees emitted
test_that("map_attribute: no-tree input skips plotting without error", {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  expect_message(
    map_attribute(make_no_tree_obj()),
    "no trees"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# map_attribute() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: NULL ausplots object hits the no-trees branch (NULL %in% NULL = FALSE)
# Input:         NULL (no valid object)
# Expected:      no error; message about no trees emitted
test_that("map_attribute: NULL input hits no-trees branch without error", {
  expect_message(map_attribute(NULL), "no trees")
})

# What it tests: NULL input takes the no-trees path, not an error path
# Input:         NULL
# Expected:      message emitted, no error
test_that("map_attribute: NULL input takes no-trees path not error path", {
  expect_no_error(map_attribute(NULL))
})

# What it tests: missing veg.PI hits the no-trees branch (NULL %in% NULL = FALSE)
# Input:         ausplots object with veg.PI removed
# Expected:      no error; message about no trees emitted
test_that("map_attribute: missing veg.PI hits no-trees branch without error", {
  obj <- make_no_tree_obj()
  obj$veg.PI <- NULL
  expect_message(map_attribute(obj), "no trees")
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid, .sentry_capture_exception is called
# Input:         invalid ausplots object; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("map_attribute: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- map_attribute
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn("not a list"))
  expect_true(capture_exception_called)
})

# What it tests: on success (no-tree path), .sentry_capture_message is called with level "info"
# Input:         no-tree ausplots object; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("map_attribute: capture_message 'info' on successful completion", {
  captured_levels <- character(0)
  fn <- map_attribute
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  fn(make_no_tree_obj())
  expect_true("info" %in% captured_levels)
})
