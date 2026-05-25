library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# basal_area()
#
# Calculates basal area per plot from raw basal wedge sweep data.
# No API calls — all inputs come from veg.basal passed by the caller.
# ══════════════════════════════════════════════════════════════════════════════

make_veg_basal <- function() {
  data.frame(
    site_unique             = c("SITE001", "SITE001", "SITE002"),
    point_id                = c(1L, 2L, 1L),
    basal_area              = c(5.0, 3.0, 4.0),
    hits                    = c(2L, 1L, 3L),
    standardised_name       = c("Eucalyptus sp.", "Acacia sp.", "Eucalyptus sp."),
    herbarium_determination = c("Eucalyptus sp.", "Acacia sp.", "Eucalyptus sp."),
    genus_species           = c("Eucalyptus sp.", "Acacia sp.", "Eucalyptus sp."),
    genus                   = c("Eucalyptus", "Acacia", "Eucalyptus"),
    stringsAsFactors = FALSE
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# basal_area() — OUTPUT STRUCTURE (happy path)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: default call returns a data frame
# Input:         minimal valid veg.basal
# Expected:      is.data.frame(result) is TRUE
test_that("basal_area: returns a data frame", {
  expect_true(is.data.frame(basal_area(make_veg_basal())))
})

# What it tests: result has site_unique and basal_area_m2_ha columns
# Input:         minimal valid veg.basal
# Expected:      both columns present
test_that("basal_area: result contains site_unique and basal_area_m2_ha columns", {
  result <- basal_area(make_veg_basal())
  expect_true("site_unique" %in% names(result))
  expect_true("basal_area_m2_ha" %in% names(result))
})

# What it tests: by.spp=TRUE, species_name="SN" returns standardised_name column
# Input:         minimal valid veg.basal
# Expected:      "standardised_name" in names(result)
test_that("basal_area: by.spp=TRUE species_name=SN returns standardised_name column", {
  result <- basal_area(make_veg_basal(), by.spp = TRUE, species_name = "SN")
  expect_true("standardised_name" %in% names(result))
})

# What it tests: by.spp=TRUE, species_name="HD" returns herbarium_determination column
# Input:         minimal valid veg.basal
# Expected:      "herbarium_determination" in names(result)
test_that("basal_area: by.spp=TRUE species_name=HD returns herbarium_determination column", {
  result <- basal_area(make_veg_basal(), by.spp = TRUE, species_name = "HD")
  expect_true("herbarium_determination" %in% names(result))
})

# What it tests: by.hits=TRUE returns mean_hits column
# Input:         minimal valid veg.basal
# Expected:      "mean_hits" in names(result)
test_that("basal_area: by.hits=TRUE returns mean_hits column", {
  result <- basal_area(make_veg_basal(), by.hits = TRUE)
  expect_true("mean_hits" %in% names(result))
})

# ══════════════════════════════════════════════════════════════════════════════
# basal_area() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: missing basal_area column is caught and wrapped
# Input:         veg.basal with basal_area column removed
# Expected:      stop() with "Failed to calculate basal area:" prefix
test_that("basal_area: missing basal_area column is caught and wrapped", {
  vb <- make_veg_basal()
  vb$basal_area <- NULL
  expect_error(basal_area(vb), "Failed to calculate basal area:")
})

# What it tests: original error message survives inside the wrapped message
# Input:         veg.basal with point_id column removed — plyr grouping variable missing
# Expected:      error message contains "point_id" from the original R cause
test_that("basal_area: wrapped error preserves original cause", {
  vb <- make_veg_basal()
  vb$point_id <- NULL
  expect_error(basal_area(vb), "point_id")
})

# What it tests: missing site_unique column is caught and wrapped
# Input:         veg.basal with site_unique column removed
# Expected:      stop() with "Failed to calculate basal area:" prefix
test_that("basal_area: missing site_unique column is caught and wrapped", {
  vb <- make_veg_basal()
  vb$site_unique <- NULL
  expect_error(basal_area(vb), "Failed to calculate basal area:")
})

# What it tests: by.hits=TRUE, missing hits column is caught and wrapped
# Input:         veg.basal with hits column removed, by.hits=TRUE
# Expected:      stop() with "Failed to calculate basal area:" prefix
test_that("basal_area: by.hits=TRUE missing hits column is caught and wrapped", {
  vb <- make_veg_basal()
  vb$hits <- NULL
  expect_error(basal_area(vb, by.hits = TRUE), "Failed to calculate basal area:")
})

# ══ SENTRY CAPTURE ══

# What it tests: when basal_area column is missing, .sentry_capture_exception is called
# Input:         veg.basal with basal_area column removed; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("basal_area: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- basal_area
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  vb <- make_veg_basal()
  vb$basal_area <- NULL
  expect_error(fn(vb))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid veg.basal; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("basal_area: capture_message 'info' on successful computation", {
  captured_levels <- character(0)
  fn <- basal_area
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  fn(make_veg_basal())
  expect_true("info" %in% captured_levels)
})
