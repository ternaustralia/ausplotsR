library(testthat)
library(jsonlite)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# FIXTURE LOADER
# ══════════════════════════════════════════════════════════════════════════════

fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name), simplifyDataFrame = TRUE)
}

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — build a minimal ausplots-like object from fixtures
#
# veg_voucher.json and veg_pi.json both use site_location_name="SATFLB0004"
# and site_location_visit_id=53705, giving site_unique="SATFLB0004-53705".
# ══════════════════════════════════════════════════════════════════════════════

make_ausplots_from_fixtures <- function() {
  vouch_raw <- fixture("veg_voucher.json")
  vouch <- data.frame(
    site_unique = paste0(vouch_raw$site_location_name, "-", vouch_raw$site_location_visit_id),
    vouch_raw,
    stringsAsFactors = FALSE
  )

  pi_raw <- fixture("veg_pi.json")
  pi_df <- data.frame(
    site_unique = paste0(pi_raw$site_location_name, "-", pi_raw$site_location_visit_id),
    hits_unique = seq_len(nrow(pi_raw)),
    pi_raw,
    stringsAsFactors = FALSE
  )

  list(veg.vouch = vouch, veg.PI = pi_df)
}

# ══════════════════════════════════════════════════════════════════════════════
# biodiversity_metrics() — INPUT VALIDATION
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: non-list input throws an informative error immediately
# Input:         "not a list" — not a get_ausplots() object
# Expected:      error containing "`ausplots` must be an object returned by get_ausplots"
test_that("biodiversity_metrics: non-list input throws error", {
  expect_error(
    biodiversity_metrics("not a list"),
    "`ausplots` must be an object returned by get_ausplots"
  )
})

# What it tests: list missing veg.vouch throws an informative error
# Input:         list with veg.PI but no veg.vouch
# Expected:      error containing "`ausplots$veg.vouchers` is missing"
test_that("biodiversity_metrics: missing veg.vouch throws error", {
  expect_error(
    biodiversity_metrics(list(veg.PI = data.frame())),
    "`ausplots\\$veg.vouchers` is missing"
  )
})

# What it tests: list missing veg.PI throws an informative error
# Input:         list with veg.vouch but no veg.PI
# Expected:      error containing "`ausplots$veg.PI` is missing"
test_that("biodiversity_metrics: missing veg.PI throws error", {
  expect_error(
    biodiversity_metrics(list(veg.vouch = data.frame())),
    "`ausplots\\$veg.PI` is missing"
  )
})

# What it tests: plots filter with a site_unique that does not exist throws error
# Input:         valid ausplots object from fixtures; plots = "NONEXISTENT_SITE"
# Expected:      error containing "No plots left"
test_that("biodiversity_metrics: plots filter with no matching sites throws error", {
  ap <- make_ausplots_from_fixtures()
  expect_error(
    biodiversity_metrics(ap, plots = "NONEXISTENT_SITE"),
    "No plots left"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# biodiversity_metrics() — OUTPUT STRUCTURE
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: result is a data.frame with one row per unique site_unique
# Input:         ausplots object built from fixtures (site SATFLB0004-53705)
# Expected:      data.frame with nrow == number of unique site_unique values
#                and a "site_unique" column
test_that("biodiversity_metrics: returns one row per site_unique", {
  ap <- make_ausplots_from_fixtures()
  result <- biodiversity_metrics(ap)
  expect_true(is.data.frame(result))
  n_sites <- length(unique(ap$veg.vouch$site_unique))
  expect_equal(nrow(result), n_sites)
  expect_true("site_unique" %in% names(result))
})

# What it tests: all expected output columns are present in the result
# Input:         ausplots object built from fixtures
# Expected:      result contains site_unique, species_richness, total_abundance,
#                shannon, simpson, pielou_evenness, RRR, CWE
test_that("biodiversity_metrics: all expected output columns are present", {
  ap <- make_ausplots_from_fixtures()
  result <- biodiversity_metrics(ap)
  expected_cols <- c("site_unique", "species_richness", "total_abundance",
                     "shannon", "simpson", "pielou_evenness", "RRR", "CWE")
  expect_true(all(expected_cols %in% names(result)))
})

# What it tests: species_richness is non-negative for all plots
# Input:         ausplots object built from fixtures
# Expected:      all values in species_richness >= 0
test_that("biodiversity_metrics: species_richness is non-negative", {
  ap <- make_ausplots_from_fixtures()
  result <- biodiversity_metrics(ap)
  expect_true(all(result$species_richness >= 0))
})

# What it tests: plots filter returns only the requested subset of sites
# Input:         ausplots object built from fixtures; plots = "SATFLB0004-53705"
# Expected:      1-row result with site_unique == "SATFLB0004-53705"
test_that("biodiversity_metrics: plots filter returns only the requested site", {
  ap <- make_ausplots_from_fixtures()
  site <- "SATFLB0004-53705"
  result <- biodiversity_metrics(ap, plots = site)
  expect_equal(nrow(result), 1L)
  expect_equal(result$site_unique, site)
})

# ══════════════════════════════════════════════════════════════════════════════
# biodiversity_metrics() — ERROR HANDLING (tryCatch)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: species_table() failure is caught and wrapped
# Input:         valid ausplots object; species_table mocked to throw
# Expected:      error containing "Failed to calculate biodiversity metrics:"
test_that("biodiversity_metrics: species_table failure is caught and wrapped", {
  ap <- make_ausplots_from_fixtures()
  mockery::stub(biodiversity_metrics, "species_table", function(...) stop("mock species_table error"))
  expect_error(
    biodiversity_metrics(ap),
    "Failed to calculate biodiversity metrics:"
  )
})

# What it tests: wrapped error preserves the original cause
# Input:         species_table mocked to throw "mock species_table error"
# Expected:      error message contains "mock species_table error"
test_that("biodiversity_metrics: wrapped error preserves original cause", {
  ap <- make_ausplots_from_fixtures()
  mockery::stub(biodiversity_metrics, "species_table", function(...) stop("mock species_table error"))
  expect_error(
    biodiversity_metrics(ap),
    "mock species_table error"
  )
})

# What it tests: wrapped error has call.=FALSE
# Input:         ausplots = "not a list"
# Expected:      error is raised; err$call is NULL
test_that("biodiversity_metrics: wrapped error has call.=FALSE", {
  err <- tryCatch(biodiversity_metrics("not a list"), error = function(e) e)
  expect_null(err$call)
})