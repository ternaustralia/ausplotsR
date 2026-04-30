library(testthat)
library(mockery)
library(jsonlite)

# ══════════════════════════════════════════════════════════════════════════════
# get_trait_data_pp()
#
# Fetches species-level functional traits from the AusPlots REST API
# (/species_level_functional_traits) and returns a data.frame with
# d13c renamed to d13C to match the internal schema.
# All tests mock .ausplots_api to avoid requiring a live network connection.
# ══════════════════════════════════════════════════════════════════════════════

fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name), simplifyDataFrame = TRUE)
}

# ══════════════════════════════════════════════════════════════════════════════
# get_trait_data_pp() — OUTPUT STRUCTURE
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: result is a data.frame
# Input:         mocked API response from fixture
# Expected:      is.data.frame(result) is TRUE
test_that("get_trait_data_pp: returns a data.frame", {
  mockery::stub(get_trait_data_pp, ".ausplots_api", fixture("species_level_functional_traits.json"))
  result <- get_trait_data_pp()
  expect_true(is.data.frame(result))
})

# What it tests: row count matches fixture
# Input:         mocked API response with 3 rows
# Expected:      nrow(result) == 3
test_that("get_trait_data_pp: row count matches fixture", {
  mockery::stub(get_trait_data_pp, ".ausplots_api", fixture("species_level_functional_traits.json"))
  result <- get_trait_data_pp()
  expect_equal(nrow(result), 3L)
})

# What it tests: d13c is renamed to d13C
# Input:         mocked API response containing column named "d13c"
# Expected:      "d13C" in names(result) and "d13c" not in names(result)
test_that("get_trait_data_pp: renames d13c to d13C", {
  mockery::stub(get_trait_data_pp, ".ausplots_api", fixture("species_level_functional_traits.json"))
  result <- get_trait_data_pp()
  expect_true("d13C" %in% names(result))
  expect_false("d13c" %in% names(result))
})

# What it tests: all 11 expected columns are present
# Input:         mocked API response from fixture
# Expected:      all required column names present
test_that("get_trait_data_pp: all 11 expected columns are present", {
  mockery::stub(get_trait_data_pp, ".ausplots_api", fixture("species_level_functional_traits.json"))
  result <- get_trait_data_pp()
  expected_cols <- c(
    "species_name", "family_name", "leaf_length", "leaf_width",
    "seed_length", "leaf_area", "seed_dry_mass", "leaf_mass_per_area",
    "photosynthetic_pathway", "max_height", "d13C"
  )
  expect_true(all(expected_cols %in% names(result)))
})

# What it tests: species_name column is character type
# Input:         mocked API response from fixture
# Expected:      is.character(result$species_name) is TRUE
test_that("get_trait_data_pp: species_name is character", {
  mockery::stub(get_trait_data_pp, ".ausplots_api", fixture("species_level_functional_traits.json"))
  result <- get_trait_data_pp()
  expect_true(is.character(result$species_name))
})

# What it tests: numeric trait columns are numeric type
# Input:         mocked API response from fixture
# Expected:      leaf_length and max_height are numeric
test_that("get_trait_data_pp: numeric trait columns are numeric", {
  mockery::stub(get_trait_data_pp, ".ausplots_api", fixture("species_level_functional_traits.json"))
  result <- get_trait_data_pp()
  expect_true(is.numeric(result$leaf_length))
  expect_true(is.numeric(result$max_height))
})

# ══════════════════════════════════════════════════════════════════════════════
# get_trait_data_pp() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: tryCatch catches and wraps .ausplots_api failure
# Input:         .ausplots_api mocked to throw
# Expected:      stop() with "Failed to retrieve species-level trait data:" prefix
test_that("get_trait_data_pp: .ausplots_api failure is caught and wrapped with context", {
  mockery::stub(get_trait_data_pp, ".ausplots_api", function(...) stop("connection refused"))
  expect_error(get_trait_data_pp(), "Failed to retrieve species-level trait data:")
})

# What it tests: original error message survives inside the wrapped message
# Input:         .ausplots_api mocked to throw with known message
# Expected:      wrapped error contains the original cause
test_that("get_trait_data_pp: wrapped error preserves original cause", {
  mockery::stub(get_trait_data_pp, ".ausplots_api", function(...) stop("connection refused"))
  expect_error(get_trait_data_pp(), "connection refused")
})

# What it tests: as.data.frame failure on non-coercible return is caught
# Input:         .ausplots_api returns an environment (not coercible to data.frame)
# Expected:      stop() with "Failed to retrieve species-level trait data:" prefix
test_that("get_trait_data_pp: as.data.frame failure on non-coercible return is caught", {
  mockery::stub(get_trait_data_pp, ".ausplots_api", function(...) new.env())
  expect_error(get_trait_data_pp(), "Failed to retrieve species-level trait data:")
})

# ══════════════════════════════════════════════════════════════════════════════
# get_trait_data_pp() — SILENT WRONG OUTPUT (tryCatch never fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: API returns df with neither d13c nor d13C — rename is silently skipped
# Input:         .ausplots_api returns df with no d13c/d13C column
# Expected:      function returns without error; d13C absent from result
test_that("get_trait_data_pp: missing d13c column causes rename to be silently skipped", {
  mockery::stub(get_trait_data_pp, ".ausplots_api",
    function(...) data.frame(species_name = "Acacia", stringsAsFactors = FALSE))
  result <- get_trait_data_pp()
  expect_false("d13C" %in% names(result))
})

# What it tests: API returns df already named d13C — rename guard skips correctly
# Input:         .ausplots_api returns df already containing d13C
# Expected:      function returns without error; d13C present exactly once
test_that("get_trait_data_pp: d13C already present is preserved without duplication", {
  mockery::stub(get_trait_data_pp, ".ausplots_api",
    function(...) data.frame(species_name = "Acacia", d13C = -28.5, stringsAsFactors = FALSE))
  result <- get_trait_data_pp()
  expect_true("d13C" %in% names(result))
  expect_equal(sum(names(result) == "d13C"), 1L)
})
