library(testthat)
library(mockery)
library(jsonlite)

# ══════════════════════════════════════════════════════════════════════════════
# species_traits()
#
# Merges a species list with the trait table fetched via get_trait_data_pp().
# Input can come from species_list (character vector), species_df (data.frame),
# or an ausplots object's veg.vouch table.
# All tests mock get_trait_data_pp to avoid requiring a live network connection.
# ══════════════════════════════════════════════════════════════════════════════

fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name), simplifyDataFrame = TRUE)
}

# What it tests: species_list input returns a data.frame with a species_name column
# Input:         species_list = c("Acacia aneura", "Eucalyptus camaldulensis")
# Expected:      data.frame with species_name column
test_that("species_traits: species_list returns a data.frame with species_name column", {
  mockery::stub(species_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  result <- species_traits(species_list = c("Acacia aneura", "Eucalyptus camaldulensis"))
  expect_true(is.data.frame(result))
  expect_true("species_name" %in% names(result))
})

# What it tests: genus-only names (no space) are silently dropped before merging
# Input:         species_list = c("Acacia", "Eucalyptus camaldulensis")
# Expected:      "Acacia" is absent from result
test_that("species_traits: genus-only names are silently removed", {
  mockery::stub(species_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  result <- species_traits(species_list = c("Acacia", "Eucalyptus camaldulensis"))
  expect_false("Acacia" %in% result$species_name)
})

# What it tests: NA and empty string entries in species_list are silently dropped
# Input:         species_list = c(NA, "", "Acacia aneura")
# Expected:      no NA or empty string in result$species_name
test_that("species_traits: NAs and empty strings in species_list are dropped", {
  mockery::stub(species_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  result <- species_traits(species_list = c(NA, "", "Acacia aneura"))
  expect_false(any(is.na(result$species_name)))
  expect_false("" %in% result$species_name)
})

# What it tests: result rows are ordered alphabetically by species_name
# Input:         species_list with names out of alphabetical order
# Expected:      result$species_name matches sort(result$species_name)
test_that("species_traits: result is ordered alphabetically", {
  mockery::stub(species_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  result <- species_traits(species_list = c("Zygophyllum aurantiacum", "Acacia aneura"))
  expect_equal(result$species_name, sort(result$species_name))
})

# What it tests: species_df that is not a data.frame throws an informative error
# Input:         species_df = "not a df"
# Expected:      error containing "`species_df` must be a data.frame"
test_that("species_traits: species_df that is not a data.frame throws error", {
  mockery::stub(species_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  expect_error(
    species_traits(species_df = "not a df"),
    "`species_df` must be a data.frame"
  )
})

# What it tests: species_df without a species_name column throws an informative error
# Input:         species_df = data.frame(taxon = "Acacia aneura")
# Expected:      error containing "must contain a column called `species_name`"
test_that("species_traits: species_df missing species_name column throws error", {
  mockery::stub(species_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  expect_error(
    species_traits(species_df = data.frame(taxon = "Acacia aneura")),
    "must contain a column called `species_name`"
  )
})

# What it tests: calling with no arguments throws an informative error
# Input:         no arguments at all
# Expected:      error containing "Please provide either"
test_that("species_traits: no inputs throws error", {
  mockery::stub(species_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  expect_error(
    species_traits(),
    "Please provide either"
  )
})

# What it tests: ausplots that is not a list throws an informative error
# Input:         ausplots = "not a list"
# Expected:      error containing "`ausplots` must be an object returned by get_ausplots"
test_that("species_traits: non-list ausplots throws error", {
  mockery::stub(species_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  expect_error(
    species_traits(ausplots = "not a list"),
    "`ausplots` must be an object returned by get_ausplots"
  )
})

# What it tests: ausplots without veg.vouch throws an informative error
# Input:         list with veg.PI but no veg.vouch
# Expected:      error containing "`ausplots$veg.vouch` is missing"
test_that("species_traits: ausplots without veg.vouch throws error", {
  mockery::stub(species_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  expect_error(
    species_traits(ausplots = list(veg.PI = data.frame())),
    "`ausplots\\$veg.vouch` is missing"
  )
})