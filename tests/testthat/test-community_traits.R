library(testthat)
library(mockery)
library(jsonlite)

# ══════════════════════════════════════════════════════════════════════════════
# FIXTURE LOADER
# ══════════════════════════════════════════════════════════════════════════════

fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name), simplifyDataFrame = TRUE)
}

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — build a minimal ausplots-like object from fixtures
#
# veg_pi.json uses site_location_name="SATFLB0004" and
# site_location_visit_id=53705, giving site_unique="SATFLB0004-53705".
# ══════════════════════════════════════════════════════════════════════════════

make_ausplots_from_fixtures <- function() {
  pi_raw <- fixture("veg_pi.json")
  pi_df <- data.frame(
    site_unique = paste0(pi_raw$site_location_name, "-", pi_raw$site_location_visit_id),
    hits_unique = seq_len(nrow(pi_raw)),
    pi_raw,
    stringsAsFactors = FALSE
  )
  list(veg.PI = pi_df)
}

# ══════════════════════════════════════════════════════════════════════════════
# community_traits() — INPUT VALIDATION
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: non-list input throws an informative error immediately
# Input:         "not a list" — not a get_ausplots() object
# Expected:      error containing "`ausplots` must be an object returned by get_ausplots"
test_that("community_traits: non-list input throws error", {
  expect_error(
    community_traits("not a list"),
    "`ausplots` must be an object returned by get_ausplots"
  )
})

# What it tests: list missing veg.PI throws an informative error
# Input:         list without veg.PI
# Expected:      error containing "`ausplots$veg.PI` is missing"
test_that("community_traits: missing veg.PI throws error", {
  expect_error(
    community_traits(list(veg.vouch = data.frame())),
    "`ausplots\\$veg.PI` is missing"
  )
})

# What it tests: categorical_trait column that does not exist in trait_data_pp throws error
# Input:         valid ausplots object; categorical_trait = "nonexistent_col"
# Expected:      error containing "not found in `trait_data_pp`"
test_that("community_traits: invalid categorical_trait throws error", {
  mockery::stub(community_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  ap <- make_ausplots_from_fixtures()
  expect_error(
    community_traits(ap, categorical_trait = "nonexistent_col"),
    "not found in `trait_data_pp`"
  )
})

# What it tests: species column not found in veg.PI throws error
# Input:         veg.PI missing "standardised_name" column; species_name = "SN"
# Expected:      error containing "not found in `veg.PI`"
test_that("community_traits: missing species column in veg.PI throws error", {
  mockery::stub(community_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  pi <- data.frame(
    site_unique = "SATFLB0004-53705",
    hits_unique = 1L,
    herbarium_determination = "Acacia aneura",
    stringsAsFactors = FALSE
  )
  expect_error(
    community_traits(list(veg.PI = pi), species_name = "SN"),
    "not found in `veg.PI`"
  )
})

# What it tests: plots filter leaving no rows in veg.PI throws error
# Input:         valid ausplots object from fixtures; plots = "NONEXISTENT_SITE"
# Expected:      error containing "No rows left in `veg.PI` after filtering"
test_that("community_traits: plots filter with no matching sites throws error", {
  mockery::stub(community_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  ap <- make_ausplots_from_fixtures()
  expect_error(
    community_traits(ap, plots = "NONEXISTENT_SITE"),
    "No rows left in `veg.PI` after filtering"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# community_traits() — OUTPUT STRUCTURE
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: result is a data.frame with one row per site_unique
# Input:         ausplots object built from fixtures; FD and adiv must be installed
# Expected:      data.frame with nrow == number of unique site_unique values
#                and a "site_unique" column
test_that("community_traits: returns one row per site_unique", {
  skip_if_not_installed("FD")
  skip_if_not_installed("adiv")
  mockery::stub(community_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  ap <- make_ausplots_from_fixtures()
  result <- community_traits(ap)
  expect_true(is.data.frame(result))
  expect_true("site_unique" %in% names(result))
  n_sites <- length(unique(ap$veg.PI$site_unique))
  expect_equal(nrow(result), n_sites)
})

# What it tests: result contains CWM columns for numeric traits
# Input:         ausplots object built from fixtures
# Expected:      at least one column name starts with "CWM_"
test_that("community_traits: result contains CWM columns", {
  skip_if_not_installed("FD")
  skip_if_not_installed("adiv")
  mockery::stub(community_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  ap <- make_ausplots_from_fixtures()
  result <- community_traits(ap)
  expect_true(any(grepl("^CWM_", names(result))))
})

# What it tests: result contains photosynthetic pathway proportion columns
# Input:         ausplots object built from fixtures
# Expected:      result contains prop_C3 and prop_C4
test_that("community_traits: result contains prop_C3 and prop_C4 columns", {
  skip_if_not_installed("FD")
  skip_if_not_installed("adiv")
  mockery::stub(community_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  ap <- make_ausplots_from_fixtures()
  result <- community_traits(ap)
  expect_true("prop_C3" %in% names(result))
  expect_true("prop_C4" %in% names(result))
})

# What it tests: result contains functional diversity metric columns
# Input:         ausplots object built from fixtures
# Expected:      result contains FRic, FEve, FDis, RaoQ columns
test_that("community_traits: result contains functional diversity columns", {
  skip_if_not_installed("FD")
  skip_if_not_installed("adiv")
  mockery::stub(community_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  ap <- make_ausplots_from_fixtures()
  result <- community_traits(ap)
  fd_cols <- c("FRic", "FEve", "FDis", "RaoQ")
  expect_true(all(fd_cols %in% names(result)))
})

# What it tests: result contains functional redundancy and uniqueness columns
# Input:         ausplots object built from fixtures
# Expected:      result contains functional_redundancy and functional_uniqueness
test_that("community_traits: result contains functional redundancy and uniqueness columns", {
  skip_if_not_installed("FD")
  skip_if_not_installed("adiv")
  mockery::stub(community_traits, "get_trait_data_pp", function() fixture("species_level_functional_traits.json"))
  ap <- make_ausplots_from_fixtures()
  result <- community_traits(ap)
  expect_true("functional_redundancy" %in% names(result))
  expect_true("functional_uniqueness" %in% names(result))
})