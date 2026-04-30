library(testthat)
library(jsonlite)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# invasion_info()
#
# Aligns plant names via APCalign and appends AFA invasion status fields
# from the AusPlots REST API (/afa_static).
# All tests mock .ausplots_api using fixtures/afa_static.json.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# FIXTURE LOADER
# ══════════════════════════════════════════════════════════════════════════════

fixture <- function(name) {
  jsonlite::fromJSON(test_path("fixtures", name), simplifyDataFrame = TRUE)
}

# ══════════════════════════════════════════════════════════════════════════════
# invasion_info() — INPUT VALIDATION
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: afa_static that is not a data.frame throws an informative error
# Input:         afa_static = "not a df" (character, not data.frame)
# Expected:      error containing "`afa_static` must be a data.frame"
test_that("invasion_info: afa_static must be a data.frame", {
  expect_error(
    invasion_info(afa_static = "not a df"),
    "`afa_static` must be a data.frame"
  )
})

# What it tests: afa_static missing required schema columns throws an informative error
# Input:         ausplots with herbarium_determination; afa_static = data.frame(x = 1)
# Expected:      error containing "required columns are missing from `afa_static`"
test_that("invasion_info: afa_static missing required columns throws error", {
  mockery::stub(invasion_info, "requireNamespace", TRUE)
  expect_error(
    invasion_info(
      ausplots   = data.frame(herbarium_determination = "Acacia aneura"),
      afa_static = data.frame(x = 1)
    ),
    "required columns are missing from `afa_static`"
  )
})

# What it tests: ausplots that is not NULL or a data.frame throws an informative error
# Input:         ausplots = "bad input" (character, not data.frame or NULL)
# Expected:      error containing "`ausplots` must be a data.frame or NULL"
test_that("invasion_info: non-data.frame ausplots throws error", {
  afa <- as.data.frame(fixture("afa_static.json"))
  mockery::stub(invasion_info, "requireNamespace", TRUE)
  expect_error(
    invasion_info(ausplots = "bad input", afa_static = afa),
    "`ausplots` must be a data.frame or NULL"
  )
})

# What it tests: taxon_col not present in ausplots data.frame throws an informative error
# Input:         ausplots has column "wrong_col"; taxon_col = "herbarium_determination"
# Expected:      error containing "`taxon_col` not found in `ausplots`"
test_that("invasion_info: taxon_col not in ausplots throws error", {
  afa <- as.data.frame(fixture("afa_static.json"))
  mockery::stub(invasion_info, "requireNamespace", TRUE)
  expect_error(
    invasion_info(
      ausplots   = data.frame(wrong_col = "Acacia aneura"),
      afa_static = afa,
      taxon_col  = "herbarium_determination"
    ),
    "`taxon_col` not found in `ausplots`"
  )
})

# What it tests: APCalign not installed produces an informative error before any join
# Input:         valid ausplots and afa_static; APCalign mocked as unavailable
# Expected:      error containing "Package `APCalign` must be installed"
test_that("invasion_info: missing APCalign throws informative error", {
  afa <- as.data.frame(fixture("afa_static.json"))
  input <- data.frame(
    herbarium_determination = "Acacia aneura",
    stringsAsFactors = FALSE
  )
  mockery::stub(invasion_info, "requireNamespace", FALSE)
  expect_error(
    invasion_info(ausplots = input, afa_static = afa),
    "Package `APCalign` must be installed"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# invasion_info() — OUTPUT STRUCTURE (requires APCalign)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: result is a data.frame with more columns than the input,
#   including invasion status columns appended from the AFA dataset
# Input:         2-row ausplots data.frame; bundled afa_static; APCalign installed
# Expected:      result is a data.frame; contains invasion_ACT; has more columns than input
test_that("invasion_info: result contains invasion columns appended to original data", {
  skip_if_not_installed("APCalign")
  mockery::stub(get_afa_static, ".ausplots_api", fixture("afa_static.json"))
  afa <- get_afa_static()
  input <- data.frame(
    herbarium_determination = c("Acacia aneura", "Oxalis corniculata"),
    stringsAsFactors = FALSE
  )
  result <- invasion_info(ausplots = input, afa_static = afa)
  expect_true(is.data.frame(result))
  expect_true("invasion_ACT" %in% names(result))
  expect_true(ncol(result) > ncol(input))
})

# What it tests: result has at least as many rows as the input after filtering
# Input:         2-row ausplots data.frame with valid binomial names
# Expected:      nrow(result) >= 2
test_that("invasion_info: result has at least as many rows as the input", {
  skip_if_not_installed("APCalign")
  mockery::stub(get_afa_static, ".ausplots_api", fixture("afa_static.json"))
  afa <- get_afa_static()
  input <- data.frame(
    herbarium_determination = c("Acacia aneura", "Oxalis corniculata"),
    stringsAsFactors = FALSE
  )
  result <- invasion_info(ausplots = input, afa_static = afa)
  expect_gte(nrow(result), 2L)
})

# What it tests: keep_all = FALSE returns a reduced column set
# Input:         same 2-row input; keep_all = FALSE
# Expected:      result has fewer columns than with keep_all = TRUE
test_that("invasion_info: keep_all = FALSE returns fewer columns than keep_all = TRUE", {
  skip_if_not_installed("APCalign")
  mockery::stub(get_afa_static, ".ausplots_api", fixture("afa_static.json"))
  afa <- get_afa_static()
  input <- data.frame(
    herbarium_determination = c("Acacia aneura", "Oxalis corniculata"),
    stringsAsFactors = FALSE
  )
  result_all  <- invasion_info(ausplots = input, afa_static = afa, keep_all = TRUE)
  result_reduced <- invasion_info(ausplots = input, afa_static = afa, keep_all = FALSE)
  expect_lt(ncol(result_reduced), ncol(result_all))
})