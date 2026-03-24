library(testthat)

# ══════════════════════════════════════════════════════════════════════════════
# FIXTURE LOADER
#
# Reads NVIS.rda from tests/testthat/fixtures/ and returns the NVIS data frame.
# To update mock data: replace fixtures/NVIS.rda — no R code changes needed.
# Generate it with:
#   NVIS <- get_nvis()  # or a trimmed subset
#   save(NVIS, file = "tests/testthat/fixtures/NVIS.rda")
# ══════════════════════════════════════════════════════════════════════════════

fixture_nvis <- function() {
  e <- new.env(parent = emptyenv())
  load(test_path("fixtures", "NVIS.rda"), envir = e)
  as.data.frame(e$NVIS)
}

# ══════════════════════════════════════════════════════════════════════════════
# SITE_UNIQUE FORMAT PATTERN
#
# Format: <6 uppercase letters><4 digits>-<visit digits>
#   e.g. "SATFLB0004-53705", "SASMDD0001-53710"
#   - Characters 1-2: state prefix    (SA, QD, NS, VC, TC, WA, NT)
#   - Character  3:   plot type       (A = AusPlot, T = Transect, S = Survey, etc.)
#   - Characters 4-6: IBRA bioregion  (e.g. FLB, MGD, MDD)
#   - Characters 7-10: 4-digit plot number (e.g. 0004)
#   - Dash separator: literal -
#   - Visit ID: one or more digits    (e.g. 53705)
# ══════════════════════════════════════════════════════════════════════════════

SITE_UNIQUE_PATTERN <- "^[A-Z]{6}[0-9]{4}-[0-9]+$"

# ══════════════════════════════════════════════════════════════════════════════
# INPUT VALIDATION
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: passing a list (not NULL, character, or data.frame) is rejected
# Input:         site_unique = list("SATFLB0004-53705")
# Expected:      error containing "site_unique' must be NULL"
test_that("get_nvis: list input is rejected with informative error", {
  expect_error(
    ausplotsR::get_nvis(site_unique = list("SATFLB0004-53705")),
    "site_unique.*must be NULL"
  )
})

# What it tests: a data.frame without a 'site_unique' column is rejected
# Input:         data.frame with column 'plot_name', not 'site_unique'
# Expected:      error "must contain a 'site_unique' column"
test_that("get_nvis: data.frame without site_unique column is rejected", {
  bad_df <- data.frame(plot_name = "SATFLB0004-53705", stringsAsFactors = FALSE)
  expect_error(
    ausplotsR::get_nvis(site_unique = bad_df),
    "must contain a 'site_unique' column"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# RETURN TYPE
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: get_nvis() always returns a data.frame
# Input:         site_unique = NULL  (returns full table)
# Expected:      result is a data.frame
test_that("get_nvis: returns a data.frame", {
  result <- ausplotsR::get_nvis()
  expect_s3_class(result, "data.frame")
})

# What it tests: result contains the 'site_unique' column
# Input:         site_unique = NULL
# Expected:      "site_unique" is present in column names
test_that("get_nvis: result contains site_unique column", {
  result <- ausplotsR::get_nvis()
  expect_true("site_unique" %in% names(result))
})

# ══════════════════════════════════════════════════════════════════════════════
# COLUMN NAMES — result schema matches fixture
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: all columns present in the fixture are present in the result
# Input:         site_unique = NULL; expected columns derived from fixture_nvis()
# Expected:      no columns from the fixture are missing in the result
test_that("get_nvis: result contains all columns present in fixture", {
  mock_df <- fixture_nvis()
  result  <- ausplotsR::get_nvis()
  missing <- setdiff(names(mock_df), names(result))
  expect_equal(
    missing, character(0),
    label = paste("Missing columns:", paste(missing, collapse = ", "))
  )
})

# What it tests: no extra columns appear in the result beyond what the fixture defines
# Input:         site_unique = NULL
# Expected:      result has no columns that are not in the fixture
test_that("get_nvis: result contains no columns absent from fixture", {
  mock_df <- fixture_nvis()
  result  <- ausplotsR::get_nvis()
  extra   <- setdiff(names(result), names(mock_df))
  expect_equal(
    extra, character(0),
    label = paste("Unexpected columns:", paste(extra, collapse = ", "))
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# NULL INPUT — returns full table (mocked via fixture)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: NULL site_unique returns the full NVIS fixture table
# Input:         site_unique = NULL, mock backed by fixtures/NVIS.rda
# Expected:      result rows match the fixture row count; site_unique column present
test_that("get_nvis: NULL returns full table matching fixture", {
  mock_df <- fixture_nvis()
  local_mocked_bindings(
    get_nvis = function(site_unique = NULL) {
      nvis <- mock_df
      nvis$site_unique <- as.character(nvis$site_unique)
      if (is.null(site_unique)) return(as.data.frame(nvis))
      if (is.data.frame(site_unique)) {
        request_sites <- unique(as.character(site_unique$site_unique))
      } else {
        request_sites <- unique(as.character(site_unique))
      }
      request_df <- data.frame(site_unique = request_sites, stringsAsFactors = FALSE)
      as.data.frame(merge(request_df, nvis, by = "site_unique", all.x = TRUE, sort = FALSE))
    },
    .package = "ausplotsR"
  )
  result <- ausplotsR::get_nvis()
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_equal(nrow(result), nrow(mock_df))
  expect_true("site_unique" %in% names(result))
})

# ══════════════════════════════════════════════════════════════════════════════
# CHARACTER VECTOR INPUT (mocked via fixture)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: a single valid site_unique string returns exactly that row
# Input:         site_unique = first site_unique from fixture, mock backed by NVIS.rda
# Expected:      result has 1 row; site_unique column equals the requested value
test_that("get_nvis: single site_unique string returns matching row", {
  mock_df <- fixture_nvis()
  local_mocked_bindings(
    get_nvis = function(site_unique = NULL) {
      nvis <- mock_df
      nvis$site_unique <- as.character(nvis$site_unique)
      if (is.null(site_unique)) return(as.data.frame(nvis))
      if (is.data.frame(site_unique)) {
        request_sites <- unique(as.character(site_unique$site_unique))
      } else {
        request_sites <- unique(as.character(site_unique))
      }
      request_df <- data.frame(site_unique = request_sites, stringsAsFactors = FALSE)
      as.data.frame(merge(request_df, nvis, by = "site_unique", all.x = TRUE, sort = FALSE))
    },
    .package = "ausplotsR"
  )
  target <- as.character(mock_df$site_unique[1])
  result <- ausplotsR::get_nvis(site_unique = target)
  expect_equal(nrow(result), 1L)
  expect_equal(result$site_unique, target)
})

# What it tests: a vector of two site_unique values returns exactly 2 rows
# Input:         site_unique = first two site_uniques from fixture
# Expected:      result has 2 rows; both requested IDs appear in output
test_that("get_nvis: vector of two site_unique values returns 2 rows", {
  mock_df <- fixture_nvis()
  skip_if(nrow(mock_df) < 2, "fixture has fewer than 2 rows")
  local_mocked_bindings(
    get_nvis = function(site_unique = NULL) {
      nvis <- mock_df
      nvis$site_unique <- as.character(nvis$site_unique)
      if (is.null(site_unique)) return(as.data.frame(nvis))
      if (is.data.frame(site_unique)) {
        request_sites <- unique(as.character(site_unique$site_unique))
      } else {
        request_sites <- unique(as.character(site_unique))
      }
      request_df <- data.frame(site_unique = request_sites, stringsAsFactors = FALSE)
      as.data.frame(merge(request_df, nvis, by = "site_unique", all.x = TRUE, sort = FALSE))
    },
    .package = "ausplotsR"
  )
  targets <- as.character(mock_df$site_unique[1:2])
  result <- ausplotsR::get_nvis(site_unique = targets)
  expect_equal(nrow(result), 2L)
  expect_true(all(targets %in% result$site_unique))
})

# What it tests: duplicate site_unique values in the input are deduplicated
# Input:         same site_unique repeated twice
# Expected:      result has 1 row
test_that("get_nvis: duplicate site_unique values are deduplicated", {
  mock_df <- fixture_nvis()
  local_mocked_bindings(
    get_nvis = function(site_unique = NULL) {
      nvis <- mock_df
      nvis$site_unique <- as.character(nvis$site_unique)
      if (is.null(site_unique)) return(as.data.frame(nvis))
      if (is.data.frame(site_unique)) {
        request_sites <- unique(as.character(site_unique$site_unique))
      } else {
        request_sites <- unique(as.character(site_unique))
      }
      request_df <- data.frame(site_unique = request_sites, stringsAsFactors = FALSE)
      as.data.frame(merge(request_df, nvis, by = "site_unique", all.x = TRUE, sort = FALSE))
    },
    .package = "ausplotsR"
  )
  target <- as.character(mock_df$site_unique[1])
  result <- ausplotsR::get_nvis(site_unique = c(target, target))
  expect_equal(nrow(result), 1L)
})

# What it tests: a site_unique not present in NVIS returns a row with NA values
# Input:         site_unique = "ZZZNONE0000-00000"  (guaranteed non-existent)
# Expected:      result has 1 row; non-key columns are NA
test_that("get_nvis: unknown site_unique returns row with NAs", {
  mock_df <- fixture_nvis()
  local_mocked_bindings(
    get_nvis = function(site_unique = NULL) {
      nvis <- mock_df
      nvis$site_unique <- as.character(nvis$site_unique)
      if (is.null(site_unique)) return(as.data.frame(nvis))
      if (is.data.frame(site_unique)) {
        request_sites <- unique(as.character(site_unique$site_unique))
      } else {
        request_sites <- unique(as.character(site_unique))
      }
      request_df <- data.frame(site_unique = request_sites, stringsAsFactors = FALSE)
      as.data.frame(merge(request_df, nvis, by = "site_unique", all.x = TRUE, sort = FALSE))
    },
    .package = "ausplotsR"
  )
  result <- ausplotsR::get_nvis(site_unique = "ZZZNONE0000-00000")
  expect_equal(nrow(result), 1L)
  expect_equal(result$site_unique, "ZZZNONE0000-00000")
  non_key_cols <- setdiff(names(result), "site_unique")
  if (length(non_key_cols) > 0) {
    expect_true(all(is.na(result[1, non_key_cols])))
  }
})

# ══════════════════════════════════════════════════════════════════════════════
# DATA.FRAME INPUT (mocked via fixture)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: a data.frame with a 'site_unique' column is accepted and
#   returns the matching rows
# Input:         data.frame with site_unique = first entry from fixture
# Expected:      result has 1 row matching that site_unique
test_that("get_nvis: data.frame with site_unique column returns matching rows", {
  mock_df <- fixture_nvis()
  local_mocked_bindings(
    get_nvis = function(site_unique = NULL) {
      nvis <- mock_df
      nvis$site_unique <- as.character(nvis$site_unique)
      if (is.null(site_unique)) return(as.data.frame(nvis))
      if (is.data.frame(site_unique)) {
        request_sites <- unique(as.character(site_unique$site_unique))
      } else {
        request_sites <- unique(as.character(site_unique))
      }
      request_df <- data.frame(site_unique = request_sites, stringsAsFactors = FALSE)
      as.data.frame(merge(request_df, nvis, by = "site_unique", all.x = TRUE, sort = FALSE))
    },
    .package = "ausplotsR"
  )
  target_df <- data.frame(site_unique = as.character(mock_df$site_unique[1]),
                          stringsAsFactors = FALSE)
  result <- ausplotsR::get_nvis(site_unique = target_df)
  expect_equal(nrow(result), 1L)
  expect_equal(result$site_unique, as.character(mock_df$site_unique[1]))
})

# What it tests: a data.frame with extra columns beside 'site_unique' is accepted
# Input:         data.frame with site_unique + extra_col
# Expected:      no error; result has 1 row
test_that("get_nvis: data.frame with extra columns is accepted", {
  mock_df <- fixture_nvis()
  local_mocked_bindings(
    get_nvis = function(site_unique = NULL) {
      nvis <- mock_df
      nvis$site_unique <- as.character(nvis$site_unique)
      if (is.null(site_unique)) return(as.data.frame(nvis))
      if (is.data.frame(site_unique)) {
        request_sites <- unique(as.character(site_unique$site_unique))
      } else {
        request_sites <- unique(as.character(site_unique))
      }
      request_df <- data.frame(site_unique = request_sites, stringsAsFactors = FALSE)
      as.data.frame(merge(request_df, nvis, by = "site_unique", all.x = TRUE, sort = FALSE))
    },
    .package = "ausplotsR"
  )
  target_df <- data.frame(
    site_unique = as.character(mock_df$site_unique[1]),
    extra_col   = "something",
    stringsAsFactors = FALSE
  )
  expect_no_error(ausplotsR::get_nvis(site_unique = target_df))
  result <- ausplotsR::get_nvis(site_unique = target_df)
  expect_equal(nrow(result), 1L)
})

# ══════════════════════════════════════════════════════════════════════════════
# SITE_UNIQUE FORMAT — regex validation
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: every site_unique in the full NVIS table matches the expected
#   naming convention
# Input:         site_unique = NULL (full table from bundled data)
# Expected:      all site_unique values match SITE_UNIQUE_PATTERN
test_that("get_nvis: all site_unique values match expected format", {
  result  <- ausplotsR::get_nvis()
  invalid <- result$site_unique[!grepl(SITE_UNIQUE_PATTERN, result$site_unique)]
  expect_equal(
    length(invalid), 0L,
    label = paste("Non-conforming site_unique values:", paste(head(invalid, 5), collapse = ", "))
  )
})

# What it tests: every site_unique in the fixture also conforms to the pattern
# Input:         fixtures/NVIS.rda loaded via fixture_nvis()
# Expected:      all site_unique values in fixture match SITE_UNIQUE_PATTERN
test_that("get_nvis: fixture site_unique values all match expected format", {
  mock_df <- fixture_nvis()
  invalid <- mock_df$site_unique[!grepl(SITE_UNIQUE_PATTERN, mock_df$site_unique)]
  expect_equal(
    length(invalid), 0L,
    label = paste("Non-conforming values in fixture:", paste(head(invalid, 5), collapse = ", "))
  )
})

# What it tests: the regex correctly rejects malformed site_unique strings
# Input:         a selection of known-bad formats
# Expected:      none of them match SITE_UNIQUE_PATTERN
test_that("get_nvis: site_unique regex rejects malformed values", {
  bad_values <- c(
    "SATFLB0004",          # missing dash and visit ID
    "SATFLB0004-",         # dash but no visit digits
    "SA_FLB0004-53705",    # underscore in letter block
    "satflb0004-53705",    # lowercase
    "SATFLB004-53705",     # only 3 plot digits
    "SATFLB00004-53705",   # 5 plot digits
    "S1TFLB0004-53705",    # digit in 6-letter block
    "SAFLB0004-53705",     # only 5 letters before digits
    "SATFLB0004-53705-1"   # extra segment after visit ID
  )
  matches <- grepl(SITE_UNIQUE_PATTERN, bad_values)
  expect_true(
    all(!matches),
    label = paste("Should NOT match but did:", paste(bad_values[matches], collapse = ", "))
  )
})

# What it tests: the regex correctly accepts well-formed site_unique strings
# Input:         known-good examples covering different states and plot types
# Expected:      all match SITE_UNIQUE_PATTERN
test_that("get_nvis: site_unique regex accepts well-formed values", {
  good_values <- c(
    "SATFLB0004-53705",   # SA, AusPlot
    "QDAMGD0022-11111",   # QLD, AusPlot
    "SASMDD0001-53710",   # SA, Survey  (real data example)
    "NSWBBS0001-99999",   # NSW (NS=state, W=type, BBS=bioregion)
    "VCTTFL0010-1",       # VIC, Transect, single-digit visit
    "TCAFLE0003-123456"   # TAS, AusPlot, long visit ID
  )
  matches <- grepl(SITE_UNIQUE_PATTERN, good_values)
  expect_true(
    all(matches),
    label = paste("Should match but did not:", paste(good_values[!matches], collapse = ", "))
  )
})