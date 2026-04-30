library(testthat)

# ══════════════════════════════════════════════════════════════════════════════
# growth_form_table()
#
# Builds a sites × growth-form matrix from point-intercept data.
# m_kind controls the metric; species_name controls the taxon source column.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — build a minimal veg.PI data frame for testing
# ══════════════════════════════════════════════════════════════════════════════

make_veg_pi_gft <- function() {
  data.frame(
    site_unique             = c("SITE001", "SITE001", "SITE001", "SITE002", "SITE002"),
    growth_form             = c("Tree/Palm", "Tree/Palm", "Shrub",  "Tree/Palm", "Shrub"),
    standardised_name       = c("Acacia aneura", "Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura", "Eucalyptus camaldulensis"),
    herbarium_determination = c("Acacia aneura", "Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura", "Eucalyptus camaldulensis"),
    genus_species           = c("Acacia aneura", "Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura", "Eucalyptus camaldulensis"),
    hits_unique             = c("T1P1", "T1P2", "T1P3", "T1P1", "T1P2"),
    in_canopy_sky           = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors        = FALSE
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# growth_form_table() — HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: m_kind='PA', species_name='SN' returns a data.frame
# Input:         minimal veg.PI with 2 sites and 2 growth forms
# Expected:      data.frame; growth form values appear as column names (dots replace slashes)
test_that("growth_form_table: m_kind='PA' species_name='SN' returns a data.frame", {
  vp <- make_veg_pi_gft()
  result <- growth_form_table(vp, m_kind = "PA", species_name = "SN")
  expect_true(is.data.frame(result))
  expect_true(any(c("Tree.Palm", "Shrub") %in% names(result)))
})

# What it tests: result has one row per unique site_unique
# Input:         2 distinct site_unique values in veg.PI
# Expected:      nrow == 2
test_that("growth_form_table: returns one row per site_unique", {
  vp <- make_veg_pi_gft()
  result <- growth_form_table(vp, m_kind = "PA", species_name = "SN")
  expect_equal(nrow(result), 2L)
})

# What it tests: site_unique values appear as row names
# Input:         veg.PI with SITE001 and SITE002
# Expected:      rownames contains SITE001 and SITE002
test_that("growth_form_table: site_unique values appear as row names", {
  vp <- make_veg_pi_gft()
  result <- growth_form_table(vp, m_kind = "PA", species_name = "SN")
  expect_true(all(c("SITE001", "SITE002") %in% rownames(result)))
})

# What it tests: absent growth form receives a 0 value
# Input:         SITE002 has only Tree/Palm (repeated); Shrub is absent
# Expected:      result["SITE002", "Shrub"] == 0
test_that("growth_form_table: absent growth form receives 0", {
  vp <- data.frame(
    site_unique             = c("SITE001", "SITE001", "SITE001", "SITE002", "SITE002"),
    growth_form             = c("Tree/Palm", "Tree/Palm", "Shrub", "Tree/Palm", "Tree/Palm"),
    standardised_name       = c("Acacia aneura", "Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura", "Acacia aneura"),
    herbarium_determination = c("Acacia aneura", "Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura", "Acacia aneura"),
    genus_species           = c("Acacia aneura", "Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura", "Acacia aneura"),
    hits_unique             = c("T1P1", "T1P2", "T1P3", "T1P1", "T1P2"),
    in_canopy_sky           = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors        = FALSE
  )
  result <- growth_form_table(vp, m_kind = "PA", species_name = "SN")
  expect_equal(as.numeric(result["SITE002", "Shrub"]), 0)
})

# What it tests: m_kind='PA', species_name='GS' returns a data.frame
# Input:         same minimal veg.PI
# Expected:      data.frame with growth form columns present
test_that("growth_form_table: m_kind='PA' species_name='GS' returns a data.frame", {
  vp <- make_veg_pi_gft()
  result <- growth_form_table(vp, m_kind = "PA", species_name = "GS")
  expect_true(is.data.frame(result))
})

# ══════════════════════════════════════════════════════════════════════════════
# growth_form_table() — ERROR HANDLING
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: non-data.frame veg.PI is caught and wrapped
# Input:         veg.PI = "not a df" (character)
# Expected:      error containing "Failed to build growth form table:"
test_that("growth_form_table: non-data.frame input throws wrapped error", {
  expect_error(
    growth_form_table("not a df", m_kind = "PA"),
    "Failed to build growth form table:"
  )
})

# What it tests: wrapped error preserves the original cause
# Input:         veg.PI = "not a df" (character)
# Expected:      error message also contains "veg.PI must be a data.frame"
test_that("growth_form_table: wrapped error preserves original cause", {
  expect_error(
    growth_form_table("not a df", m_kind = "PA"),
    "veg.PI must be a data.frame"
  )
})

# What it tests: missing required columns (growth_form) throws wrapped error
# Input:         veg.PI missing growth_form column
# Expected:      error containing "Failed to build growth form table:"
test_that("growth_form_table: missing required column throws wrapped error", {
  vp <- make_veg_pi_gft()
  vp$growth_form <- NULL
  expect_error(
    growth_form_table(vp, m_kind = "PA"),
    "Failed to build growth form table:"
  )
})

# What it tests: missing m_kind is caught and wrapped
# Input:         valid veg.PI; m_kind not supplied
# Expected:      error containing "Failed to build growth form table:"
test_that("growth_form_table: missing m_kind throws wrapped error", {
  vp <- make_veg_pi_gft()
  expect_error(
    growth_form_table(vp),
    "Failed to build growth form table:"
  )
})

# What it tests: wrapped error has call.=FALSE
# Input:         veg.PI = NULL
# Expected:      error is raised; err$call is NULL
test_that("growth_form_table: wrapped error has call.=FALSE", {
  err <- tryCatch(growth_form_table(NULL, m_kind = "PA"), error = function(e) e)
  expect_null(err$call)
})
