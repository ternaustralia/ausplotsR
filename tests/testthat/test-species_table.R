library(testthat)

# ══════════════════════════════════════════════════════════════════════════════
# species_table()
#
# Builds a sites × species matrix from point-intercept data.
# m_kind controls the metric; species_name controls the taxon source column.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — build a minimal veg.PI data frame for testing
# ══════════════════════════════════════════════════════════════════════════════

make_veg_pi_st <- function() {
  data.frame(
    site_unique             = c("SITE001", "SITE001", "SITE002"),
    standardised_name       = c("Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura"),
    herbarium_determination = c("Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura"),
    genus_species           = c("Acacia aneura", "Eucalyptus camaldulensis", "Acacia aneura"),
    hits_unique             = c("T1P1", "T1P2", "T1P1"),
    in_canopy_sky           = c(FALSE, FALSE, FALSE),
    transect                = c("T1", "T1", "T1"),
    genus                   = c("Acacia", "Eucalyptus", "Acacia"),
    stringsAsFactors        = FALSE
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# species_table() — HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: m_kind='PA', species_name='SN' returns a data.frame
# Input:         minimal veg.PI with 2 sites and 2 species
# Expected:      data.frame; 2 site rows; Acacia aneura present as a column
test_that("species_table: m_kind='PA' species_name='SN' returns a data.frame", {
  vp <- make_veg_pi_st()
  result <- species_table(vp, m_kind = "PA", species_name = "SN")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2L)
  expect_true("Acacia.aneura" %in% names(result))
})

# What it tests: m_kind='PA', species_name='HD' returns a data.frame
# Input:         minimal veg.PI with 2 sites and 2 herbarium names
# Expected:      data.frame; contains Acacia.aneura column
test_that("species_table: m_kind='PA' species_name='HD' returns a data.frame", {
  vp <- make_veg_pi_st()
  result <- species_table(vp, m_kind = "PA", species_name = "HD")
  expect_true(is.data.frame(result))
  expect_true("Acacia.aneura" %in% names(result))
})

# What it tests: m_kind='PA', species_name='GS' returns a data.frame
# Input:         minimal veg.PI with genus_species column
# Expected:      data.frame with genus_species values as columns
test_that("species_table: m_kind='PA' species_name='GS' returns a data.frame", {
  vp <- make_veg_pi_st()
  result <- species_table(vp, m_kind = "PA", species_name = "GS")
  expect_true(is.data.frame(result))
})

# What it tests: site_unique values appear as row names
# Input:         minimal veg.PI with SITE001 and SITE002
# Expected:      rownames contains SITE001 and SITE002
test_that("species_table: site_unique values appear as row names", {
  vp <- make_veg_pi_st()
  result <- species_table(vp, m_kind = "PA", species_name = "SN")
  expect_true(all(c("SITE001", "SITE002") %in% rownames(result)))
})

# What it tests: absent species receive a 0 value
# Input:         SITE002 has only Acacia aneura; Eucalyptus is absent
# Expected:      result["SITE002", "Eucalyptus camaldulensis"] == 0
test_that("species_table: absent species receive a 0 value", {
  vp <- make_veg_pi_st()
  result <- species_table(vp, m_kind = "PA", species_name = "SN")
  expect_equal(as.numeric(result["SITE002", "Eucalyptus.camaldulensis"]), 0)
})

# ══════════════════════════════════════════════════════════════════════════════
# species_table() — ERROR HANDLING
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: non-data.frame veg.PI is caught and wrapped
# Input:         veg.PI = "not a df" (character)
# Expected:      error containing "Failed to build species table:"
test_that("species_table: non-data.frame input throws wrapped error", {
  expect_error(
    species_table("not a df", m_kind = "PA"),
    "Failed to build species table:"
  )
})

# What it tests: wrapped error preserves the original cause
# Input:         veg.PI = "not a df" (character)
# Expected:      error message also contains "veg.PI must be a data.frame"
test_that("species_table: wrapped error preserves original cause", {
  expect_error(
    species_table("not a df", m_kind = "PA"),
    "veg.PI must be a data.frame"
  )
})

# What it tests: missing m_kind is caught and wrapped
# Input:         valid veg.PI; m_kind not supplied
# Expected:      error containing "Failed to build species table:"
test_that("species_table: missing m_kind throws wrapped error", {
  vp <- make_veg_pi_st()
  expect_error(
    species_table(vp),
    "Failed to build species table:"
  )
})

# What it tests: wrapped error has call.=FALSE (no call stack trace appended)
# Input:         veg.PI = NULL
# Expected:      error is raised; err$call is NULL
test_that("species_table: wrapped error has call.=FALSE", {
  err <- tryCatch(species_table(NULL, m_kind = "PA"), error = function(e) e)
  expect_null(err$call)
})
