library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# ausplots_trajectory()
#
# Generates PCoA and/or diversity trajectory plots across multiple site visits.
# Writes output to a PDF file. Uses vegan, species_table, and
# trajectoryPCoA_ausplots internally.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# HELPER — minimal multi-visit ausplots object
#
# Two sites (SITE_A, SITE_B) each visited 3 times.
# veg.PI has 5 point-intercept rows per visit with 2 species + 1 NA.
# ══════════════════════════════════════════════════════════════════════════════

make_traj_object <- function() {
  site_uniques <- c(
    "SITE_A-1", "SITE_A-2", "SITE_A-3",
    "SITE_B-1", "SITE_B-2", "SITE_B-3"
  )

  # Each site_unique gets a distinct species composition so vegdist is
  # non-degenerate and cmdscale() can produce a valid PCoA.
  # 5 hits per site_unique; hit IDs must be globally unique.
  spp <- list(
    "SITE_A-1" = c("Acacia aneura",            "Acacia aneura",
                   "Acacia aneura",            "Acacia aneura",            "Acacia aneura"),
    "SITE_A-2" = c("Acacia aneura",            "Eucalyptus camaldulensis",
                   "Acacia aneura",            "Eucalyptus camaldulensis", NA),
    "SITE_A-3" = c("Eucalyptus camaldulensis", "Eucalyptus camaldulensis",
                   "Eucalyptus camaldulensis", "Eucalyptus camaldulensis", "Eucalyptus camaldulensis"),
    "SITE_B-1" = c("Acacia aneura",            "Acacia aneura",
                   "Callitris glaucophylla",   "Callitris glaucophylla",   "Acacia aneura"),
    "SITE_B-2" = c("Callitris glaucophylla",   "Callitris glaucophylla",
                   "Eucalyptus camaldulensis", "Acacia aneura",            NA),
    "SITE_B-3" = c("Callitris glaucophylla",   "Callitris glaucophylla",
                   "Callitris glaucophylla",   "Callitris glaucophylla",   "Callitris glaucophylla")
  )

  veg_rows <- do.call(rbind, lapply(seq_along(site_uniques), function(i) {
    su   <- site_uniques[i]
    data.frame(
      site_unique       = su,
      hits_unique       = paste0(su, "_H", seq_len(5)),
      standardised_name = spp[[su]],
      in_canopy_sky     = rep(FALSE, 5),
      growth_form       = rep("Tree/Palm", 5),
      stringsAsFactors  = FALSE
    )
  }))

  list(
    site.info = data.frame(
      site_location_name = rep(c("SITE_A", "SITE_B"), each = 3),
      site_unique        = site_uniques,
      visit_number       = rep(1:3, 2),
      visit_date         = rep("2020-01-01", 6),
      stringsAsFactors   = FALSE
    ),
    veg.PI = veg_rows
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# ausplots_trajectory() — HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: default choices (PCoA + diversity) writes a PDF file
# Input:         valid multi-visit ausplots object; outfile = tempfile
# Expected:      file exists after call; no error thrown
test_that("ausplots_trajectory: default choices writes a PDF file", {
  obj  <- make_traj_object()
  out  <- tempfile(fileext = ".pdf")
  expect_no_error(
    suppressMessages(ausplots_trajectory(obj, outfile = out))
  )
  expect_true(file.exists(out))
  unlink(out)
})

# What it tests: choices='PCoA' only writes a PDF without diversity plots
# Input:         valid multi-visit ausplots object; choices = 'PCoA'
# Expected:      file exists; no error
test_that("ausplots_trajectory: choices='PCoA' only writes a PDF", {
  obj  <- make_traj_object()
  out  <- tempfile(fileext = ".pdf")
  expect_no_error(
    suppressMessages(ausplots_trajectory(obj, choices = "PCoA", outfile = out))
  )
  expect_true(file.exists(out))
  unlink(out)
})

# What it tests: choices='diversity' with pre-computed community.indices skips species_table
# Input:         ausplots object augmented with community.indices; choices = 'diversity'
# Expected:      file exists; no error
test_that("ausplots_trajectory: choices='diversity' with community.indices writes a PDF", {
  obj <- make_traj_object()
  obj$community.indices <- data.frame(
    site_unique       = obj$site.info$site_unique,
    Shannon_diversity = runif(6, 0.3, 1.5),
    Species_richness  = as.integer(rep(2, 6)),
    stringsAsFactors  = FALSE
  )
  out <- tempfile(fileext = ".pdf")
  expect_no_error(
    suppressMessages(
      ausplots_trajectory(obj, choices = "diversity", outfile = out)
    )
  )
  expect_true(file.exists(out))
  unlink(out)
})

# What it tests: plot_select filters to named sites without error
# Input:         valid object; plot_select = "SITE_A"
# Expected:      file exists; no error
test_that("ausplots_trajectory: plot_select filters to a valid site", {
  obj <- make_traj_object()
  out <- tempfile(fileext = ".pdf")
  expect_no_error(
    suppressMessages(
      ausplots_trajectory(obj, choices = "PCoA",
                          plot_select = "SITE_A", outfile = out)
    )
  )
  expect_true(file.exists(out))
  unlink(out)
})

# ══════════════════════════════════════════════════════════════════════════════
# ausplots_trajectory() — ERROR HANDLING (tryCatch wraps entire body)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: missing my.ausplots.object is caught and wrapped
# Input:         no arguments
# Expected:      error message contains "Failed to generate trajectory plots"
#                and "my.ausplots.object must be specified"
test_that("ausplots_trajectory: missing my.ausplots.object is caught and wrapped", {
  expect_error(
    ausplots_trajectory(),
    "Failed to generate trajectory plots"
  )
  expect_error(
    ausplots_trajectory(),
    "my.ausplots.object must be specified"
  )
})

# What it tests: non-list my.ausplots.object is caught and wrapped
# Input:         my.ausplots.object = "not a list"
# Expected:      error message contains "Failed to generate trajectory plots"
#                and "must be a list"
test_that("ausplots_trajectory: non-list my.ausplots.object is caught and wrapped", {
  expect_error(
    ausplots_trajectory("not a list"),
    "Failed to generate trajectory plots"
  )
  expect_error(
    ausplots_trajectory("not a list"),
    "must be a list"
  )
})

# What it tests: plot_select that is not a character vector is caught and wrapped
# Input:         valid object; plot_select = 42
# Expected:      error message contains "Failed to generate trajectory plots"
test_that("ausplots_trajectory: non-character plot_select is caught and wrapped", {
  obj <- make_traj_object()
  expect_error(
    ausplots_trajectory(obj, plot_select = 42),
    "Failed to generate trajectory plots"
  )
})

# What it tests: plot_select with no matching site names is caught and wrapped
# Input:         valid object; plot_select = "DOES_NOT_EXIST"
# Expected:      error message contains "Failed to generate trajectory plots"
#                and "No entries in plot_select were found"
test_that("ausplots_trajectory: plot_select with no matches is caught and wrapped", {
  obj <- make_traj_object()
  expect_error(
    ausplots_trajectory(obj, plot_select = "DOES_NOT_EXIST"),
    "Failed to generate trajectory plots"
  )
  expect_error(
    ausplots_trajectory(obj, plot_select = "DOES_NOT_EXIST"),
    "No entries in plot_select were found"
  )
})

# What it tests: invalid choices value is caught and wrapped
# Input:         valid object; choices = "bad_choice"
# Expected:      error message contains "Failed to generate trajectory plots"
#                and "choices must include at least one of PCoA or diversity"
test_that("ausplots_trajectory: invalid choices is caught and wrapped", {
  obj <- make_traj_object()
  expect_error(
    ausplots_trajectory(obj, choices = "bad_choice"),
    "Failed to generate trajectory plots"
  )
  expect_error(
    ausplots_trajectory(obj, choices = "bad_choice"),
    "choices must include at least one of PCoA or diversity"
  )
})

# What it tests: no sites meeting min.revisits threshold is caught and wrapped
# Input:         valid object; min.revisits = 10 (no site has 10 visits)
# Expected:      error message contains "Failed to generate trajectory plots"
#                and "No sites have at least"
test_that("ausplots_trajectory: insufficient visits is caught and wrapped", {
  obj <- make_traj_object()
  expect_error(
    ausplots_trajectory(obj, min.revisits = 10),
    "Failed to generate trajectory plots"
  )
  expect_error(
    ausplots_trajectory(obj, min.revisits = 10),
    "No sites have at least"
  )
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid, .sentry_capture_exception is called
# Input:         "not a list"; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("ausplots_trajectory: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- ausplots_trajectory
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn("not a list"))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid trajectory object; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("ausplots_trajectory: capture_message 'info' on successful plot generation", {
  captured_levels <- character(0)
  fn <- ausplots_trajectory
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  fn(make_traj_object())
  expect_true("info" %in% captured_levels)
})
