library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# optim_species()
#
# Runs one or more site-selection optimisation algorithms on a species-by-sites
# matrix. Has two explicit stop() calls for bad `start` and `plot_name` args.
# Uses try() internally for plot_opt(). No API calls.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# optim_species() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: invalid `start` argument is caught and wrapped
# Input:         valid matrix; start = "bad"
# Expected:      stop() with "Failed to optimise species accumulation:" prefix
test_that("optim_species: invalid start is caught and wrapped", {
  m <- matrix(c(1, 0, 0, 1), nrow = 2,
              dimnames = list(c("A", "B"), c("sp1", "sp2")))
  expect_error(
    optim_species(m, n.plt = 2, start = "bad",
                  richness = TRUE, RRR = FALSE, CWE = FALSE,
                  shannon = FALSE, simpson = FALSE, simpson_beta = FALSE,
                  frequent = FALSE, random = FALSE, plot = FALSE, verbose = FALSE),
    "Failed to optimise species accumulation:"
  )
})

# What it tests: original error message survives inside the wrapped message
# Input:         invalid start
# Expected:      error contains the original stop() cause
test_that("optim_species: wrapped error preserves original cause", {
  m <- matrix(c(1, 0, 0, 1), nrow = 2,
              dimnames = list(c("A", "B"), c("sp1", "sp2")))
  expect_error(
    optim_species(m, n.plt = 2, start = "bad",
                  richness = TRUE, RRR = FALSE, CWE = FALSE,
                  shannon = FALSE, simpson = FALSE, simpson_beta = FALSE,
                  frequent = FALSE, random = FALSE, plot = FALSE, verbose = FALSE),
    "Argument 'start' must be"
  )
})

# What it tests: invalid plot_name (not in matrix rownames) is caught and wrapped
# Input:         valid matrix; plot_name = "NONEXISTENT"
# Expected:      stop() with "Failed to optimise species accumulation:" prefix
test_that("optim_species: invalid plot_name is caught and wrapped", {
  m <- matrix(c(1, 0, 0, 1), nrow = 2,
              dimnames = list(c("A", "B"), c("sp1", "sp2")))
  expect_error(
    optim_species(m, n.plt = 2, plot_name = "NONEXISTENT",
                  richness = TRUE, RRR = FALSE, CWE = FALSE,
                  shannon = FALSE, simpson = FALSE, simpson_beta = FALSE,
                  frequent = FALSE, random = FALSE, plot = FALSE, verbose = FALSE),
    "Failed to optimise species accumulation:"
  )
})

# What it tests: NULL matrix input is caught and wrapped
# Input:         NULL
# Expected:      stop() with "Failed to optimise species accumulation:" prefix
test_that("optim_species: NULL input is caught and wrapped", {
  expect_error(
    optim_species(NULL),
    "Failed to optimise species accumulation:"
  )
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid, .sentry_capture_exception is called
# Input:         NULL; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("optim_species: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- optim_species
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn(NULL))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid species list; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("optim_species: capture_message 'info' on successful optimisation", {
  captured_levels <- character(0)
  fn <- optim_species
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  sp <- matrix(
    c(1, 0, 1, 0, 1, 1),
    nrow = 2,
    dimnames = list(c("siteA", "siteB"), c("sp1", "sp2", "sp3"))
  )
  fn(sp, n.plt = 2, start = "fixed",
     richness = TRUE, RRR = FALSE, CWE = FALSE,
     shannon = FALSE, simpson = FALSE, simpson_beta = FALSE,
     frequent = FALSE, random = FALSE, plot = FALSE, verbose = FALSE)
  expect_true("info" %in% captured_levels)
})
