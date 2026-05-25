# OpenTelemetry helpers for manual testing/development scripts.
# Source this file before running any local diagnostic scripts:
#   source("helper-scripts/sentry_helpers.R")
#
# Requires: otel (CRAN) + otelsdk (CRAN) + a running OTEL collector, or use
# otelsdk::start_logging_exporter() / otelsdk::start_file_exporter() locally.

# ── Setup ──────────────────────────────────────────────────────────────────────
# Call once to activate a local stdout exporter for development.

otel_dev_setup <- function() {
  if (!requireNamespace("otelsdk", quietly = TRUE)) {
    stop("Install otelsdk from CRAN: install.packages('otelsdk')")
  }
  otelsdk::start_logging_exporter()
  message("OpenTelemetry stdout exporter active.")
}

# ── Wrappers matching the package's internal API ───────────────────────────────
# These let you call the same functions as the package without loading it.

sentry_capture_message <- function(msg, level = "info") {
  switch(level,
    "warning" = otel::log_warn(msg),
    "error"   = otel::log_error(msg),
    otel::log_info(msg)
  )
}

sentry_capture_exception <- function(e, title = "An exception occurred in ausplotsR") {
  otel::log_error(paste0(title, ": ", conditionMessage(e)))
}

