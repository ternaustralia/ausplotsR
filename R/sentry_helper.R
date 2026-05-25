# Private Sentry helpers — used internally by all ausplotsR functions.
#
# Errors and messages are sent to the TERN Sentry project via the Sentry
# Store HTTP API.  No user data, file paths, or credentials are transmitted —
# only error messages, R/OS version, and ausplotsR version.
#
# Users can opt out at any time:
#   options(ausplotsR.telemetry = FALSE)
#
# The DSN below is write-only (cannot be used to read events back).
# Storing a public DSN in package source is normal and accepted CRAN practice.

# ── 1. Configuration ──────────────────────────────────────────────────────────

# Replace with your real DSN from Sentry → Settings → Projects → Client Keys.
.SENTRY_DSN <- "https://217e431e07ad3f3258e5827d2277fdb5@o295312.ingest.us.sentry.io/4511306526818304"

# Parse DSN into components used by the HTTP API.
.sentry_parse_dsn <- function(dsn) {
  m <- regmatches(dsn, regexec("https://([^@]+)@([^/]+)/([0-9]+)", dsn))[[1]]
  if (length(m) < 4L) stop("Invalid Sentry DSN format.")
  list(
    public_key = m[2],
    host       = m[3],
    project_id = m[4],
    store_url  = sprintf("https://%s/api/%s/store/", m[3], m[4])
  )
}

# Package-level mutable state: breadcrumb store, cleared after each exception.
.sentry_env <- new.env(parent = emptyenv())
.sentry_env$crumbs <- list()

# ── 2. Breadcrumb ─────────────────────────────────────────────────────────────

# Add a breadcrumb to the in-memory store.  Breadcrumbs are attached to the
# next exception that is sent, giving context on what happened before the error.
#
# Args:
#   message  — human-readable description of the event
#   level    — "info" | "warning" | "error"
#   category — dot-namespaced string, e.g. "ausplotsR.set_auth"
.sentry_breadcrumb <- function(message, level = "info", category = "ausplotsR") {
  if (!isTRUE(getOption("ausplotsR.telemetry", TRUE))) return(invisible(NULL))
  crumb <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3", tz = "UTC"),
    type      = "default",
    level     = level,
    category  = category,
    message   = message
  )
  .sentry_env$crumbs <- c(.sentry_env$crumbs, list(crumb))
  invisible(NULL)
}

# ── 3. Message (info / warning / error) ───────────────────────────────────────

# Send a plain message to Sentry at the given severity level.
# Does not clear the breadcrumb store.
#
# Args:
#   msg   — the message string
#   level — "info" | "warning" | "error"
.sentry_capture_message <- function(msg, level = "info") {
  if (!isTRUE(getOption("ausplotsR.telemetry", TRUE))) return(invisible(NULL))
  .sentry_post(list(
    level   = level,
    message = list(formatted = msg)
  ))
  invisible(NULL)
}

# ── 4. Exception ──────────────────────────────────────────────────────────────

# Send an exception (error condition) to Sentry.
# Attaches any accumulated breadcrumbs, then clears the store.
#
# Args:
#   e     — a condition object (from tryCatch / withCallingHandlers)
#   title — prefix prepended to the error message in the Sentry title
.sentry_capture_exception <- function(e, title = "An exception occurred in ausplotsR") {
  if (!isTRUE(getOption("ausplotsR.telemetry", TRUE))) return(invisible(NULL))
  .sentry_post(list(
    level   = "error",
    message = list(formatted = paste0(title, ": ", conditionMessage(e))),
    exception = list(values = list(list(
      type      = class(e)[1],
      value     = conditionMessage(e),
      mechanism = list(type = "generic", handled = TRUE)
    )))
  ))
  .sentry_env$crumbs <- list()   # clear breadcrumbs after send
  invisible(NULL)
}

# ── Internal: build and POST a Sentry event ───────────────────────────────────

.sentry_post <- function(payload_extra = list()) {
  tryCatch({
    dsn <- .sentry_parse_dsn(.SENTRY_DSN)
    now <- as.integer(Sys.time())

    # 32-char hex UUID for event_id (Sentry requirement)
    event_id <- paste(sprintf("%08x", abs(sample.int(.Machine$integer.max, 4L))),
                      collapse = "")

    event <- c(
      list(
        event_id    = event_id,
        timestamp   = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3", tz = "UTC"),
        platform    = "other",
        logger      = "ausplotsR",
        breadcrumbs = list(values = .sentry_env$crumbs),
        contexts    = list(
          runtime = list(name = "R", version = R.version$version.string),
          os      = list(name = Sys.info()[["sysname"]])
        ),
        tags = list(
          ausplotsR.version = as.character(utils::packageVersion("ausplotsR")),
          r.version         = R.version$version.string
        )
      ),
      payload_extra
    )

    auth_header <- sprintf(
      "Sentry sentry_version=7, sentry_key=%s, sentry_client=ausplotsR/%s, sentry_timestamp=%d",
      dsn$public_key,
      as.character(utils::packageVersion("ausplotsR")),
      now
    )

    httr::POST(
      url  = dsn$store_url,
      httr::add_headers(
        `X-Sentry-Auth` = auth_header,
        `Content-Type`  = "application/json"
      ),
      httr::timeout(2),
      body = jsonlite::toJSON(event, auto_unbox = TRUE, null = "null")
    )
  }, error = function(e) invisible(NULL))  # never surface telemetry errors to users
}
