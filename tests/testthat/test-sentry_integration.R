library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# Sentry integration
#
# Tests for the httr-based Sentry helpers in R/sentry_helper.R.
# setup.R stubs .sentry_post as a no-op so no real HTTP calls are made.
# Tests that need to assert payload content use mockery::stub() on a local
# copy of the function under test.
#
# ausplotsR::: cannot be used during devtools::test() because the package is
# loaded via pkgload, not installed.  Use getFromNamespace() instead.
# ══════════════════════════════════════════════════════════════════════════════

# Capture the package's mutable breadcrumb environment once.
.se <- getFromNamespace(".sentry_env", "ausplotsR")

# Helper: reset crumbs before/after each breadcrumb test.
reset_crumbs <- function() .se$crumbs <- list()

# ══════════════════════════════════════════════════════════════════════════════
# .sentry_parse_dsn
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: valid DSN is parsed into correct components
# Input:         well-formed DSN string
# Expected:      public_key, host, project_id, store_url extracted correctly
test_that(".sentry_parse_dsn extracts all components from a valid DSN", {
  parsed <- .sentry_parse_dsn(
    "https://abc123@o12345.ingest.sentry.io/67890"
  )
  expect_equal(parsed$public_key, "abc123")
  expect_equal(parsed$host,       "o12345.ingest.sentry.io")
  expect_equal(parsed$project_id, "67890")
  expect_equal(parsed$store_url,  "https://o12345.ingest.sentry.io/api/67890/store/")
})

# What it tests: invalid DSN raises a meaningful error
# Input:         non-DSN string
# Expected:      stop() with "Invalid Sentry DSN format"
test_that(".sentry_parse_dsn errors on invalid DSN", {
  expect_error(.sentry_parse_dsn("not-a-dsn"), "Invalid Sentry DSN format")
})

# ══════════════════════════════════════════════════════════════════════════════
# .sentry_breadcrumb
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: breadcrumb is added to .sentry_env$crumbs with correct fields
# Input:         message = "step 1", level = "info", category = "test.cat"
# Expected:      .sentry_env$crumbs has exactly 1 entry with those values
test_that(".sentry_breadcrumb adds crumb with correct fields", {
  reset_crumbs()
  on.exit(reset_crumbs())

  .sentry_breadcrumb("step 1", level = "info", category = "test.cat")

  crumbs <- .se$crumbs
  expect_length(crumbs, 1L)
  expect_equal(crumbs[[1]]$message,  "step 1")
  expect_equal(crumbs[[1]]$level,    "info")
  expect_equal(crumbs[[1]]$category, "test.cat")
  expect_equal(crumbs[[1]]$type,     "default")
})

# What it tests: multiple breadcrumbs accumulate in order
# Input:         two .sentry_breadcrumb calls
# Expected:      .sentry_env$crumbs has 2 entries in insertion order
test_that(".sentry_breadcrumb accumulates multiple crumbs in order", {
  reset_crumbs()
  on.exit(reset_crumbs())

  .sentry_breadcrumb("first")
  .sentry_breadcrumb("second")

  crumbs <- .se$crumbs
  expect_length(crumbs, 2L)
  expect_equal(crumbs[[1]]$message, "first")
  expect_equal(crumbs[[2]]$message, "second")
})

# What it tests: opt-out prevents breadcrumbs from being added
# Input:         ausplotsR.telemetry = FALSE
# Expected:      .sentry_env$crumbs remains empty
test_that(".sentry_breadcrumb is no-op when telemetry is disabled", {
  reset_crumbs()
  on.exit({ reset_crumbs(); options(ausplotsR.telemetry = NULL) })
  options(ausplotsR.telemetry = FALSE)

  .sentry_breadcrumb("should not appear")

  expect_length(.se$crumbs, 0L)
})

# ══════════════════════════════════════════════════════════════════════════════
# .sentry_post
#
# These tests use a LOCAL copy of .sentry_post (getFromNamespace) and stub
# httr::POST via mockery::stub() so the full payload-building logic runs
# but no real network call is made.
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: the correct Sentry Store API URL is derived from the DSN
# Input:         default .SENTRY_DSN; payload_extra = trivial
# Expected:      httr::POST called with the store URL for this project
test_that(".sentry_post sends to the correct Sentry store URL", {
  captured_url <- NULL
  fn <- getFromNamespace(".sentry_post", "ausplotsR")
  mockery::stub(fn, "httr::POST", function(url = NULL, ..., body = NULL) {
    captured_url <<- url
    structure(list(status_code = 200L), class = "response")
  })
  fn(list(level = "info", message = list(formatted = "test")))
  expect_equal(captured_url,
    "https://o295312.ingest.us.sentry.io/api/4511306526818304/store/")
})

# What it tests: the JSON body contains all mandatory Sentry event fields and
#                merges payload_extra correctly
# Input:         payload_extra with level = "info" and a message
# Expected:      parsed body has event_id, timestamp, platform, logger,
#                breadcrumbs, contexts, tags, and the payload_extra fields
test_that(".sentry_post body contains required Sentry event fields", {
  captured_body <- NULL
  fn <- getFromNamespace(".sentry_post", "ausplotsR")
  mockery::stub(fn, "httr::POST", function(url = NULL, ..., body = NULL) {
    captured_body <<- body
    structure(list(status_code = 200L), class = "response")
  })
  fn(list(level = "info", message = list(formatted = "hello")))
  parsed <- jsonlite::fromJSON(captured_body)
  expect_true("event_id"    %in% names(parsed))
  expect_true("timestamp"   %in% names(parsed))
  expect_true("breadcrumbs" %in% names(parsed))
  expect_true("contexts"    %in% names(parsed))
  expect_true("tags"        %in% names(parsed))
  expect_equal(parsed$platform,          "other")
  expect_equal(parsed$logger,            "ausplotsR")
  expect_equal(parsed$level,             "info")
  expect_equal(parsed$message$formatted, "hello")
})

# What it tests: payload_extra fields (level, exception, etc.) are merged into
#                the top-level event object
# Input:         payload_extra with level = "error" and an exception structure
# Expected:      parsed body has the exception values present
test_that(".sentry_post merges payload_extra into the event body", {
  captured_body <- NULL
  fn <- getFromNamespace(".sentry_post", "ausplotsR")
  mockery::stub(fn, "httr::POST", function(url = NULL, ..., body = NULL) {
    captured_body <<- body
    structure(list(status_code = 200L), class = "response")
  })
  fn(list(level = "error",
          exception = list(values = list(
            list(type = "simpleError", value = "boom")
          ))))
  parsed <- jsonlite::fromJSON(captured_body)
  expect_equal(parsed$level,                     "error")
  expect_equal(parsed$exception$values$type,     "simpleError")
  expect_equal(parsed$exception$values$value,    "boom")
})

# What it tests: network failures inside httr::POST are silently swallowed
# Input:         httr::POST stub throws
# Expected:      no error propagated to the caller
test_that(".sentry_post silently ignores httr::POST errors", {
  fn <- getFromNamespace(".sentry_post", "ausplotsR")
  mockery::stub(fn, "httr::POST", function(...) stop("network failure"))
  expect_no_error(fn(list(level = "info")))
})

# ══════════════════════════════════════════════════════════════════════════════
# .sentry_capture_message
#
# These tests use testthat::local_mocked_bindings() to replace httr::POST for
# the duration of the test.  The full call chain runs:
#   .sentry_capture_message → .sentry_post → httr::POST (mocked)
# Payload assertions operate on the parsed JSON body.
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: level "warning" and message text reach httr::POST as JSON body
# Input:         msg = "hello warning", level = "warning"
# Expected:      parsed body has level = "warning" and formatted = "hello warning"
test_that(".sentry_capture_message sends correct level and text via httr::POST", {
  captured_body <- NULL
  local_mocked_bindings(
    POST = function(url = NULL, ..., body = NULL) {
      captured_body <<- body
      structure(list(status_code = 200L), class = "response")
    },
    .package = "httr"
  )
  .sentry_capture_message("hello warning", level = "warning")
  parsed <- jsonlite::fromJSON(captured_body)
  expect_equal(parsed$level,             "warning")
  expect_equal(parsed$message$formatted, "hello warning")
})

# What it tests: level "error" reaches httr::POST correctly
# Input:         level = "error"
# Expected:      parsed body level = "error"
test_that(".sentry_capture_message forwards level 'error' via httr::POST", {
  captured_body <- NULL
  local_mocked_bindings(
    POST = function(url = NULL, ..., body = NULL) {
      captured_body <<- body
      structure(list(status_code = 200L), class = "response")
    },
    .package = "httr"
  )
  .sentry_capture_message("bad thing", level = "error")
  parsed <- jsonlite::fromJSON(captured_body)
  expect_equal(parsed$level, "error")
})

# What it tests: default level is "info" when level is not supplied
# Input:         no level argument
# Expected:      parsed body level = "info"
test_that(".sentry_capture_message defaults to 'info' level", {
  captured_body <- NULL
  local_mocked_bindings(
    POST = function(url = NULL, ..., body = NULL) {
      captured_body <<- body
      structure(list(status_code = 200L), class = "response")
    },
    .package = "httr"
  )
  .sentry_capture_message("just info")
  parsed <- jsonlite::fromJSON(captured_body)
  expect_equal(parsed$level, "info")
})

# What it tests: opt-out prevents httr::POST from being called at all
# Input:         ausplotsR.telemetry = FALSE
# Expected:      httr::POST never called
test_that(".sentry_capture_message is no-op when telemetry is disabled", {
  on.exit(options(ausplotsR.telemetry = NULL))
  options(ausplotsR.telemetry = FALSE)
  post_called <- FALSE
  local_mocked_bindings(
    POST = function(...) {
      post_called <<- TRUE
      structure(list(status_code = 200L), class = "response")
    },
    .package = "httr"
  )
  .sentry_capture_message("hello", level = "info")
  expect_false(post_called)
})

# ══════════════════════════════════════════════════════════════════════════════
# .sentry_capture_exception
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: exception payload has correct level, message, and exception type
# Input:         simpleError("boom"), title = "An exception occurred in test"
# Expected:      parsed body fields match
test_that(".sentry_capture_exception builds correct Sentry payload", {
  captured_body <- NULL
  local_mocked_bindings(
    POST = function(url = NULL, ..., body = NULL) {
      captured_body <<- body
      structure(list(status_code = 200L), class = "response")
    },
    .package = "httr"
  )
  .sentry_capture_exception(simpleError("boom"), title = "An exception occurred in test")
  parsed <- jsonlite::fromJSON(captured_body)
  expect_equal(parsed$level,                     "error")
  expect_equal(parsed$message$formatted,         "An exception occurred in test: boom")
  expect_equal(parsed$exception$values$value,    "boom")
  expect_equal(parsed$exception$values$type,     "simpleError")
})

# What it tests: default title is used when title is omitted
# Input:         simpleError("oops"); no title arg
# Expected:      message contains "An exception occurred in ausplotsR"
test_that(".sentry_capture_exception uses default title when omitted", {
  captured_body <- NULL
  local_mocked_bindings(
    POST = function(url = NULL, ..., body = NULL) {
      captured_body <<- body
      structure(list(status_code = 200L), class = "response")
    },
    .package = "httr"
  )
  .sentry_capture_exception(simpleError("oops"))
  parsed <- jsonlite::fromJSON(captured_body)
  expect_true(grepl("An exception occurred in ausplotsR", parsed$message$formatted))
})

# What it tests: breadcrumb store is cleared after the exception is sent
# Input:         .sentry_env$crumbs pre-populated with one crumb
# Expected:      .sentry_env$crumbs is empty after the call
# (Global httr::POST mock from setup.R returns fake 200; no local stub needed.)
test_that(".sentry_capture_exception clears breadcrumb store after send", {
  .se$crumbs <- list(list(message = "crumb 1"))
  on.exit(reset_crumbs())
  .sentry_capture_exception(simpleError("x"))
  expect_length(.se$crumbs, 0L)
})

# What it tests: opt-out prevents httr::POST and breadcrumbs are NOT cleared
# Input:         ausplotsR.telemetry = FALSE; one crumb in store
# Expected:      httr::POST not called; crumbs untouched
test_that(".sentry_capture_exception is no-op when telemetry is disabled", {
  .se$crumbs <- list(list(message = "keep me"))
  on.exit({ reset_crumbs(); options(ausplotsR.telemetry = NULL) })
  options(ausplotsR.telemetry = FALSE)
  post_called <- FALSE
  local_mocked_bindings(
    POST = function(...) {
      post_called <<- TRUE
      structure(list(status_code = 200L), class = "response")
    },
    .package = "httr"
  )
  .sentry_capture_exception(simpleError("x"))
  expect_false(post_called)
  expect_length(.se$crumbs, 1L)
})

# What it tests: exception does not raise even when the POST chain runs
# Input:         real call; global httr::POST mock returns fake 200
# Expected:      no error raised
test_that(".sentry_capture_exception does not raise", {
  expect_no_error(.sentry_capture_exception(simpleError("x")))
})

# ══════════════════════════════════════════════════════════════════════════════
# .onAttach
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: .onAttach emits the telemetry disclosure message
# Expected:      message contains "ausplotsR.telemetry"
test_that(".onAttach shows telemetry disclosure message", {
  on_attach <- getFromNamespace(".onAttach", "ausplotsR")
  expect_message(on_attach("", "ausplotsR"), "ausplotsR.telemetry")
})

