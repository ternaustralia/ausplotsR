library(testthat)
library(mockery)

# ══════════════════════════════════════════════════════════════════════════════
# set_auth()
#
# Stores API role and secret as R options for authenticated queries. Has two
# explicit stop() calls for NULL inputs. All other errors (e.g. options()
# failure) are caught by the outer tryCatch.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# set_auth() — OUTPUT / HAPPY PATH
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: valid role and secret are stored as R options without error
# Input:         role = "test_role", secret = "test_secret"
# Expected:      no error; options set correctly
test_that("set_auth: valid role and secret are stored without error", {
  on.exit({
    options("ausplotsR_role" = NULL)
    options("ausplotsR_secret" = NULL)
  }, add = TRUE)
  expect_no_error(
    suppressMessages(set_auth(role = "test_role", secret = "test_secret"))
  )
  expect_equal(getOption("ausplotsR_role"), "test_role")
  expect_equal(getOption("ausplotsR_secret"), "test_secret")
})

# ══════════════════════════════════════════════════════════════════════════════
# set_auth() — CAUGHT ERROR PATHS (tryCatch fires)
# ══════════════════════════════════════════════════════════════════════════════

# What it tests: NULL role is caught and wrapped
# Input:         role = NULL, secret = "s"
# Expected:      stop() with "Failed to set authentication:" prefix
test_that("set_auth: NULL role is caught and wrapped", {
  expect_error(
    set_auth(role = NULL, secret = "s"),
    "Failed to set authentication:"
  )
})

# What it tests: original error message survives inside the wrapped message
# Input:         role = NULL
# Expected:      error contains the original stop() cause about role
test_that("set_auth: wrapped error preserves original cause for NULL role", {
  expect_error(
    set_auth(role = NULL, secret = "s"),
    "role must be"
  )
})

# What it tests: NULL secret is caught and wrapped
# Input:         role = "r", secret = NULL
# Expected:      stop() with "Failed to set authentication:" prefix
test_that("set_auth: NULL secret is caught and wrapped", {
  expect_error(
    set_auth(role = "r", secret = NULL),
    "Failed to set authentication:"
  )
})

# What it tests: original error message survives for NULL secret
# Input:         role = "r", secret = NULL
# Expected:      error contains the original stop() cause about secret
test_that("set_auth: wrapped error preserves original cause for NULL secret", {
  expect_error(
    set_auth(role = "r", secret = NULL),
    "secret must be"
  )
})

# ══ SENTRY CAPTURE ══

# What it tests: when input is invalid (NULL role), .sentry_capture_exception is called
# Input:         role = NULL; .sentry_capture_exception stubbed
# Expected:      capture_exception_called = TRUE AND expect_error fires
test_that("set_auth: capture_exception called on error", {
  capture_exception_called <- FALSE
  fn <- set_auth
  mockery::stub(fn, ".sentry_capture_exception", function(e, ...) {
    capture_exception_called <<- TRUE
  })
  expect_error(fn(role = NULL, secret = "s"))
  expect_true(capture_exception_called)
})

# What it tests: on success, .sentry_capture_message is called with level "info"
# Input:         valid role and secret; .sentry_capture_message stubbed
# Expected:      an "info" level message was captured
test_that("set_auth: capture_message 'info' on successful auth", {
  captured_levels <- character(0)
  fn <- set_auth
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_levels <<- c(captured_levels, level)
  })
  fn(role = "my_role", secret = "my_secret")
  expect_true("info" %in% captured_levels)
})

# ══ SENTRY — entry breadcrumb content ══

# What it tests: entry .sentry_capture_message includes the role value
# Input:         role = "analyst"; .sentry_capture_message stubbed
# Expected:      at least one captured message contains "role=analyst"
test_that("set_auth: entry breadcrumb includes role value", {
  crumb_msgs <- character(0)
  fn <- set_auth
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    crumb_msgs <<- c(crumb_msgs, msg)
  })
  suppressMessages(fn(role = "analyst", secret = "s"))
  expect_true(any(grepl("role=analyst", crumb_msgs)))
})

# What it tests: capture_message is called twice on success (entry + completion)
# Input:         valid role and secret; .sentry_capture_message stubbed
# Expected:      exactly 2 calls
test_that("set_auth: capture_message called once on success (completion only)", {
  call_count <- 0L
  fn <- set_auth
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    call_count <<- call_count + 1L
  })
  suppressMessages(fn(role = "r", secret = "s"))
  expect_equal(call_count, 2L)
})

# What it tests: completion capture_message contains "completed successfully"
# Input:         valid role and secret; .sentry_capture_message stubbed
# Expected:      last captured message contains "completed successfully"
test_that("set_auth: last capture_message is the completion message", {
  captured_msgs <- character(0)
  fn <- set_auth
  mockery::stub(fn, ".sentry_capture_message", function(msg, level = "info") {
    captured_msgs <<- c(captured_msgs, msg)
  })
  suppressMessages(fn(role = "r", secret = "s"))
  expect_true(grepl("completed successfully", captured_msgs[length(captured_msgs)]))
})

# ══ SENTRY — .sentry_capture_exception ══

# What it tests: .sentry_capture_exception sends title-prefixed message to Sentry
# Input:         simpleError("boom"); httr::POST mocked
# Expected:      body message.formatted = "An exception occurred in ausplotsR: boom"
test_that(".sentry_capture_exception sends prefixed message to Sentry", {
  captured_body <- NULL
  local_mocked_bindings(
    POST = function(url = NULL, ..., body = NULL) {
      captured_body <<- body
      structure(list(status_code = 200L), class = "response")
    },
    .package = "httr"
  )
  .sentry_capture_exception(simpleError("boom"))
  parsed <- jsonlite::fromJSON(captured_body)
  expect_equal(parsed$message$formatted, "An exception occurred in ausplotsR: boom")
})

# What it tests: .sentry_capture_exception does not raise an error itself
# Input:         real call with no backend configured
# Expected:      no error raised
test_that(".sentry_capture_exception does not raise", {
  expect_no_error(.sentry_capture_exception(simpleError("x")))
})

# ══ SENTRY — .sentry_capture_message ══

# What it tests: .sentry_capture_message sends 'warning' level payload to Sentry
# Input:         msg = "hello", level = "warning"; httr::POST mocked
# Expected:      body has level = "warning" and formatted = "hello"
test_that(".sentry_capture_message sends 'warning' level to Sentry", {
  captured_body <- NULL
  local_mocked_bindings(
    POST = function(url = NULL, ..., body = NULL) {
      captured_body <<- body
      structure(list(status_code = 200L), class = "response")
    },
    .package = "httr"
  )
  .sentry_capture_message("hello", level = "warning")
  parsed <- jsonlite::fromJSON(captured_body)
  expect_equal(parsed$level,             "warning")
  expect_equal(parsed$message$formatted, "hello")
})

# What it tests: .sentry_capture_message does not raise an error
# Input:         real call with no backend configured
# Expected:      no error raised
test_that(".sentry_capture_message does not raise", {
  expect_no_error(.sentry_capture_message("hi", level = "info"))
})


