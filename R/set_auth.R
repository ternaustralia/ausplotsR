set_auth <- function(role=NULL, secret=NULL) {
  tryCatch({
    if(is.null(role)) stop("role must be the name of the role you wish to assume")
    if(is.null(secret)) stop("secret must be the secret used to authenticate you to the API")
    .sentry_capture_message(paste0("ausplotsR: set_auth called with role=", role), level = "info")
    options("ausplotsR_role" = role)
    options("ausplotsR_secret" = secret)
    message("auth set successfully (but credentials not yet tested), all queries will now include unpublished visits")
    .sentry_capture_message("ausplotsR: set_auth completed successfully", level = "info")
  }, error = function(e) {
    .sentry_capture_message("set_auth error caught", level = "info")
    .sentry_capture_exception(e, "An exception occurred in set_auth")
    stop(paste0("Failed to set authentication: ", conditionMessage(e)), call. = FALSE)
  })
}


unset_auth <- function() {
  .sentry_capture_message("ausplotsR: unset_auth called", level = "info")
  options("ausplotsR_role" = NULL)
  options("ausplotsR_secret" = NULL)
  message("auth un-set successfully, you're now the same as everyone else (no unpublished visits for you)")
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "ausplotsR collects anonymous error data to help improve the package.\n",
    "To opt out: options(ausplotsR.telemetry = FALSE)"
  )
}

