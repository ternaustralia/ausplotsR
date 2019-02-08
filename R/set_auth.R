set_auth <- function(role=NULL, secret=NULL) {
  if(is.null(role)) stop("role must be the name of the role you wish to assume")
  if(is.null(secret)) stop("secret must be the secret used to authenticate you to the API")
  options("ausplotsR_role" = role)
  options("ausplotsR_secret" = secret)
  print("auth set successfully (but credentials not yet tested), all queries will now include unpublished visits")
}


unset_auth <- function() {
  options("ausplotsR_role" = NULL)
  options("ausplotsR_secret" = NULL)
  print("auth un-set successfully, you're now the same as everyone else (no unpublished visits for you)")
}
