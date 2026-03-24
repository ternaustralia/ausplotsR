#=============================================================================
# Retrieve NVIS attributes for AusPlots
#
# This function joins a static NVIS table to AusPlots sites using
# "site_unique", returning all columns from the NVIS table.
#
# By default, the function returns NVIS information for all available
# AusPlots sites. Users can also provide:
# - a vector of "site_unique" values,
# - a single "site_unique",
# - or a subset of AusPlots in form of a data frame containing a "site_unique" column.
#=============================================================================

get_nvis <- function(site_unique = NULL) {
  
  nvis <- get("NVIS", envir = asNamespace("ausplotsR"))
  nvis$site_unique <- as.character(nvis$site_unique)
  
  if (is.null(site_unique)) {
    return(as.data.frame(nvis))
  }
  
  if (is.data.frame(site_unique)) {
    if (!"site_unique" %in% names(site_unique)) {
      stop("If 'site_unique' is a data frame, it must contain a 'site_unique' column.",
           call. = FALSE)
    }
    request_sites <- unique(as.character(site_unique$site_unique))
  } else if (is.atomic(site_unique)) {
    request_sites <- unique(as.character(site_unique))
  } else {
    stop("'site_unique' must be NULL, a character vector, a single site_unique, or a data frame with a 'site_unique' column.",
         call. = FALSE)
  }
  
  request_df <- data.frame(
    site_unique = request_sites,
    stringsAsFactors = FALSE
  )
  
  out <- merge(
    x = request_df,
    y = nvis,
    by = "site_unique",
    all.x = TRUE,
    sort = FALSE
  )
  
  as.data.frame(out)
}