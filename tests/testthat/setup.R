# setup.R — executed once before the entire test suite by testthat.
#
# Stub httr::POST globally so that no real Sentry HTTP calls are made during
# any test.  ausplotsR's API calls use httr::GET, so replacing POST only
# affects .sentry_post — all of whose payload-building logic still executes.
# Tests that need to inspect the exact payload use testthat::local_mocked_bindings()
# or mockery::stub() on a local copy of .sentry_post to override this global stub.
local({
  fake_post <- function(url = NULL, config = list(), ..., body = NULL,
                        encode = c("multipart", "form", "json", "raw"),
                        handle = NULL) {
    structure(list(status_code = 200L), class = "response")
  }
  utils::assignInNamespace("POST", fake_post, ns = "httr")
})
# Reset the breadcrumb store so crumbs from one test never leak into the next.
# Use getFromNamespace() because ausplotsR::: doesn't resolve during devtools::test().
local({
  env <- getFromNamespace(".sentry_env", "ausplotsR")
  env$crumbs <- list()
})
# Clear any auth options left from interactive sessions so that tests never
# hit the _inc_unpub API path accidentally.
options("ausplotsR_role"   = NULL)
options("ausplotsR_secret" = NULL)

# ── Shared fixture helpers ────────────────────────────────────────────────────
# Available to every test file without re-definition.

fixture <- function(name) {
  jsonlite::fromJSON(testthat::test_path("fixtures", name), simplifyDataFrame = TRUE)
}

make_ausplots_from_fixtures <- function() {
  vouch_raw <- fixture("veg_voucher.json")
  vouch <- data.frame(
    site_unique = paste0(vouch_raw$site_location_name, "-", vouch_raw$site_location_visit_id),
    vouch_raw,
    stringsAsFactors = FALSE
  )
  pi_raw <- fixture("veg_pi.json")
  pi_df <- data.frame(
    site_unique = paste0(pi_raw$site_location_name, "-", pi_raw$site_location_visit_id),
    hits_unique = seq_len(nrow(pi_raw)),
    pi_raw,
    stringsAsFactors = FALSE
  )
  site_raw <- fixture("site.json")
  site_df <- data.frame(
    site_unique = paste0(site_raw$site_location_name, "-", site_raw$site_location_visit_id),
    site_raw,
    stringsAsFactors = FALSE
  )
  list(veg.vouch = vouch, veg.PI = pi_df, site.info = site_df)
}
