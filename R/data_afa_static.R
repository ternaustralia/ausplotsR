
#' Fetch the Alien Flora of Australia (AFA) dataset from the AusPlots API
#'
#' Retrieves the current harmonised AFA invasion status table from the AusPlots
#' REST API and returns it as a data frame with the internal column schema
#' expected by \code{\link{invasion_info}}.
#'
#' @return A data frame with one row per taxon containing harmonised invasion
#'   status columns for all Australian states, territories, and external
#'   territories, in the schema expected by \code{\link{invasion_info}}.
#'
#' @seealso \code{\link{invasion_info}}, \code{\link{get_ausplots}}
#'
#' @export
get_afa_static <- function() {

  raw <- .ausplots_api(path = "/afa_static", query = list())
  afa <- as.data.frame(raw, stringsAsFactors = FALSE)

  # Map API column names → internal schema names used by invasion_info().
  # Format: api_column_name = "internal_column_name"
  col_map <- c(
    apc_canonical_name      = "canonical_name",               # APC canonical binomial
    apc_name                = "scientific_name",              # full APC scientific name with author
    aligned_name            = "accepted_name_usage",          # name used for join key construction
    act_unified_status      = "status_ACT_harmonised",        # state/territory status columns
    nsw_unified_status      = "status_NSW_harmonised",
    nt_unified_status       = "status_NT_harmonised",
    qld_unified_status      = "status_QLD_harmonised",
    sa_unified_status       = "status_SA_harmonised",
    tas_unified_status      = "status_TAS_harmonised",
    vic_unified_status      = "status_VIC_harmonised",
    wa_unified_status       = "status_WA_harmonised",
    ar                      = "status_AR_harmonised",         # external territory status columns
    coi                     = "status_CoI_harmonised",
    cai                     = "status_CaI_harmonised",
    chi                     = "status_ChI_harmonised",
    lhi                     = "status_LHI_harmonised",
    ni                      = "status_NI_harmonised",
    hi                      = "status_HI_harmonised",
    csi                     = "status_CSI_harmonised",
    mdi                     = "status_MDI_harmonised",
    afa_unified_status      = "unified_AFA_status_continental",      # national AFA status (mainland)
    national_status         = "unified_AFA_status_all_territories",  # national AFA status (incl. territories)
    griis_alien_status      = "griis_is_invasive",            # GRIIS invasive flag
    establishment_means     = "establishment_means",          # Darwin Core establishment
    degree_of_establishment = "degree_of_establishment"       # Darwin Core degree
  )
  for (api_col in names(col_map)) {
    internal_col <- col_map[[api_col]]
    if (api_col %in% names(afa) && !internal_col %in% names(afa)) {
      names(afa)[names(afa) == api_col] <- internal_col
    }
  }

  # The API does not return declared/threatened detail columns or family.
  # Add them as NA so the schema expected by invasion_info() is complete.
  for (missing_col in c("declared_state_details", "declared_national_details",
                        "threatened_state_details", "threatened_epbc_details",
                        "family")) {
    if (!missing_col %in% names(afa)) {
      afa[[missing_col]] <- NA_character_
    }
  }

  afa
}