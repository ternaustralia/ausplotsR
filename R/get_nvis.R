#=============================================================================
# Retrieve NVIS attributes for AusPlots
#
# This function fetches NVIS vegetation attributes from the AusPlots REST
# API (/nvis_ausplot_r) and returns them as a data frame.
#
# By default, the function returns NVIS information for all available
# AusPlots sites. Users can also provide:
# - a vector of "site_unique" values,
# - a single "site_unique",
# - or a subset of AusPlots in form of a data frame containing a "site_unique" column.
#
# -----------------------------------------------------------------------------
# API response columns
# -----------------------------------------------------------------------------
#
# site_location_visit_id   : Integer visit ID
# site_location_name       : Plot name
# site_unique              : Primary join key (PLOTID-VISITID). Includes State
#                            (first two letters), transect (T) or AusPlot (A),
#                            IBRA bioregion (three-letter code), a four-digit
#                            numeric identifier, a dash, and the visit code.
# class                    : Dominant growth form for the structurally dominant
#                            stratum (NVIS Level 1: Class).
# structural_formation     : Structural formation of the dominant vegetation
#                            stratum (NVIS Level 2), including cover, average
#                            height and dominant growth form.
# nvis_code                : NVIS structural formation code representing
#                            dominant growth form, cover class code and height
#                            class code.
# broad_floristic_formation: NVIS Level 3 classification describing dominant
#                            genus (or genera) and structural formation for the
#                            dominant stratum.
# sub_formation            : NVIS Level 4 classification describing dominant
#                            genera and structural formation across up to three
#                            strata.
# association              : NVIS Level 5 vegetation association describing up
#                            to three dominant species and structural formations
#                            across up to three strata.
# vegetation_community_l5  : Common-language description of the NVIS Level 5
#                            vegetation association.
# vegetation_community_l6  : Common-language description of the NVIS Level 6
#                            vegetation sub-association.
# sub_association          : NVIS Level 6 classification describing up to five
#                            dominant species and structural formations across
#                            strata.
# upper_average_height_metre: Average height of the upper canopy stratum
#                            calculated from point intercept hits (>2 m height).
# upper_cover_percentage   : Canopy cover calculated from point intercept hits
#                            (1000 per plot), including canopy hits and
#                            "in canopy sky". Dominant stratum if cover >1.5%.
# mid_average_height_metre : Average height of the mid stratum calculated using
#                            NVIS height and growth-form rules applied to point
#                            intercept hits.
# mid_cover_percentage     : Foliage projective cover calculated from point
#                            intercept hits. Dominant stratum if cover >2% and
#                            upper cover <1.5%.
# ground_average_height_metre: Average height of the ground layer calculated
#                            using NVIS height rules applied to point intercept
#                            hits.
# ground_cover_percentage  : Ground-layer cover percentage calculated from
#                            ground-layer point intercept hits (1000 per plot).
# MVG_VICTA                : NVIS Major Vegetation Group assigned using the
#                            VICTA (Vegetation Information Classifier Tool
#                            Automator) algorithm applied to the association
#                            string.
# MVG_NAME                 : Full name of the assigned NVIS Major Vegetation
#                            Group.
# MVS_VICTA                : NVIS Major Vegetation Subgroup assigned using the
#                            VICTA classification algorithm.
# MVS_NAME                 : Full name of the assigned NVIS Major Vegetation
#                            Subgroup.
#
# -----------------------------------------------------------------------------
# NVIS strata codes used in classification strings
# -----------------------------------------------------------------------------
# U  Upper stratum (tree overstorey / canopy)
# M  Mid stratum (shrub layer)
# G  Ground stratum (ground layer)
# NVIS substrata are not delineated in this dataset.
#
# -----------------------------------------------------------------------------
# Notation used in NVIS classification strings
# -----------------------------------------------------------------------------
# +   Denotes dominant stratum
# ^   Denotes dominant growth form or dominant genus
# \   Separates strata in Level 4 structural descriptions
# /   Separates strata and co-dominant genera
# ;   Separates strata in Level 5 and Level 6 vegetation descriptions
#
# -----------------------------------------------------------------------------
# Data caveats
# -----------------------------------------------------------------------------
# - Height values are in metres and are derived from point intercept
#   measurements. Heights are estimated where vegetative material is
#   intercepted, which may underestimate true canopy height.
# - Cover values may be over- or underestimated in sparse woodland or
#   shrubland plots depending on sampling position.
# - Broad floristic formation (Level 3) uses dominant genus and structural
#   formation of the dominant stratum and may not always fully represent
#   community composition.
# - Growth forms were converted to NVIS growth forms from AusTraits
#   plant growth-form classifications.
# - Floristic nomenclature follows the Australian Plant Census (APC).
# - Some plots contain multiple visits (typically 2-4 revisits) and may
#   occasionally be classified differently due to seasonal timing.
#
# -----------------------------------------------------------------------------
# Version history
# -----------------------------------------------------------------------------
# Version 2.0 updates include:
# - Updated growth-form classification (Tree vs Tree mallee).
# - Inclusion of up to three dominant growth forms in Level 5 and Level 6
#   descriptions.
# - Updated stratum rules where tree or tree mallee occurs in the mid stratum.
# - Removal of redundant mid strata where structural formation is identical to
#   the upper stratum.
# - Reinstated family-level determinations.
# - Reinstated "Indeterminate" determinations affecting dominant growth-form
#   assignments.
# This dataset is updated annually.
#
# -----------------------------------------------------------------------------
# References
# -----------------------------------------------------------------------------
# Sparrow B. et al. (2025) TERN Ecosystem Surveillance Monitoring. Version 2.0.
#   https://portal.tern.org.au/metadata/9210c331-ba96-4e0c-9554-f029d61534be
# NVIS Technical Working Group (2017) Australian Vegetation Attribute Manual:
#   National Vegetation Information System Version 7.0.
# Lewis D. et al. (2024) Australian Soil and Land Survey Field Handbook.
# Wenk E.H. et al. (2024) AusTraits plant growth form dataset.
# Source: Terrestrial Ecosystem Research Network (TERN)
#=============================================================================

get_nvis <- function(site_unique = NULL) {
  tryCatch({
    .sentry_capture_message(paste0("ausplotsR: get_nvis called with site_unique=", ifelse(is.null(site_unique), "NULL (all sites)", ifelse(is.data.frame(site_unique), paste0("data.frame with ", nrow(site_unique), " rows"), paste0(length(site_unique), " site(s)")))), level = "info")
    raw <- .ausplots_api(path = "/nvis_ausplot_r", query = list())
    nvis <- as.data.frame(raw, stringsAsFactors = FALSE)
    nvis$site_unique <- as.character(nvis$site_unique)
    .sentry_capture_message(paste0("ausplotsR: get_nvis API returned ", nrow(nvis), " rows for ", length(unique(nvis$site_unique)), " sites"), level = "info")
    
    if (is.null(site_unique)) {
      .sentry_capture_message("ausplotsR: get_nvis returning all sites (no filter)", level = "info")
      .sentry_capture_message("ausplotsR: NVIS data fetched successfully", level = "info")
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
    .sentry_capture_message(paste0("ausplotsR: get_nvis filtering to ", length(request_sites), " requested sites"), level = "info")
    
    out <- merge(
      x = request_df,
      y = nvis,
      by = "site_unique",
      all.x = TRUE,
      sort = FALSE
    )
    .sentry_capture_message(paste0("ausplotsR: get_nvis merge complete: ", nrow(out), " rows, ", sum(!is.na(out$site_unique)), " matched"), level = "info")
    .sentry_capture_message("ausplotsR: NVIS data fetched successfully", level = "info")
    as.data.frame(out)
  }, error = function(e) {
    .sentry_capture_exception(e, "An exception occurred in get_nvis")
    stop(paste0("Failed to retrieve NVIS vegetation attributes: ", conditionMessage(e)), call. = FALSE)
  })
}