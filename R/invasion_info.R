#' Add invasion information to AusPlots records using a static AFA table
#'
#' This function aligns plant names from AusPlots records with \pkg{APCalign}
#' and appends invasion-related information from a static Alien Flora of
#' Australia (AFA) dataset.
#'
#' Matching is performed primarily using APC-aligned accepted names and, where
#' needed, a conservative binomial fallback. By default, the function retrieves
#' AusPlots voucher records and appends harmonised invasion status fields for
#' Australian states, mainland territories, and selected external territories,
#' together with national AFA status, GRIIS invasive status, Darwin Core-style
#' establishment fields, and declared and threatened status summaries.
#'
#' A user-supplied data frame can also be provided.
#'
#' @param ausplots A data frame of plant records, or `NULL`. If `NULL`, the
#'   function calls [get_ausplots()] with `veg.vouchers = TRUE` and uses the
#'   `veg.vouch` component.
#' @param afa_static A data frame containing the harmonised Alien Flora of
#'   Australia dataset, as returned by \code{\link{get_afa_static}}.
#' @param taxon_col Character string giving the column in `ausplots` to use for
#'   taxonomic alignment. Defaults to `"herbarium_determination"`.
#' @param keep_apcalign_cols Logical. If `TRUE`, retain columns returned by
#'   \pkg{APCalign}. If `FALSE`, keep only the original input columns plus the
#'   core aligned-name fields.
#' @param keep_all Logical. If `TRUE`, return all joined columns. If `FALSE`,
#'   return a reduced set focused on invasion, declared, and threatened fields.
#' @param remove_helper_cols Logical. If `TRUE`, remove internal helper columns
#'   used for matching.
#' @param clean_output Logical. If `TRUE`, filter uncertain or indeterminate
#'   determinations where relevant and keep a simplified output table.
#' @param ... Additional arguments passed to [get_ausplots()] when `ausplots` is
#'   `NULL`.
#'
#' @details
#' Taxon names are first cleaned and aligned with \pkg{APCalign}. The function
#' then matches aligned names to the AFA dataset using a normalised text key
#' based on `accepted_name_usage`, with a binomial fallback where no exact match
#' is found.
#'
#' The appended invasion fields include harmonised state and territory invasion
#' status columns (`invasion_ACT`, `invasion_NSW`, etc.), external territory
#' invasion status fields, national AFA status (`invasion_AFA_national`,
#' `invasion_AFA_all_territories`), GRIIS invasive status (`invasion_GRIIS`),
#' Darwin Core-style establishment fields
#' (`invasion_establishment_means`, `invasion_degree_of_establishment`), and
#' declared and threatened status summaries.
#'
#' This function is designed primarily for AusPlots voucher data, but can also
#' be used with compatible user-supplied plant record tables.
#'
#' @return A data frame containing the input records plus appended invasion,
#'   declared, threatened, and establishment fields from the AFA dataset.
#'
#' @seealso [get_ausplots()], [species_list()], [species_table()]
#'
#' @examples
#' \dontrun{
#' afa <- get_afa_static()
#' x <- invasion_info(afa_static = afa)
#'
#' vouch <- get_ausplots(veg.vouchers = TRUE)$veg.vouch
#' y <- invasion_info(
#'   ausplots = head(vouch, 100),
#'   afa_static = afa,
#'   keep_all = FALSE
#' )
#' }
#'
#' @export

invasion_info <- function(
    ausplots = NULL,
    afa_static,
    taxon_col = "herbarium_determination",
    keep_apcalign_cols = TRUE,
    keep_all = TRUE,
    remove_helper_cols = TRUE,
    clean_output = TRUE,
    ...
) {

  if (!is.data.frame(afa_static)) {
    stop("`afa_static` must be a data.frame.", call. = FALSE)
  }
  
  if (!requireNamespace("APCalign", quietly = TRUE)) {
    stop("Package `APCalign` must be installed to use `invasion_info()`.", call. = FALSE)
  }
  
  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------
  
  trim_na <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NaN", "NULL")] <- NA_character_
    x
  }
  
  clean_text <- function(x) {
    x <- as.character(x)
    x <- iconv(x, from = "", to = "UTF-8", sub = "")
    x[is.na(x)] <- NA_character_
    x <- gsub("[[:space:]]+", " ", x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }
  
  make_join_key <- function(x) {
    x <- clean_text(x)
    x <- tolower(x)
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    x <- gsub("[^a-z0-9 ]", " ", x)
    x <- gsub("[[:space:]]+", " ", x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }
  
  first_two_words <- function(x) {
    x <- clean_text(x)
    out <- ifelse(
      !is.na(x),
      vapply(strsplit(x, "\\s+"), function(z) {
        if (length(z) >= 2) paste(z[1:2], collapse = " ") else z[1]
      }, character(1)),
      NA_character_
    )
    out[out == ""] <- NA_character_
    out
  }
  
  # ---------------------------------------------------------------------------
  # Input AusPlots data
  # ---------------------------------------------------------------------------
  
  if (is.null(ausplots)) {
    if (!exists("get_ausplots", mode = "function")) {
      stop("`ausplots` is NULL, but `get_ausplots()` is not available.", call. = FALSE)
    }
    
    ausplots_obj <- get_ausplots(veg.vouchers = TRUE, ...)
    
    if (!is.list(ausplots_obj) || !"veg.vouch" %in% names(ausplots_obj)) {
      stop("`get_ausplots()` did not return a list containing `$veg.vouch`.", call. = FALSE)
    }
    
    ausplots <- ausplots_obj$veg.vouch
  }
  
  if (!is.data.frame(ausplots)) {
    stop("`ausplots` must be a data.frame or NULL.", call. = FALSE)
  }
  
  if (!taxon_col %in% names(ausplots)) {
    stop("`taxon_col` not found in `ausplots`.", call. = FALSE)
  }
  
  ausplots <- ausplots %>%
    dplyr::mutate(
      .taxon_original = as.character(.data[[taxon_col]]),
      .taxon_clean = clean_text(.data[[taxon_col]])
    ) %>%
    dplyr::filter(
      !is.na(.taxon_clean),
      lengths(strsplit(.taxon_clean, "\\s+")) >= 2
    )
  
  # ---------------------------------------------------------------------------
  # Validate AFA table (current schema)
  # ---------------------------------------------------------------------------
  
  required_afa_cols <- c(
    "scientific_name",
    "canonical_name",
    "accepted_name_usage",
    "family",
    "unified_AFA_status_continental",
    "unified_AFA_status_all_territories",
    "establishment_means",
    "degree_of_establishment",
    "declared_state_details",
    "declared_national_details",
    "threatened_state_details",
    "threatened_epbc_details",
    "status_ACT_harmonised",
    "status_NSW_harmonised",
    "status_NT_harmonised",
    "status_QLD_harmonised",
    "status_SA_harmonised",
    "status_TAS_harmonised",
    "status_VIC_harmonised",
    "status_WA_harmonised",
    "status_AR_harmonised",
    "status_CoI_harmonised",
    "status_CaI_harmonised",
    "status_ChI_harmonised",
    "status_LHI_harmonised",
    "status_NI_harmonised",
    "status_HI_harmonised",
    "status_CSI_harmonised",
    "status_MDI_harmonised",
    "griis_is_invasive"
  )
  
  missing_afa_cols <- setdiff(required_afa_cols, names(afa_static))
  if (length(missing_afa_cols) > 0) {
    stop(
      "The following required columns are missing from `afa_static`: ",
      paste(missing_afa_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # APCalign
  # ---------------------------------------------------------------------------
  
  taxa_lookup <- data.frame(
    .taxon_clean = unique(ausplots$.taxon_clean),
    stringsAsFactors = FALSE
  )
  
  aligned_raw <- APCalign::align_taxa(original_name = taxa_lookup$.taxon_clean)
  aligned_raw <- as.data.frame(aligned_raw, stringsAsFactors = FALSE)
  
  returned_cols <- names(aligned_raw)
  
  full_name_candidates <- c(
    "aligned_name",
    "accepted_name",
    "suggested_name",
    "scientific_name",
    "taxon_name"
  )
  
  full_name_col <- full_name_candidates[full_name_candidates %in% returned_cols][1]
  
  if (is.na(full_name_col) || length(full_name_col) == 0) {
    stop(
      "APCalign output does not contain a recognised aligned name column. Returned columns were: ",
      paste(returned_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  canonical_candidates <- c(
    "APC_canonical_name",
    "canonical_name",
    "scientific_name_canonical",
    "matched_canonical_name",
    "accepted_canonical_name"
  )
  
  canonical_col <- canonical_candidates[canonical_candidates %in% returned_cols][1]
  
  aligned_raw$aligned_name <- clean_text(aligned_raw[[full_name_col]])
  
  if (!is.na(canonical_col) && length(canonical_col) > 0) {
    aligned_raw$aligned_canonical <- clean_text(aligned_raw[[canonical_col]])
  } else {
    aligned_raw$aligned_canonical <- first_two_words(aligned_raw$aligned_name)
  }
  
  aligned_lookup <- cbind(taxa_lookup, aligned_raw)
  
  out <- ausplots %>%
    dplyr::left_join(aligned_lookup, by = ".taxon_clean") %>%
    dplyr::mutate(
      aligned_name = clean_text(.data[["aligned_name"]]),
      aligned_canonical = clean_text(.data[["aligned_canonical"]])
    )
  
  if (!keep_apcalign_cols) {
    base_cols <- names(ausplots)
    keep_extra <- c("aligned_name", "aligned_canonical")
    out <- out[, unique(c(base_cols, keep_extra)), drop = FALSE]
  }
  
  # ---------------------------------------------------------------------------
  # Prepare AFA lookup
  # ---------------------------------------------------------------------------
  
  afa_lookup_exact <- afa_static %>%
    dplyr::mutate(
      accepted_name_usage = clean_text(.data[["accepted_name_usage"]]),
      canonical_name = clean_text(.data[["canonical_name"]]),
      .afa_join_name = make_join_key(.data[["accepted_name_usage"]])
    ) %>%
    dplyr::transmute(
      .afa_join_name,
      accepted_name_usage,
      canonical_name,
      invasion_ACT = clean_text(.data[["status_ACT_harmonised"]]),
      invasion_NSW = clean_text(.data[["status_NSW_harmonised"]]),
      invasion_NT  = clean_text(.data[["status_NT_harmonised"]]),
      invasion_QLD = clean_text(.data[["status_QLD_harmonised"]]),
      invasion_SA  = clean_text(.data[["status_SA_harmonised"]]),
      invasion_TAS = clean_text(.data[["status_TAS_harmonised"]]),
      invasion_VIC = clean_text(.data[["status_VIC_harmonised"]]),
      invasion_WA  = clean_text(.data[["status_WA_harmonised"]]),
      
      invasion_AR  = clean_text(.data[["status_AR_harmonised"]]),
      invasion_CoI = clean_text(.data[["status_CoI_harmonised"]]),
      invasion_CaI = clean_text(.data[["status_CaI_harmonised"]]),
      invasion_ChI = clean_text(.data[["status_ChI_harmonised"]]),
      invasion_LHI = clean_text(.data[["status_LHI_harmonised"]]),
      invasion_NI  = clean_text(.data[["status_NI_harmonised"]]),
      invasion_HI  = clean_text(.data[["status_HI_harmonised"]]),
      invasion_CSI = clean_text(.data[["status_CSI_harmonised"]]),
      invasion_MDI = clean_text(.data[["status_MDI_harmonised"]]),
      
      invasion_GRIIS = ifelse(.data[["griis_is_invasive"]] %in% TRUE, "invasive", NA_character_),
      invasion_AFA_national = clean_text(.data[["unified_AFA_status_continental"]]),
      invasion_AFA_all_territories = clean_text(.data[["unified_AFA_status_all_territories"]]),
      invasion_establishment_means = clean_text(.data[["establishment_means"]]),
      invasion_degree_of_establishment = clean_text(.data[["degree_of_establishment"]]),
      
      declared_state_details = clean_text(.data[["declared_state_details"]]),
      declared_national_details = clean_text(.data[["declared_national_details"]]),
      threatened_state_details = clean_text(.data[["threatened_state_details"]]),
      threatened_epbc_details = clean_text(.data[["threatened_epbc_details"]])
    ) %>%
    dplyr::distinct(.afa_join_name, .keep_all = TRUE)
  
  afa_lookup_binom <- afa_static %>%
    dplyr::mutate(
      canonical_name = clean_text(.data[["canonical_name"]]),
      .afa_binom = make_join_key(first_two_words(.data[["canonical_name"]]))
    ) %>%
    dplyr::transmute(
      .afa_binom,
      accepted_name_usage_fb = clean_text(.data[["accepted_name_usage"]]),
      invasion_ACT_fb = clean_text(.data[["status_ACT_harmonised"]]),
      invasion_NSW_fb = clean_text(.data[["status_NSW_harmonised"]]),
      invasion_NT_fb  = clean_text(.data[["status_NT_harmonised"]]),
      invasion_QLD_fb = clean_text(.data[["status_QLD_harmonised"]]),
      invasion_SA_fb  = clean_text(.data[["status_SA_harmonised"]]),
      invasion_TAS_fb = clean_text(.data[["status_TAS_harmonised"]]),
      invasion_VIC_fb = clean_text(.data[["status_VIC_harmonised"]]),
      invasion_WA_fb  = clean_text(.data[["status_WA_harmonised"]]),
      
      invasion_AR_fb  = clean_text(.data[["status_AR_harmonised"]]),
      invasion_CoI_fb = clean_text(.data[["status_CoI_harmonised"]]),
      invasion_CaI_fb = clean_text(.data[["status_CaI_harmonised"]]),
      invasion_ChI_fb = clean_text(.data[["status_ChI_harmonised"]]),
      invasion_LHI_fb = clean_text(.data[["status_LHI_harmonised"]]),
      invasion_NI_fb  = clean_text(.data[["status_NI_harmonised"]]),
      invasion_HI_fb  = clean_text(.data[["status_HI_harmonised"]]),
      invasion_CSI_fb = clean_text(.data[["status_CSI_harmonised"]]),
      invasion_MDI_fb = clean_text(.data[["status_MDI_harmonised"]]),
      
      invasion_GRIIS_fb = ifelse(.data[["griis_is_invasive"]] %in% TRUE, "invasive", NA_character_),
      invasion_AFA_national_fb = clean_text(.data[["unified_AFA_status_continental"]]),
      invasion_AFA_all_territories_fb = clean_text(.data[["unified_AFA_status_all_territories"]]),
      invasion_establishment_means_fb = clean_text(.data[["establishment_means"]]),
      invasion_degree_of_establishment_fb = clean_text(.data[["degree_of_establishment"]]),
      
      declared_state_details_fb = clean_text(.data[["declared_state_details"]]),
      declared_national_details_fb = clean_text(.data[["declared_national_details"]]),
      threatened_state_details_fb = clean_text(.data[["threatened_state_details"]]),
      threatened_epbc_details_fb = clean_text(.data[["threatened_epbc_details"]])
    ) %>%
    dplyr::distinct(.afa_binom, .keep_all = TRUE)
  
  # ---------------------------------------------------------------------------
  # Join (exact first, then conservative binomial fallback)
  # ---------------------------------------------------------------------------
  
  out <- out %>%
    dplyr::mutate(
      .join_name = make_join_key(.data[["aligned_name"]]),
      .join_binom = make_join_key(first_two_words(.data[["aligned_canonical"]]))
    ) %>%
    dplyr::left_join(
      afa_lookup_exact,
      by = c(".join_name" = ".afa_join_name")
    ) %>%
    dplyr::left_join(
      afa_lookup_binom,
      by = c(".join_binom" = ".afa_binom")
    ) %>%
    dplyr::mutate(
      invasion_match_name = dplyr::coalesce(
        .data[["accepted_name_usage"]],
        .data[["accepted_name_usage_fb"]]
      ),
      
      invasion_ACT = dplyr::coalesce(.data[["invasion_ACT"]], .data[["invasion_ACT_fb"]]),
      invasion_NSW = dplyr::coalesce(.data[["invasion_NSW"]], .data[["invasion_NSW_fb"]]),
      invasion_NT  = dplyr::coalesce(.data[["invasion_NT"]],  .data[["invasion_NT_fb"]]),
      invasion_QLD = dplyr::coalesce(.data[["invasion_QLD"]], .data[["invasion_QLD_fb"]]),
      invasion_SA  = dplyr::coalesce(.data[["invasion_SA"]],  .data[["invasion_SA_fb"]]),
      invasion_TAS = dplyr::coalesce(.data[["invasion_TAS"]], .data[["invasion_TAS_fb"]]),
      invasion_VIC = dplyr::coalesce(.data[["invasion_VIC"]], .data[["invasion_VIC_fb"]]),
      invasion_WA  = dplyr::coalesce(.data[["invasion_WA"]],  .data[["invasion_WA_fb"]]),
      
      invasion_AR  = dplyr::coalesce(.data[["invasion_AR"]],  .data[["invasion_AR_fb"]]),
      invasion_CoI = dplyr::coalesce(.data[["invasion_CoI"]], .data[["invasion_CoI_fb"]]),
      invasion_CaI = dplyr::coalesce(.data[["invasion_CaI"]], .data[["invasion_CaI_fb"]]),
      invasion_ChI = dplyr::coalesce(.data[["invasion_ChI"]], .data[["invasion_ChI_fb"]]),
      invasion_LHI = dplyr::coalesce(.data[["invasion_LHI"]], .data[["invasion_LHI_fb"]]),
      invasion_NI  = dplyr::coalesce(.data[["invasion_NI"]],  .data[["invasion_NI_fb"]]),
      invasion_HI  = dplyr::coalesce(.data[["invasion_HI"]],  .data[["invasion_HI_fb"]]),
      invasion_CSI = dplyr::coalesce(.data[["invasion_CSI"]], .data[["invasion_CSI_fb"]]),
      invasion_MDI = dplyr::coalesce(.data[["invasion_MDI"]], .data[["invasion_MDI_fb"]]),
      
      invasion_GRIIS = dplyr::coalesce(.data[["invasion_GRIIS"]], .data[["invasion_GRIIS_fb"]]),
      invasion_AFA_national = dplyr::coalesce(.data[["invasion_AFA_national"]], .data[["invasion_AFA_national_fb"]]),
      invasion_AFA_all_territories = dplyr::coalesce(.data[["invasion_AFA_all_territories"]], .data[["invasion_AFA_all_territories_fb"]]),
      invasion_establishment_means = dplyr::coalesce(.data[["invasion_establishment_means"]], .data[["invasion_establishment_means_fb"]]),
      invasion_degree_of_establishment = dplyr::coalesce(.data[["invasion_degree_of_establishment"]], .data[["invasion_degree_of_establishment_fb"]]),
      
      declared_state_details = dplyr::coalesce(.data[["declared_state_details"]], .data[["declared_state_details_fb"]]),
      declared_national_details = dplyr::coalesce(.data[["declared_national_details"]], .data[["declared_national_details_fb"]]),
      threatened_state_details = dplyr::coalesce(.data[["threatened_state_details"]], .data[["threatened_state_details_fb"]]),
      threatened_epbc_details = dplyr::coalesce(.data[["threatened_epbc_details"]], .data[["threatened_epbc_details_fb"]]),
      
      invasion_matched =
        !is.na(.data[["invasion_AFA_national"]]) |
        !is.na(.data[["invasion_GRIIS"]])
    ) %>%
    dplyr::select(
      -accepted_name_usage,
      -accepted_name_usage_fb,
      -dplyr::ends_with("_fb")
    )
  
  # ---------------------------------------------------------------------------
  # Output trimming
  # ---------------------------------------------------------------------------
  
  if (!keep_all) {
    keep_cols <- c(
      intersect(c(taxon_col, "aligned_name", "aligned_canonical", "invasion_match_name", "invasion_matched"), names(out)),
      grep("^invasion_", names(out), value = TRUE),
      intersect(
        c(
          "declared_state_details",
          "declared_national_details",
          "threatened_state_details",
          "threatened_epbc_details"
        ),
        names(out)
      )
    )
    
    out <- out[, unique(keep_cols), drop = FALSE]
  }
  
  if (remove_helper_cols) {
    helper_cols <- intersect(c(".taxon_original", ".taxon_clean", ".join_name", ".join_binom"), names(out))
    if (length(helper_cols) > 0) {
      out <- out[, setdiff(names(out), helper_cols), drop = FALSE]
    }
  }
  
  # ---------------------------------------------------------------------------
  # Clean output
  # ---------------------------------------------------------------------------
  
  if (clean_output) {
    keep_cols <- c(
      taxon_col,
      "standardised_name",
      "aligned_name",
      "aligned_canonical",
      "invasion_ACT",
      "invasion_NSW",
      "invasion_NT",
      "invasion_QLD",
      "invasion_SA",
      "invasion_TAS",
      "invasion_VIC",
      "invasion_WA",
      "invasion_AR",
      "invasion_CoI",
      "invasion_CaI",
      "invasion_ChI",
      "invasion_LHI",
      "invasion_NI",
      "invasion_HI",
      "invasion_CSI",
      "invasion_MDI",
      "invasion_GRIIS",
      "invasion_AFA_national",
      "invasion_AFA_all_territories",
      "invasion_establishment_means",
      "invasion_degree_of_establishment",
      "invasion_match_name",
      "invasion_matched",
      "declared_state_details",
      "declared_national_details",
      "threatened_state_details",
      "threatened_epbc_details"
    )
    
    if ("is_uncertain_determination" %in% names(out)) {
      out <- out %>%
        dplyr::filter(
          !is.na(.data$is_uncertain_determination),
          .data$is_uncertain_determination == FALSE
        )
    }
    
    if (taxon_col %in% names(out)) {
      out <- out %>%
        dplyr::filter(
          !is.na(.data[[taxon_col]]),
          .data[[taxon_col]] != "Indeterminate"
        )
    }
    
    out <- out %>%
      dplyr::select(dplyr::any_of(keep_cols))
  }
  
  out
}
