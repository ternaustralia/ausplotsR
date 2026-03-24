# ============================================================
# biodiversity_metrics()
# Calculate biodiversity metrics for all plots or a subset
# directly from an ausplots object
#
# Uses:
#   - veg.vouchers -> presence/absence metrics
#   - veg.PI       -> point-intercept hit counts as abundance proxy
#
# Returns one row per site_unique
# ============================================================

biodiversity_metrics <- function(ausplots,
                                 plots = NULL,
                                 species_name = c("SN", "HD")) {
  
  species_name <- match.arg(species_name)
  
  # -----------------------------
  # 1. Basic checks
  # -----------------------------
  if (!is.list(ausplots)) {
    stop("`ausplots` must be an object returned by get_ausplots().")
  }
  
  if (is.null(ausplots$veg.vouch)) {
    stop("`ausplots$veg.vouchers` is missing.")
  }
  
  if (is.null(ausplots$veg.PI)) {
    stop("`ausplots$veg.PI` is missing.")
  }
  
  # -----------------------------
  # 2. Build presence/absence matrix
  #    from veg.vouchers
  # -----------------------------
  pa_matrix <- species_table(
    ausplots$veg.vouch,
    m_kind = "PA",
    species_name = species_name
  )
  
  pa_matrix <- as.data.frame(pa_matrix)
  
  if (is.null(rownames(pa_matrix))) {
    stop("Presence/absence matrix has no row names.")
  }
  
  # Optional subset of plots
  if (!is.null(plots)) {
    pa_matrix <- pa_matrix[rownames(pa_matrix) %in% plots, , drop = FALSE]
  }
  
  if (nrow(pa_matrix) == 0) {
    stop("No plots left in the presence/absence matrix after filtering.")
  }
  
  # Make sure PA matrix is numeric
  pa_matrix[] <- lapply(pa_matrix, function(x) as.numeric(as.character(x)))
  pa_matrix[is.na(pa_matrix)] <- 0
  pa_matrix[pa_matrix > 0] <- 1
  
  # Remove species absent from all selected plots
  pa_matrix <- pa_matrix[, colSums(pa_matrix) > 0, drop = FALSE]
  
  if (ncol(pa_matrix) == 0) {
    stop("No species columns with non-zero values found in presence/absence matrix.")
  }
  
  # -----------------------------
  # 3. Build abundance matrix
  #    from veg.PI point-intercept hits
  # -----------------------------
  veg_pi <- ausplots$veg.PI
  
  if (!is.null(plots)) {
    veg_pi <- veg_pi[veg_pi$site_unique %in% plots, , drop = FALSE]
  }
  
  if (nrow(veg_pi) == 0) {
    warning("No rows left in `veg.PI` after filtering. Abundance-based metrics will be returned as NA.")
    abundance_matrix <- NULL
  } else {
    
    species_column <- switch(
      species_name,
      "SN" = "standardised_name",
      "HD" = "herbarium_determination"
    )
    
    if (!species_column %in% names(veg_pi)) {
      warning("Column `", species_column, "` not found in `veg.PI`. Abundance-based metrics will be returned as NA.")
      abundance_matrix <- NULL
    } else {
      
      keep <- !is.na(veg_pi[[species_column]]) & veg_pi[[species_column]] != ""
      veg_pi <- veg_pi[keep, , drop = FALSE]
      
      if (nrow(veg_pi) == 0) {
        warning("No identified species records available in `veg.PI`. Abundance-based metrics will be returned as NA.")
        abundance_matrix <- NULL
      } else {
        hit_table <- table(veg_pi$site_unique, veg_pi[[species_column]])
        abundance_matrix <- as.data.frame.matrix(hit_table)
        abundance_matrix[] <- lapply(abundance_matrix, as.numeric)
      }
    }
  }
  
  # -----------------------------
  # 4. Presence/absence metrics
  # -----------------------------
  
  # Species richness
  species_richness <- rowSums(pa_matrix)
  
  # Frequency of each species across plots
  species_frequency <- colSums(pa_matrix)
  
  # Range Rarity Richness (RRR)
  rarity_matrix <- sweep(pa_matrix, 2, species_frequency, FUN = "/")
  RRR <- rowSums(rarity_matrix, na.rm = TRUE)
  
  # Corrected Weighted Endemism (CWE)
  CWE <- RRR / species_richness
  CWE[species_richness == 0] <- NA_real_
  
  # -----------------------------
  # 5. Abundance-based metrics
  # -----------------------------
  total_abundance <- rep(NA_real_, nrow(pa_matrix))
  shannon <- rep(NA_real_, nrow(pa_matrix))
  simpson <- rep(NA_real_, nrow(pa_matrix))
  pielou_evenness <- rep(NA_real_, nrow(pa_matrix))
  
  names(total_abundance) <- rownames(pa_matrix)
  names(shannon) <- rownames(pa_matrix)
  names(simpson) <- rownames(pa_matrix)
  names(pielou_evenness) <- rownames(pa_matrix)
  
  if (!is.null(abundance_matrix)) {
    
    # Keep only plots shared with PA matrix
    shared_plots <- intersect(rownames(pa_matrix), rownames(abundance_matrix))
    
    if (length(shared_plots) == 0) {
      warning("No shared plots between presence/absence and abundance matrices. Abundance-based metrics returned as NA.")
    } else {
      
      abundance_matrix <- abundance_matrix[shared_plots, , drop = FALSE]
      
      # If species sets differ, keep abundance species as they are
      # because abundance metrics are based on point-intercept structure
      abundance_matrix[] <- lapply(abundance_matrix, function(x) as.numeric(as.character(x)))
      abundance_matrix[is.na(abundance_matrix)] <- 0
      
      total_abundance[shared_plots] <- rowSums(abundance_matrix)
      
      shannon[shared_plots] <- apply(abundance_matrix, 1, function(x) {
        x <- x[x > 0]
        if (length(x) == 0) return(NA_real_)
        p <- x / sum(x)
        -sum(p * log(p))
      })
      
      simpson[shared_plots] <- apply(abundance_matrix, 1, function(x) {
        x <- x[x > 0]
        if (length(x) == 0) return(NA_real_)
        p <- x / sum(x)
        1 - sum(p^2)
      })
      
      # Pielou evenness uses Shannon from abundance
      # and richness from the PA matrix
      pielou_evenness[shared_plots] <- shannon[shared_plots] / log(species_richness[shared_plots])
      pielou_evenness[species_richness <= 1] <- NA_real_
    }
  }
  
  # -----------------------------
  # 6. Output table
  # -----------------------------
  output <- data.frame(
    site_unique = rownames(pa_matrix),
    species_richness = as.numeric(species_richness),
    total_abundance = as.numeric(total_abundance[rownames(pa_matrix)]),
    shannon = as.numeric(shannon[rownames(pa_matrix)]),
    simpson = as.numeric(simpson[rownames(pa_matrix)]),
    pielou_evenness = as.numeric(pielou_evenness[rownames(pa_matrix)]),
    RRR = as.numeric(RRR),
    CWE = as.numeric(CWE),
    stringsAsFactors = FALSE
  )
  
  rownames(output) <- NULL
  
  return(output)
}
