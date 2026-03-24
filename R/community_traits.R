# ============================================================
# community_traits()
# Calculate plot-level community trait metrics from AusPlots
#
# Output includes:
# - Community-weighted means (CWMs) for numeric traits
# - Proportions of photosynthetic pathway classes
#   (C3-CAM counted as C3; C4-CAM counted as C4; unknown omitted)
# - Functional diversity metrics:
#     FRic  = Functional richness
#     FEve  = Functional evenness
#     FDis  = Functional dispersion
#     RaoQ  = Rao's quadratic entropy
# - Functional redundancy and uniqueness from adiv
#
# Traits are taken from the internal dataset: trait_data_pp
# Abundances are based on point-intercept hit counts from veg.PI
# ============================================================

community_traits <- function(ausplots,
                             plots = NULL,
                             species_name = c("SN", "HD"),
                             cwm_traits = NULL,
                             fd_traits = c("max_height", "seed_dry_mass", "leaf_mass_per_area"),
                             categorical_trait = "photosynthetic_pathway") {
  
  species_name <- match.arg(species_name)
  
  # -----------------------------
  # 1. checks
  # -----------------------------
  if (!is.list(ausplots)) {
    stop("`ausplots` must be an object returned by get_ausplots().")
  }
  
  if (is.null(ausplots$veg.PI)) {
    stop("`ausplots$veg.PI` is missing.")
  }
  
  if (!exists("trait_data_pp", inherits = TRUE)) {
    stop("Internal trait dataset `trait_data_pp` is not available.")
  }
  
  if (!requireNamespace("FD", quietly = TRUE)) {
    stop("Package `FD` is required. Please install it.")
  }
  
  if (!requireNamespace("adiv", quietly = TRUE)) {
    stop("Package `adiv` is required. Please install it.")
  }
  
  trait_table <-  get("trait_data_pp", envir = asNamespace("ausplotsR"))
  
  if (!is.data.frame(trait_table)) {
    stop("`trait_data_pp` must be a data.frame.")
  }
  
  if (!"species_name" %in% names(trait_table)) {
    stop("`trait_data_pp` must contain a column called `species_name`.")
  }
  
  if (!is.null(categorical_trait) && !categorical_trait %in% names(trait_table)) {
    stop("Categorical trait column `", categorical_trait, "` not found in `trait_data_pp`.")
  }
  
  # -----------------------------
  # 2. choose species column in veg.PI
  # -----------------------------
  species_column <- switch(
    species_name,
    SN = "standardised_name",
    HD = "herbarium_determination"
  )
  
  if (!species_column %in% names(ausplots$veg.PI)) {
    stop("Column `", species_column, "` not found in `veg.PI`.")
  }
  
  veg_pi <- ausplots$veg.PI
  
  if (!is.null(plots)) {
    veg_pi <- veg_pi[veg_pi$site_unique %in% plots, , drop = FALSE]
  }
  
  if (nrow(veg_pi) == 0) {
    stop("No rows left in `veg.PI` after filtering.")
  }
  
  # -----------------------------
  # 3. keep valid species only
  # -----------------------------
  veg_pi$species_name <- as.character(veg_pi[[species_column]])
  veg_pi <- veg_pi[!is.na(veg_pi$species_name) & veg_pi$species_name != "", , drop = FALSE]
  
  # remove genus-only records
  veg_pi <- veg_pi[grepl(" ", veg_pi$species_name), , drop = FALSE]
  
  if (nrow(veg_pi) == 0) {
    stop("No valid species-level records left in `veg.PI`.")
  }
  
  # -----------------------------
  # 4. abundance matrix from point-intercept hits
  # -----------------------------
  abundance_tab <- table(veg_pi$site_unique, veg_pi$species_name)
  abundance_matrix <- as.data.frame.matrix(abundance_tab)
  abundance_matrix[] <- lapply(abundance_matrix, as.numeric)
  
  if (ncol(abundance_matrix) == 0) {
    stop("No species columns available in abundance matrix.")
  }
  
  # -----------------------------
  # 5. align abundance matrix with trait table
  # -----------------------------
  trait_table$species_name <- as.character(trait_table$species_name)
  trait_table <- trait_table[!duplicated(trait_table$species_name), , drop = FALSE]
  
  shared_species <- intersect(colnames(abundance_matrix), trait_table$species_name)
  
  if (length(shared_species) == 0) {
    stop("No species in `veg.PI` matched the internal trait table.")
  }
  
  abundance_matrix <- abundance_matrix[, shared_species, drop = FALSE]
  trait_sub <- trait_table[match(shared_species, trait_table$species_name), , drop = FALSE]
  
  # -----------------------------
  # 6. choose traits
  # -----------------------------
  all_numeric_traits <- names(trait_sub)[sapply(trait_sub, is.numeric)]
  all_numeric_traits <- setdiff(all_numeric_traits, c("species_name", categorical_trait))
  
  # CWM traits = all numeric by default
  if (is.null(cwm_traits)) {
    cwm_traits <- all_numeric_traits
  } else {
    cwm_traits <- cwm_traits[cwm_traits %in% names(trait_sub)]
  }
  
  # FD traits = selected core traits only
  fd_traits <- fd_traits[fd_traits %in% names(trait_sub)]
  
  # -----------------------------
  # 7. initialise output
  # -----------------------------
  output <- data.frame(
    site_unique = rownames(abundance_matrix),
    stringsAsFactors = FALSE
  )
  
  # -----------------------------
  # 8. community-weighted means (CWMs)
  # -----------------------------
  if (length(cwm_traits) > 0) {
    for (tr in cwm_traits) {
      trait_vals <- trait_sub[[tr]]
      
      weighted_sum <- apply(abundance_matrix, 1, function(x) {
        keep <- !is.na(trait_vals)
        if (!any(keep)) return(NA_real_)
        sum(x[keep] * trait_vals[keep], na.rm = TRUE)
      })
      
      weighted_total <- apply(abundance_matrix, 1, function(x) {
        keep <- !is.na(trait_vals)
        if (!any(keep)) return(NA_real_)
        sum(x[keep], na.rm = TRUE)
      })
      
      output[[paste0("CWM_", tr)]] <- as.numeric(weighted_sum / weighted_total)
    }
  }
  
  # -----------------------------
  # 9. proportions for photosynthetic pathway
  #     only prop_C3 and prop_C4 returned
  #     C3-CAM -> C3
  #     C4-CAM -> C4
  # -----------------------------
  output$prop_C3 <- NA_real_
  output$prop_C4 <- NA_real_
  
  if (!is.null(categorical_trait)) {
    cat_vals <- as.character(trait_sub[[categorical_trait]])
    cat_vals[is.na(cat_vals) | cat_vals == ""] <- "unknown"
    cat_vals[cat_vals == "C3-CAM"] <- "C3"
    cat_vals[cat_vals == "C4-CAM"] <- "C4"
    
    output$prop_C3 <- apply(abundance_matrix, 1, function(x) {
      total_hits <- sum(x, na.rm = TRUE)
      if (total_hits == 0) return(NA_real_)
      sum(x[cat_vals == "C3"], na.rm = TRUE) / total_hits
    })
    
    output$prop_C4 <- apply(abundance_matrix, 1, function(x) {
      total_hits <- sum(x, na.rm = TRUE)
      if (total_hits == 0) return(NA_real_)
      sum(x[cat_vals == "C4"], na.rm = TRUE) / total_hits
    })
  }
  
  # -----------------------------
  # 10. initialise functional outputs
  # -----------------------------
  output$FRic <- NA_real_
  output$FEve <- NA_real_
  output$FDis <- NA_real_
  output$RaoQ <- NA_real_
  output$functional_redundancy <- NA_real_
  output$functional_uniqueness <- NA_real_
  
  # -----------------------------
  # 11. functional diversity / redundancy / uniqueness
  #     use only fd_traits
  # -----------------------------
  if (length(fd_traits) > 0) {
    
    trait_fd <- trait_sub[, c("species_name", fd_traits), drop = FALSE]
    
    # keep only species with complete trait data
    complete_trait_rows <- complete.cases(trait_fd[, fd_traits, drop = FALSE])
    trait_fd <- trait_fd[complete_trait_rows, , drop = FALSE]
    
    if (nrow(trait_fd) >= 2) {
      rownames(trait_fd) <- trait_fd$species_name
      trait_fd$species_name <- NULL
      
      shared_species_fd <- intersect(colnames(abundance_matrix), rownames(trait_fd))
      
      if (length(shared_species_fd) >= 2) {
        abundance_fd <- abundance_matrix[, shared_species_fd, drop = FALSE]
        trait_fd <- trait_fd[shared_species_fd, , drop = FALSE]
        
        # remove species absent from all communities
        abundance_fd <- abundance_fd[, colSums(abundance_fd) > 0, drop = FALSE]
        trait_fd <- trait_fd[colnames(abundance_fd), , drop = FALSE]
        
        # keep plots with at least 2 species
        plots_ok <- rowSums(abundance_fd > 0) >= 2
        
        if (any(plots_ok) && ncol(abundance_fd) >= 2) {
          
          abundance_fd_ok <- abundance_fd[plots_ok, , drop = FALSE]
          
          # remove zero-sum species again after plot filtering
          abundance_fd_ok <- abundance_fd_ok[, colSums(abundance_fd_ok) > 0, drop = FALSE]
          trait_fd_ok <- trait_fd[colnames(abundance_fd_ok), , drop = FALSE]
          
          # keep only plots still with at least 2 species
          plots_ok2 <- rowSums(abundance_fd_ok > 0) >= 2
          abundance_fd_ok <- abundance_fd_ok[plots_ok2, , drop = FALSE]
          
          if (nrow(abundance_fd_ok) >= 1 && ncol(abundance_fd_ok) >= 2) {
            
            abundance_fd_ok <- abundance_fd_ok[, colSums(abundance_fd_ok) > 0, drop = FALSE]
            trait_fd_ok <- trait_fd_ok[colnames(abundance_fd_ok), , drop = FALSE]
            
            # FD metrics
            fd_results <- tryCatch(
              FD::dbFD(
                x = trait_fd_ok,
                a = abundance_fd_ok,
                calc.FRic = TRUE,
                calc.FDiv = FALSE,
                calc.CWM = FALSE,
                stand.x = FALSE,
                corr = "cailliez",
                messages = FALSE
              ),
              error = function(e) NULL
            )
            
            if (!is.null(fd_results)) {
              ok_sites <- rownames(abundance_fd_ok)
              
              if (!is.null(fd_results$FRic)) {
                output$FRic[match(ok_sites, output$site_unique)] <- as.numeric(fd_results$FRic)
              }
              if (!is.null(fd_results$FEve)) {
                output$FEve[match(ok_sites, output$site_unique)] <- as.numeric(fd_results$FEve)
              }
              if (!is.null(fd_results$FDis)) {
                output$FDis[match(ok_sites, output$site_unique)] <- as.numeric(fd_results$FDis)
              }
              if (!is.null(fd_results$RaoQ)) {
                output$RaoQ[match(ok_sites, output$site_unique)] <- as.numeric(fd_results$RaoQ)
              }
            }
            
            # distance matrix for adiv
            dis_fd <- tryCatch(
              FD::gowdis(trait_fd_ok),
              error = function(e) NULL
            )
            
            if (!is.null(dis_fd)) {
              adiv_results <- tryCatch(
                adiv::uniqueness(
                  comm = abundance_fd_ok,
                  dis = dis_fd,
                  abundance = TRUE
                ),
                error = function(e) NULL
              )
              
              if (!is.null(adiv_results) && "red" %in% names(adiv_results)) {
                red_df <- adiv_results$red
                
                if ("R" %in% names(red_df)) {
                  output$functional_redundancy[match(rownames(red_df), output$site_unique)] <- as.numeric(red_df$R)
                }
                if ("U" %in% names(red_df)) {
                  output$functional_uniqueness[match(rownames(red_df), output$site_unique)] <- as.numeric(red_df$U)
                }
              }
            }
          }
        }
      }
    }
  }
  
  # -----------------------------
  # 12. order output by site_unique
  # -----------------------------
  output <- output[order(output$site_unique), , drop = FALSE]
  rownames(output) <- NULL
  
  return(output)
}
