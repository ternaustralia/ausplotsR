# ============================================================
# species_traits()
# Merge species list (or all species in an ausplots object)
# with a trait table supplied by the user
# ============================================================

species_traits <- function(ausplots = NULL,
                           species_list = NULL,
                           species_df = NULL,
                           species_name = c("SN", "HD")) {
  
  species_name <- match.arg(species_name)
  
  # -----------------------------
  # internal static trait table
  # -----------------------------
  trait_table <- get("trait_data_pp", envir = asNamespace("ausplotsR"))
  
  # -----------------------------
  # checks
  # -----------------------------
  if (!is.data.frame(trait_table)) {
    stop("Internal trait table `trait_data_pp` is not available.")
  }
  
  if (!"species_name" %in% names(trait_table)) {
    stop("Internal trait table must contain a column called `species_name`.")
  }
  
  # -----------------------------
  # decide where species come from
  # priority:
  # species_list > species_df > ausplots$veg.vouch
  # -----------------------------
  if (!is.null(species_list)) {
    
    sp <- unique(as.character(species_list))
    sp <- sp[!is.na(sp) & sp != ""]
    
    species_data <- data.frame(
      species_name = sp,
      stringsAsFactors = FALSE
    )
    
  } else if (!is.null(species_df)) {
    
    if (!is.data.frame(species_df)) {
      stop("`species_df` must be a data.frame.")
    }
    
    if (!"species_name" %in% names(species_df)) {
      stop("`species_df` must contain a column called `species_name`.")
    }
    
    species_data <- species_df
    species_data$species_name <- as.character(species_data$species_name)
    species_data <- species_data[!is.na(species_data$species_name) &
                                   species_data$species_name != "", , drop = FALSE]
    
  } else {
    
    if (is.null(ausplots)) {
      stop("Please provide either `species_list`, `species_df`, or `ausplots`.")
    }
    
    if (!is.list(ausplots)) {
      stop("`ausplots` must be an object returned by get_ausplots().")
    }
    
    if (is.null(ausplots$veg.vouch)) {
      stop("`ausplots$veg.vouch` is missing.")
    }
    
    species_column <- switch(
      species_name,
      SN = "standardised_name",
      HD = "herbarium_determination"
    )
    
    if (!species_column %in% names(ausplots$veg.vouch)) {
      stop("Column `", species_column, "` not found in `veg.vouch`.")
    }
    
    sp <- ausplots$veg.vouch[[species_column]]
    sp <- unique(as.character(sp))
    sp <- sp[!is.na(sp) & sp != ""]
    
    species_data <- data.frame(
      species_name = sp,
      stringsAsFactors = FALSE
    )
  }
  
  # -----------------------------
  # prepare data
  # -----------------------------
  species_data$species_name <- as.character(species_data$species_name)
  trait_table$species_name <- as.character(trait_table$species_name)
  
  species_data <- unique(species_data)
  
  # remove genus-only records
  species_data <- species_data[grepl(" ", species_data$species_name), , drop = FALSE]
  
  trait_table <- trait_table[!duplicated(trait_table$species_name), , drop = FALSE]
  
  # -----------------------------
  # merge
  # -----------------------------
  output <- merge(
    species_data,
    trait_table,
    by = "species_name",
    all.x = TRUE,
    sort = FALSE
  )
  
  # order alphabetically
  output <- output[order(output$species_name), , drop = FALSE]
  
  rownames(output) <- NULL
  
  return(output)
}
