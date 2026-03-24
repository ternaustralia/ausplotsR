point_intercept_visual <- function(ausplots,
                                   plots,
                                   visual = c("substrate"),
                                   height_growth_form = FALSE,
                                   height_dominant_species = FALSE,
                                   top_n_species = 5,
                                   file = NULL,
                                   width = 12,
                                   height = 10) {
  
  # ----------------------------
  # checks
  # ----------------------------
  if (missing(plots) || is.null(plots) || length(plots) == 0) {
    stop("Please provide one or more plot IDs in `plots`.")
  }
  
  if (is.null(ausplots$veg.PI)) stop("ausplots$veg.PI is missing.")
  if (is.null(ausplots$site.info)) stop("ausplots$site.info is missing.")
  
  if (!"plot_is_100m_by_100m" %in% names(ausplots$site.info)) {
    stop("ausplots$site.info$plot_is_100m_by_100m is missing.")
  }
  
  if (!"site_location_name" %in% names(ausplots$site.info)) {
    stop("ausplots$site.info$site_location_name is missing.")
  }
  
  allowed_visuals <- c("substrate", "growth_form", "dominant_species")
  if (!all(visual %in% allowed_visuals)) {
    stop("`visual` must be one or more of: ",
         paste(allowed_visuals, collapse = ", "))
  }
  
  # ----------------------------
  # helper functions
  # ----------------------------
  draw_frame <- function() {
    segments(0, 0, 0, 100, lty = 2)
    segments(100, 0, 100, 100, lty = 2)
    segments(0, 0, 100, 0, lty = 2)
    segments(0, 100, 100, 100, lty = 2)
  }
  
  height_classes <- function(h) {
    h_class <- rep(NA, length(h))
    h_class[h < 3] <- "<3 m"
    h_class[h >= 3 & h < 6] <- "3-6 m"
    h_class[h >= 6 & h < 10] <- "6-10 m"
    h_class[h >= 10] <- ">10 m"
    
    factor(h_class, levels = c("<3 m", "3-6 m", "6-10 m", ">10 m"))
  }
  
  height_sizes <- c(
    "<3 m" = 0.6,
    "3-6 m" = 1.2,
    "6-10 m" = 1.8,
    ">10 m" = 2.4
  )
  
  # ----------------------------
  # filter to 100 x 100 plots
  # ----------------------------
  sites_100 <- ausplots$site.info$site_location_name[
    !is.na(ausplots$site.info$plot_is_100m_by_100m) &
      ausplots$site.info$plot_is_100m_by_100m
  ]
  
  pi_data <- subset(ausplots$veg.PI, site_location_name %in% sites_100)
  pi_data <- subset(pi_data, site_unique %in% plots)
  
  if (nrow(pi_data) == 0) {
    stop("No point intercept data left after filtering.")
  }
  
  message("Plots being drawn:")
  print(unique(pi_data$site_unique))
  
  # ----------------------------
  # standardise transect direction
  # ----------------------------
  pi_data$transect_direction <- gsub("[[:digit:]]+", "", pi_data$transect)
  pi_data$transect_number <- as.numeric(gsub(".*?([0-9]+).*", "\\1", pi_data$transect))
  
  pi_data$transect_direction2 <- NA
  pi_data$point_number2 <- NA
  pi_data$X_plot <- NA
  pi_data$Y_plot <- NA
  
  for (i in 1:nrow(pi_data)) {
    
    if (pi_data$transect_direction[i] == "E-W") {
      pi_data$point_number2[i] <- 101 - pi_data$point_number[i]
      pi_data$transect_direction2[i] <- "W-E"
    }
    
    if (pi_data$transect_direction[i] == "W-E") {
      pi_data$point_number2[i] <- pi_data$point_number[i]
      pi_data$transect_direction2[i] <- "W-E"
    }
    
    if (pi_data$transect_direction[i] == "N-S") {
      pi_data$point_number2[i] <- pi_data$point_number[i]
      pi_data$transect_direction2[i] <- "N-S"
    }
    
    if (pi_data$transect_direction[i] == "S-N") {
      pi_data$point_number2[i] <- 101 - pi_data$point_number[i]
      pi_data$transect_direction2[i] <- "N-S"
    }
  }
  
  # ----------------------------
  # assign plot coordinates
  # ----------------------------
  for (i in 1:nrow(pi_data)) {
    
    if (pi_data$transect_direction2[i] == "W-E") {
      if (pi_data$transect_number[i] == 1) {
        pi_data$Y_plot[i] <- 10
        pi_data$X_plot[i] <- pi_data$point_number2[i]
      }
      if (pi_data$transect_number[i] == 2) {
        pi_data$Y_plot[i] <- 30
        pi_data$X_plot[i] <- pi_data$point_number2[i]
      }
      if (pi_data$transect_number[i] == 3) {
        pi_data$Y_plot[i] <- 50
        pi_data$X_plot[i] <- pi_data$point_number2[i]
      }
      if (pi_data$transect_number[i] == 4) {
        pi_data$Y_plot[i] <- 70
        pi_data$X_plot[i] <- pi_data$point_number2[i]
      }
      if (pi_data$transect_number[i] == 5) {
        pi_data$Y_plot[i] <- 90
        pi_data$X_plot[i] <- pi_data$point_number2[i]
      }
    }
    
    if (pi_data$transect_direction2[i] == "N-S") {
      if (pi_data$transect_number[i] == 1) {
        pi_data$X_plot[i] <- 10
        pi_data$Y_plot[i] <- pi_data$point_number2[i]
      }
      if (pi_data$transect_number[i] == 2) {
        pi_data$X_plot[i] <- 30
        pi_data$Y_plot[i] <- pi_data$point_number2[i]
      }
      if (pi_data$transect_number[i] == 3) {
        pi_data$X_plot[i] <- 50
        pi_data$Y_plot[i] <- pi_data$point_number2[i]
      }
      if (pi_data$transect_number[i] == 4) {
        pi_data$X_plot[i] <- 70
        pi_data$Y_plot[i] <- pi_data$point_number2[i]
      }
      if (pi_data$transect_number[i] == 5) {
        pi_data$X_plot[i] <- 90
        pi_data$Y_plot[i] <- pi_data$point_number2[i]
      }
    }
  }
  
  # ----------------------------
  # open pdf only if requested
  # ----------------------------
  if (!is.null(file)) {
    pdf(file, width = width, height = height)
    on.exit(dev.off(), add = TRUE)
  }
  
  site_ids <- unique(pi_data$site_unique)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  # ----------------------------
  # plot each selected plot
  # ----------------------------
  for (sid in site_ids) {
    
    this_plot <- subset(pi_data, site_unique == sid)
    n_panels <- length(visual)
    
    layout(matrix(seq_len(n_panels * 2), ncol = 2, byrow = TRUE),
           widths = c(3.6, 2.0))
    
    for (v in visual) {
      
      # ---------------- substrate ----------------
      if (v == "substrate") {
        
        grp <- as.factor(this_plot$substrate)
        
        par(mar = c(4, 4, 3, 1), pty = "s")
        plot(this_plot$X_plot, this_plot$Y_plot,
             pch = 16,
             col = grp,
             cex = 0.7,
             main = paste(sid, "- Substrate"),
             xlab = "X-axis (m)",
             ylab = "Y-axis (m)",
             xlim = c(0, 100),
             ylim = c(0, 100),
             xaxs = "i",
             yaxs = "i")
        draw_frame()
        
        par(mar = c(1, 0, 1, 0), pty = "m")
        plot.new()
        legend("topleft",
               title = "Substrate",
               legend = levels(grp),
               pch = 16,
               col = seq_along(levels(grp)),
               bty = "n",
               cex = 0.8,
               y.intersp = 1.2)
      }
      
      # ---------------- growth form ----------------
      if (v == "growth_form") {
        
        gf_plot <- this_plot
        
        if ("height" %in% names(gf_plot) &&
            height_growth_form &&
            !all(is.na(gf_plot$height))) {
          gf_plot$h_class <- height_classes(gf_plot$height)
          gf_cex <- height_sizes[as.character(gf_plot$h_class)]
          gf_cex[is.na(gf_cex)] <- 0.8
        } else {
          gf_cex <- 0.8
        }
        
        grp <- as.factor(gf_plot$growth_form)
        
        par(mar = c(4, 4, 3, 1), pty = "s")
        plot(gf_plot$X_plot, gf_plot$Y_plot,
             pch = 16,
             col = grp,
             cex = gf_cex,
             main = paste(sid, "- Growth form"),
             xlab = "X-axis (m)",
             ylab = "Y-axis (m)",
             xlim = c(0, 100),
             ylim = c(0, 100),
             xaxs = "i",
             yaxs = "i")
        draw_frame()
        
        par(mar = c(1, 0, 1, 0), pty = "m")
        plot.new()
        usr <- par("usr")
        
        gf_leg <- legend(x = usr[1], y = usr[4],
                         legend = levels(grp),
                         title = "Growth form",
                         pch = 16,
                         col = seq_along(levels(grp)),
                         bty = "n",
                         cex = 0.75,
                         xjust = 0,
                         yjust = 1,
                         plot = FALSE)
        
        legend(x = usr[1], y = usr[4],
               legend = levels(grp),
               title = "Growth form",
               pch = 16,
               col = seq_along(levels(grp)),
               bty = "n",
               cex = 0.75,
               xjust = 0,
               yjust = 1,
               y.intersp = 1.2)
        
        if ("height" %in% names(gf_plot) &&
            height_growth_form &&
            !all(is.na(gf_plot$height))) {
          
          y_height <- gf_leg$rect$top - gf_leg$rect$h - 0.05 * diff(usr[3:4])
          
          legend(x = usr[1], y = y_height,
                 legend = names(height_sizes),
                 title = "Height (m)",
                 pch = 16,
                 pt.cex = height_sizes,
                 bty = "n",
                 cex = 0.8,
                 xjust = 0,
                 yjust = 1.5,
                 y.intersp = 1.5,
                 x.intersp = 1.3)
        }
      }
      
      # ---------------- dominant species ----------------
      if (v == "dominant_species") {
        
        sp_plot <- this_plot
        
        sp_freq <- sort(table(sp_plot$herbarium_determination), decreasing = TRUE)
        n_species <- length(sp_freq)
        top_n <- min(top_n_species, n_species)
        
        keep_species <- names(sp_freq)[1:top_n]
        sp_plot <- subset(sp_plot, herbarium_determination %in% keep_species)
        
        if (nrow(sp_plot) > 0) {
          
          if ("height" %in% names(sp_plot) &&
              height_dominant_species &&
              !all(is.na(sp_plot$height))) {
            sp_plot$h_class <- height_classes(sp_plot$height)
            sp_cex <- height_sizes[as.character(sp_plot$h_class)]
            sp_cex[is.na(sp_cex)] <- 0.8
          } else {
            sp_cex <- 0.8
          }
          
          grp <- as.factor(sp_plot$herbarium_determination)
          
          par(mar = c(4, 4, 3, 1), pty = "s")
          plot(sp_plot$X_plot, sp_plot$Y_plot,
               pch = 16,
               col = grp,
               cex = sp_cex,
               main = paste(sid, "- Dominant species"),
               xlab = "X-axis (m)",
               ylab = "Y-axis (m)",
               xlim = c(0, 100),
               ylim = c(0, 100),
               xaxs = "i",
               yaxs = "i")
          draw_frame()
          
          par(mar = c(1, 0, 1, 0), pty = "m")
          plot.new()
          usr <- par("usr")
          
          sp_leg <- legend(x = usr[1], y = usr[4],
                           legend = levels(grp),
                           title = "Dominant species",
                           pch = 16,
                           col = seq_along(levels(grp)),
                           bty = "n",
                           cex = 0.75,
                           xjust = 0,
                           yjust = 1,
                           plot = FALSE)
          
          legend(x = usr[1], y = usr[4],
                 legend = levels(grp),
                 title = "Dominant species",
                 pch = 16,
                 col = seq_along(levels(grp)),
                 bty = "n",
                 cex = 0.75,
                 xjust = 0,
                 yjust = 1,
                 y.intersp = 1.2)
          
          if ("height" %in% names(sp_plot) &&
              height_dominant_species &&
              !all(is.na(sp_plot$height))) {
            
            y_height <- sp_leg$rect$top - sp_leg$rect$h - 0.05 * diff(usr[3:4])
            
            legend(x = usr[1], y = y_height,
                   legend = names(height_sizes),
                   title = "Height (m)",
                   pch = 16,
                   pt.cex = height_sizes,
                   bty = "n",
                   cex = 0.8,
                   xjust = 0,
                   yjust = 1.5,
                   y.intersp = 1.5,
                   x.intersp = 1.3)
          }
          
        } else {
          
          par(mar = c(4, 4, 3, 1), pty = "s")
          plot.new()
          text(0.5, 0.5, paste("No dominant species data for", sid))
          
          par(mar = c(1, 0, 1, 0), pty = "m")
          plot.new()
        }
      }
    }
  }
  invisible(pi_data)
}

