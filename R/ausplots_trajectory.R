ausplots_trajectory <- function(my.ausplots.object, choices=c("PCoA", "diversity"), min.revisits=3, plot_select=NULL, outfile=NULL) {
  
  #############################
  #reset graphical defaults
  oldpar <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(oldpar)))
  #################################################
  #check inputs
  if(missing(my.ausplots.object)) {
    stop("my.ausplots.object must be specified, see get_ausplots")
    }
  
  if(!inherits(my.ausplots.object, "list")) {
    stop("my.ausplots.object must be a list containing $site.info and $veg.PI, see get_ausplots")
  }
  
  if(!is.null(plot_select)) {
    if(!inherits(plot_select, "character")) {
      stop("plot_select must be a character vector of site names (site_location_name in $site.info).")
    }
    match_result <- any(plot_select %in% my.ausplots.object$site.info$site_location_name)
    if(!match_result) {
      stop("No entries in plot_select were found in my.ausplots.object.")
    }
    match_result <- all(plot_select %in% my.ausplots.object$site.info$site_location_name)
    if(!match_result) {
      warning("plot_select included entries that weren't found in my.ausplots.object.")
    }
  }
  
  if(!any(c("PCoA", "diversity") %in% choices)) {
    stop("choices must include at least one of PCoA or diversity.")
  }
  ###############################################
  
  #remove sites with less than min.revisits
  my.ausplots.object$site.info <- subset(my.ausplots.object$site.info, site_location_name %in% subset(my.ausplots.object$site.info, visit_number >= min.revisits)$site_location_name)
  if(nrow(my.ausplots.object$site.info) == 0) {stop("No sites have at least ", min.revisits, " visits. Try a different selection of sites or reduce min.revisits")}
  
  if("PCoA" %in% choices | !"community.indices" %in% names(my.ausplots.object)) {
  #occurrence matrix
  diss <- species_table(my.ausplots.object$veg.PI, m_kind="percent_cover", cover_type="PFC", species_name="SN")
  
  #remove sites not in revisited sites table and then species with no presence
  diss <- diss[which(rownames(diss) %in% my.ausplots.object$site.info$site_unique),]
  diss <- diss[,colSums(diss) > 0]
  
  #remove sites not in veg data
  my.ausplots.object$site.info <- my.ausplots.object$site.info[which(my.ausplots.object$site.info$site_unique %in% rownames(diss)),]
  } #close if PcoA or no community.indices in data
  
  ###
  ###############################################
  #start plot
  if(missing(outfile)) {
    outfile = paste("ausplots_trajectory_plots", format(Sys.time(), "%b_%d_%Y"), ".pdf")
  }
  
  #####################################################	
  pdf(outfile, width=8, height=6)
  
  message("Generating plots...")
  
#####################################  
  
  if("PCoA" %in% choices) {
    
    dissdiss <- vegan::vegdist(diss)
    
    par(mar=c(4,4,1,7), xpd=TRUE)
    
    if(is.null(plot_select)) {
      trajectoryPCoA_ausplots(dissdiss, sites=my.ausplots.object$site.info$site_location_name, 
  surveys=my.ausplots.object$site.info$visit_number, selection=NULL, axes = c(1, 2), 
  survey.labels=FALSE, lwd=2, lty=1, angle=12, length=0.18, 
  traj.colors=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))))
      legend("topright", inset=c(-0.2, 0), legend=unique(my.ausplots.object$site.info$site_location_name), lty=1, lwd=3, col=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))), cex=0.7, bty="n")
      } #end not select
    
    if(!is.null(plot_select)) {
      trajectoryPCoA_ausplots(dissdiss, sites=my.ausplots.object$site.info$site_location_name, 
  surveys=my.ausplots.object$site.info$visit_number, selection=my.ausplots.object$site.info$site_location_name %in% plot_select, axes = c(1, 2), survey.labels=FALSE, lwd=2, lty=1, angle=12, length=0.18, traj.colors=rainbow(length(plot_select)))
      legend("topright", inset=c(-0.2, 0), legend=plot_select, lty=1, lwd=3, col=rainbow(length(plot_select)), cex=0.7, bty="n")
      } #end plot select
    
    } #end if PCoA

  ###
  
  if("diversity" %in% choices) {
    
    if("community.indices" %in% names(my.ausplots.object)) {
      DIV <- my.ausplots.object$community.indices
      DIV <- DIV[which(DIV$site_unique %in% my.ausplots.object$site.info$site_unique),]
    } else {
      DIV <- data.frame(site_unique=rownames(diss), Shannon_diversity=vegan::diversity(diss), Species_richness=vegan::specnumber(diss))
    }
    
    #biplot routine:
    numberOFvariables <- length(names(DIV)) 
    if(numberOFvariables == 3) {
     
      # if(length(choices) > 1) {
      #  dev.new()
      # }
      
      par(mar=c(4,4,1,7), xpd=TRUE)
      
      if(is.null(plot_select)) {
        trajectoryPlot_ausplots(DIV[,-which(names(DIV) == "site_unique")], sites=my.ausplots.object$site.info$site_location_name, surveys=my.ausplots.object$site.info$visit_number, selection=NULL, axes = c(2, 1), survey.labels=FALSE, lwd=2, lty=1, angle=12, length=0.18, traj.colors=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))), axis.x=names(DIV)[3], axis.y=names(DIV)[2])
        legend("topright", inset=c(-0.2, 0), legend=unique(my.ausplots.object$site.info$site_location_name), lty=1, lwd=3, col=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))), cex=0.7, bty="n")
        } #end not select
      
      if(!is.null(plot_select)) {
        trajectoryPlot_ausplots(DIV[,-which(names(DIV) == "site_unique")], sites=my.ausplots.object$site.info$site_location_name, surveys=my.ausplots.object$site.info$visit_number, selection=plot_select, axes = c(2, 1), survey.labels=FALSE, lwd=2, lty=1, angle=12, length=0.18, traj.colors=rainbow(length(plot_select)), , axis.x=names(DIV)[3], axis.y=names(DIV)[2])
        legend("topright", inset=c(-0.2, 0), legend=plot_select, lty=1, lwd=3, col=rainbow(length(plot_select)), cex=0.7, bty="n")
        } #end plot select
      
    } #close if NumberOFvariables == 3
    
    
    ###univariate
    
    # if(length(choices) > 1 | numberOFvariables == 3) {
    #   dev.new()
    # }
   
    DIV$year <- as.character(my.ausplots.object$site.info$visit_date[match(DIV$site_unique, my.ausplots.object$site.info$site_unique)])
    DIV$year <- as.numeric(unlist(lapply(strsplit(DIV$year, "-"), function(x) paste(x[[1]]))))
    DIV$site_location_name <- my.ausplots.object$site.info$site_location_name[match(DIV$site_unique, my.ausplots.object$site.info$site_unique)]
      
      if(!is.null(plot_select)) {
        DIV <- subset(DIV, site_location_name %in% plot_select)
      } #cls !is.null diversity uni
    
    par(mfrow=c(2,2))
    par(mar=c(4,4,1,7), xpd=TRUE)  
    
      for(j in names(DIV)[-which(names(DIV) %in% c("site_unique", "site_location_name", "year"))]) {
        
        plot(NULL, xlim=c(min(DIV$year), max(DIV$year)), ylim=c(min(DIV[,j]), max(DIV[,j])), ylab=j, xlab="Year", las=1)
      
        n <- 0
        line.cols <- rainbow(length(unique(DIV$site_location_name)))
        for(i in unique(DIV$site_location_name)) {
          plot.DIV <- subset(DIV, site_location_name == i)
          n <- n + 1
          points(plot.DIV$year, plot.DIV[,j], type="b", col=line.cols[n])
        }
      
        legend("topright", inset=c(-0.4,0), legend=unique(DIV$site_location_name), lty=1, lwd=3, col=rainbow(length(unique(DIV$site_location_name))), cex=0.6, bty="n")
      
      } #end for j
    
    } #end if diversity

  dev.off()
  message("Plots written to file.")
  
  
} #end function