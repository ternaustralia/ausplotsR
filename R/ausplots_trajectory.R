ausplots_trajectory <- function(my.ausplots.object, choices=c("PCoA", "diversity"), min.revisits=3, plot_select=NULL) {
  
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
    match_result <- all(plot_select %in% my.ausplots.object$site.info$site_location_name)
    if(!match_result) {
      warning("plot_select included entries that weren't found in my.ausplots.obvject.")
    }
  }
  
  if(!any(c("PCoA", "diversity") %in% choices)) {
    stop("choices must include at least one of PCoA or diversity.")
  }
  ###############################################
  
  #remove sites with less than min.revisits
  my.ausplots.object$site.info <- subset(my.ausplots.object$site.info, site_location_name %in% subset(my.ausplots.object$site.info, visit_number >= min.revisits)$site_location_name)
  if(nrow(my.ausplots.object$site.info) == 0) {stop("No sites have at least ", min.revisits, " visits. Try a different selection of sites or reduce min.revisits")}
  
  #occurrence matrix
  diss <- species_table(my.ausplots.object$veg.PI, m_kind="percent_cover", cover_type="PFC", species_name="SN")
  
  #remove sites not in revisited sites table and then species with no presence
  diss <- diss[which(rownames(diss) %in% my.ausplots.object$site.info$site_unique),]
  diss <- diss[,colSums(diss) > 0]
  
  #remove sites not in veg data
  my.ausplots.object$site.info <- my.ausplots.object$site.info[which(my.ausplots.object$site.info$site_unique %in% rownames(diss)),]
  
  ###
  
  if("PCoA" %in% choices) {
    dissdiss <- vegan::vegdist(diss)
    
    if(is.null(plot_select)) {
      trajectoryPCoA_ausplots(dissdiss, sites=my.ausplots.object$site.info$site_location_name, 
  surveys=my.ausplots.object$site.info$visit_number, selection=NULL, axes = c(1, 2), 
  survey.labels=FALSE, lwd=2, lty=1, angle=12, length=0.18, 
  traj.colors=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))))
      legend("topleft", legend=unique(my.ausplots.object$site.info$site_location_name), lty=1, lwd=3, col=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))), cex=0.7, bty="n")
      } #end not select
    
    if(!is.null(plot_select)) {
      trajectoryPCoA_ausplots(dissdiss, sites=my.ausplots.object$site.info$site_location_name, 
  surveys=my.ausplots.object$site.info$visit_number, selection=my.ausplots.object$site.info$site_location_name %in% plot_select, axes = c(1, 2), survey.labels=FALSE, lwd=2, lty=1, angle=12, length=0.18, traj.colors=rainbow(length(plot_select)))
      legend("topleft", legend=plot_select, lty=1, lwd=3, col=rainbow(length(plot_select)), cex=0.7, bty="n")
      } #end plot select
    
    } #end if PCoA

  ###
  
  if("diversity" %in% choices) {
    DIV <- data.frame(shannon=vegan::diversity(diss), richness=vegan::specnumber(diss))
    
    if(length(choices) > 1) {
      dev.new()
    }
    
    if(is.null(plot_select)) {
      trajectoryPlot_ausplots(DIV, sites=my.ausplots.object$site.info$site_location_name, surveys=my.ausplots.object$site.info$visit_number, selection=NULL, axes = c(2, 1), survey.labels=FALSE, lwd=2, lty=1, angle=12, length=0.18, traj.colors=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))))
      legend("topleft", legend=unique(my.ausplots.object$site.info$site_location_name), lty=1, lwd=3, col=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))), cex=0.7, bty="n")
      } #end not select
    
    if(!is.null(plot_select)) {
      trajectoryPlot_ausplots(DIV, sites=my.ausplots.object$site.info$site_location_name, surveys=my.ausplots.object$site.info$visit_number, selection=plot_select, axes = c(2, 1), survey.labels=FALSE, lwd=2, lty=1, angle=12, length=0.18, traj.colors=rainbow(length(plot_select)))
      legend("topleft", legend=plot_select, lty=1, lwd=3, col=rainbow(length(plot_select)), cex=0.7, bty="n")
      } #end plot select
    
    } #end if diversity

} #end function