map_ausplots <- function(my.ausplots.object) {
  tryCatch({
  .sentry_capture_message(paste0("ausplotsR: map_ausplots called with my.ausplots.object=", ifelse(is.list(my.ausplots.object), paste0("list, site.info=", ifelse(is.data.frame(my.ausplots.object$site.info), paste0(nrow(my.ausplots.object$site.info), " rows"), "missing")), class(my.ausplots.object)[1])), level = "info")
  #input checks
  
  if(!("site.info" %in% names(my.ausplots.object))) {
    stop("The input 'my.ausplots.object' must contain $site.info data to generate a map!")
  } #close if no site info
  
  ###
  
  aus.sp <- ggplot2::map_data("world2Hires", "Australia")
	
	my.ausplots.object$site.info$bioregion.f <- factor(my.ausplots.object$site.info$bioregion_name)
	
	bioregions.cnt <- length(levels(my.ausplots.object$site.info$bioregion.f))
	shape.cycles.num <- ceiling(bioregions.cnt / 7) #  Using 7 distinct Symbol Shapes
	sites.shape.values <- rep(c(15:18,3:4,8),shape.cycles.num)[1:bioregions.cnt]
	
	###
	
	the_map <- ggplot2::ggplot(data = my.ausplots.object$site.info, 
		     aes(x = longitude, y = latitude, group=bioregion.f), alpha=0.5) + 
		     geom_polygon(data=fortify(aus.sp), aes(x=long, y=lat, group=group), col="black", fill="seashell") +  
	       coord_sf(xlim=c(110, 160), ylim=c(-45,-10), expand=TRUE, crs= "+proj=longlat +ellps=clrk66 +no_defs +type=crs") +
		     geom_point(aes(colour=bioregion.f, fill=bioregion.f, shape=bioregion.f), size=1.5) + 
		     scale_shape_manual(values=sites.shape.values) +  # Cycle through Symbol Types 
		     ggtitle("AusPlots locations") + 
		     theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))
		     print(the_map)
  .sentry_capture_message("ausplotsR: map_ausplots rendered successfully", level = "info")
	
  }, error = function(e) {
    .sentry_capture_exception(e, "An exception occurred in map_ausplots")
    stop(paste0("Failed to generate ausplots map: ", conditionMessage(e)), call. = FALSE)
  })
}
