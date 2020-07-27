map_ausplots <- function(my.ausplots.object) {
	
  data(worldHiresMapEnv)
  
  aus <- maps::map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),ylim=c(-45,-5), mar=c(0,0,0,0), plot=FALSE)
	
	CRS("+init=epsg:4326") # More info (i.e. provides a datum)
	aus.sp <-  map2SpatialPolygons(aus, IDs=aus$names, proj4string=CRS("+init=epsg:4326"))
	
	my.ausplots.object$site.info$bioregion.f <- factor(my.ausplots.object$site.info$bioregion_name)
	
	bioregions.cnt <- length(levels(my.ausplots.object$site.info$bioregion.f))
	shape.cycles.num <- ceiling(bioregions.cnt / 7) #  Using 7 distinct Symbol Shapes
	sites.shape.values <- rep(c(15:18,3:4,8),shape.cycles.num)[1:bioregions.cnt]
	
	
	####
	the_map <- ggplot(data= my.ausplots.object$site.info, 
		     aes(x = longitude, y = latitude, group=bioregion.f), alpha=0.5) + 
		     geom_polygon(data=fortify(aus.sp), aes(x=long, y=lat, group=group), col="black", fill="seashell") +   
		     geom_point(aes(colour=bioregion.f, fill=bioregion.f, shape=bioregion.f), size=1.5) + 
		     
		     scale_shape_manual(values=sites.shape.values) +  # Cycle through Symbol Types 
		     ggtitle("AusPlots locations") + 
		     theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))
		     
		     print(the_map)
	
}
