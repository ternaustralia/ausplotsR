map_attribute <- function(my.ausplots.object) {
		
		
		aus = map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),ylim=c(-45,-5), mar=c(0,0,0,0), plot=FALSE)
	
	#CRS("+init=epsg:4326") # More info (i.e. provides a datum)
	aus.sp = map2SpatialPolygons(aus, IDs=aus$names, proj4string=CRS("+init=epsg:4326"))
	
	my.ausplots.object$site.info$bioregion.f <- factor(my.ausplots.object$site.info$bioregion_name)
	
	#get tree cover
	tree_cover <- single_cover_value(my.ausplots.object$veg.PI)
		tree_cover <- merge(my.ausplots.object$site.info, tree_cover, by="site_unique")[,c("site_unique", "longitude", "latitude", "percentCover", "bioregion.f")]
		names(tree_cover)[names(tree_cover) %in% "percentCover"] <- "Tree_cover"
	
	####
	the_map <- ggplot(data = tree_cover, 
		     aes(x = longitude, y = latitude, group=bioregion.f), alpha=0.5) + 
		     geom_point(aes(colour=bioregion.f, fill=bioregion.f, size=Tree_cover)) + 
		    
		     ggtitle("AusPlots locations") + 
		     theme(plot.title = element_text(hjust = 0.5, face="bold", size=14)) +
		     geom_polygon(data=fortify(aus.sp), aes(x=long, y=lat, group=group), col="black", fill=NA)
		     
		     print(the_map)
	
}