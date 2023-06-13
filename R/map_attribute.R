map_attribute <- function(my.ausplots.object) {
    
	
		if(any(c("Tree/Palm", "Tree Mallee") %in% my.ausplots.object$veg.PI$growth_form)) {
		
		  aus.sp <- ggplot2::map_data("world2Hires", "Australia")
		  
		  my.ausplots.object$site.info$bioregion.f <- factor(my.ausplots.object$site.info$bioregion_name)
	
	#get tree cover
	tree_cover <- single_cover_value(my.ausplots.object$veg.PI)
		tree_cover <- merge(my.ausplots.object$site.info, tree_cover, by="site_unique")[,c("site_unique", "longitude", "latitude", "percentCover", "bioregion.f")]
		names(tree_cover)[names(tree_cover) %in% "percentCover"] <- "Tree_cover"
	
	####

	the_map <- ggplot2::ggplot(data = tree_cover, aes(x = longitude, y = latitude, group = bioregion.f), alpha = 0.5) + 
	         geom_polygon(data=fortify(aus.sp), aes(x=long, y=lat, group=group), col="black", fill="seashell") +
      	  coord_sf(xlim=c(110, 160), ylim=c(-45,-10), expand=TRUE, crs= "+proj=longlat +ellps=clrk66 +no_defs +type=crs") +
		     #geom_point(aes(colour=bioregion.f, fill=bioregion.f, size=Tree_cover)) + 
	        geom_point(aes(size=Tree_cover), shape=1, colour = "darkgreen") + 
		     ggtitle("AusPlots locations with tree cover") + 
		     theme(plot.title = element_text(hjust = 0.5, face="bold", size=14))
		     print(the_map)
	} else {#close tree GFs
		message("The selected plots have no trees. Let's just say tree cover is zero and move on.")
			} #cls else
}