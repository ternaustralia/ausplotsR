map_ausplots <- function(my.ausplots.object) {
	
}



#----------------------------------------------------------------------------------------
# Get and Prepare a Map of Australia
#----------------------------------------------------------------------------------------

# Maps in the package 'maps' are projected in longlat by default
aus = map("worldHires", "Australia", fill=TRUE, xlim=c(110,160),ylim=c(-45,-5), mar=c(0,0,0,0), plot=FALSE)

# Convert map data to SpatialPolygons
#aus.sp = map2SpatialPolygons(aus, IDs=aus$names, proj4string=CRS("+proj=longlat"))
CRS("+init=epsg:4326") # More info (i.e. provides a datum)
aus.sp = map2SpatialPolygons(aus, IDs=aus$names, proj4string=CRS("+init=epsg:4326"))


#----------------------------------------------------------------------------------------
# Plot All AusPlots Sites on a Map of Australia
#----------------------------------------------------------------------------------------

# We well use 7 distinct symbol shapes to represent the AusPlots Sites (combined with 
# different colors). We will cycle through the 7 symbol shapes. We start by creating a
# vector of with symbol shapes codes as long as the number of bioregions in the current 
# version of the AusPlots dataset, cycling among the 7 shapes. What complicates this 
# process a bit is that the number of bioregions sampled changes with time, as additional 
# sites in different bioregions are sampled. Thus, we need to estimate the required number 
# of cycles of symbol shapes from the data. To do this we use the function ceiling, and 
# the cut back to the required number of symbol shapes as we might not need full cycles 
# (i.e. the number of sites might not be a multiple of 7; e.g. 50 sites require more than
# 7 cycles, so we use 8 and then trimm the vector from 56 (7 shapes * 8 cycles) to 50.

# Preparation: Create a vector with the symbol shapes values
bioregions.cnt = length(levels(AP.data$site.info$bioregion.f))
shape.cycles.num = ceiling(bioregions.cnt / 7) #  Using 7 distinct Symbol Shapes
sites.shape.values = rep(c(15:18,3:4,8),shape.cycles.num)[1:bioregions.cnt]

# Create Plot
ggplot( data=AP.data$site.info, 
		     aes(x = longitude, y = latitude, group=bioregion.f), alpha =0.5) + 
geom_point(aes(colour=bioregion.f, fill=bioregion.f, shape=bioregion.f), size=1.5) + 
scale_shape_manual(values=sites.shape.values) +  # Cycle through Symbol Types 
ggtitle("Locations of all AusPlots sites") + 
theme(plot.title = element_text(hjust = 0.5, face="bold", size=14)) +
geom_polygon(data=fortify(aus.sp), aes(x=long, y=lat, group=group), col="black", fill=NA)