basal_area <- function(veg.basal, by.spp=FALSE, by.hits=FALSE) {
	
	require(plyr)
	
	#The function takes the compiled raw basal wedge data from 'get_ausplots' and calculates basal area (or number of basalk wedge hits) by plot. This is the function version of the 'compile_ausplots_basal_wedge.R' script.
	
	#Nine measurements are taken at different points across the plot and these are averaged to give values per plot for basal area (+/- by species) and number of individual hits (+/- by species)
	
	#veg.basal is the raw basal wedge data with individual hits in rows
	
	#by.spp: set to TRUE to get values per individual species rather than combined
	
	#by.hits: set to TRUE to return the number of individual basal wedge hits rather than basal area
	
	#VALUE: returns a data frame with sites as rows
	
	basal <- veg.basal #for historical reasons, the compiled but raw data (referred to as simply 'basal' in the code)
	
	if(!by.hits) {
		
		if(!by.spp) {
			
			bas_areas <- count(basal, c("site_unique", "point_id"), wt_var="basal_area") #basal area scores for unique combos of site and point ID - i.e. for each of the 9 sample points in each plot, what is the total BA?
			
			bas_areas_mean <- aggregate(bas_areas$freq, by=list(bas_areas$site_unique), FUN=mean) #mean of the basal areas for each of 9 sampling points in the plot. This is to give one value for each sampled plot, averaged across the 9 different sampling points and all species considered together.
			
			names(bas_areas_mean) <- c("site_unique", "basal_area_m2_ha")
			
			return(bas_areas_mean)

		} #end if(!by.spp)
		
		if(by.spp) { #Basal Area by species per plot
			
			bas_areas_spp_mean <- aggregate(basal$basal_area, by=list(basal$site_unique, basal$herbarium_determination), FUN=mean)
			
			names(bas_areas_spp_mean) <- c("site_unique", "herbarium_determination", "basal_area_m2_ha")
			
			return(bas_areas_spp_mean)
			
		} #end if(by.spp)
	
	} #end if(!by.hits)
	
	
	if(by.hits) { #Number of (tree) hits in the basal wedge sweep reps, giving a score related to tree density (there is no specific area as hits not confined to trees within the plot)
		
		if(!by.spp) {
			
			dens <- count(basal, c("site_unique", "point_id"), wt_var="hits") #number of hits for unique combos of site and point ID - i.e. for each of the 9 basal wedge sample points in each plot, what is the total number of scored trees for any spp recorded?
			
			dens_mean <- aggregate(dens$freq, by=list(dens$site_unique), FUN=mean) #mean of the scores for each of the 9 reps - this is the average number of total tree hits for a rep at a given plot.
			
			names(dens_mean) <- c("site_unique", "mean_hits")
			
			return(dens_mean)
			
		} #end if(!by.spp)
		
		if(by.spp) { #Hits by species:
			
			dens_spp_mean <- aggregate(basal$hits, by=list(basal$site_unique, basal$herbarium_determination), FUN=mean)
			
			names(dens_spp_mean) <- c("site_unique", "herbarium_determination", "mean_hits")
			
			return(dens_spp_mean)

		} #end if(by.spp)
		
		
	} #end if(by.hits)

} #end function