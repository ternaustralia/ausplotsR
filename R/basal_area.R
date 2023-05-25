basal_area <- function(veg.basal, by.spp=FALSE, by.hits=FALSE, species_name=c("SN","HD","GS")) {
	
  
  basal <- veg.basal #for historical reasons, the compiled but raw data (referred to as simply 'basal' in the code)
	
	if(!by.hits) {
		
		if(!by.spp) {
			
			bas_areas <- plyr::count(basal, c("site_unique", "point_id"), wt_var="basal_area") #basal area scores for unique combos of site and point ID - i.e. for each of the 9 sample points in each plot, what is the total BA?
			
			bas_areas_mean <- stats::aggregate(bas_areas$freq, by=list(bas_areas$site_unique), FUN=mean) #mean of the basal areas for each of 9 sampling points in the plot. This is to give one value for each sampled plot, averaged across the 9 different sampling points and all species considered together.
			
			names(bas_areas_mean) <- c("site_unique", "basal_area_m2_ha")
			
			return(bas_areas_mean)

		} #end if(!by.spp)
		
		if(by.spp) { #Basal Area by species per plot, default is standardised_name
		
		  if(missing(species_name)) { #if no species_name supplied, default to SN
	       species_name <- "SN"
	       warning("No species_name supplied, defaulting to species_name='SN'")
		  } #end missing
		  
		  
		  if(species_name=="SN") { #uses standardised name
		    
		    bas_areas_spp_mean <- stats::aggregate(basal$basal_area, by=list(basal$site_unique, basal$standardised_name), FUN=sum)
		    
		    names(bas_areas_spp_mean) <- c("site_unique", "standardised_name", "basal_area_m2_ha")
		    
		    n.points <- stats::aggregate(basal$point_id, by=list(basal$site_unique), FUN=function(x) {return(length(unique(x)))})
		    
		    names(n.points) <- c("site_unique", "n.points")
		    
		    bas_areas_spp_mean$n.points <- n.points$n.points[match(bas_areas_spp_mean$site_unique, n.points$site_unique)]
		    
		    bas_areas_spp_mean$basal_area_m2_ha <- bas_areas_spp_mean$basal_area_m2_ha / bas_areas_spp_mean$n.points
		    
		  } #end SN
		  
		  if(species_name=="HD") { #uses herbarium determination
		    
		    bas_areas_spp_mean <- stats::aggregate(basal$basal_area, by=list(basal$site_unique, basal$herbarium_determination), FUN=mean)
		    
		    names(bas_areas_spp_mean) <- c("site_unique", "herbarium_determination", "basal_area_m2_ha") 
		  } #end HD
		    
		    if(species_name=="GS") { #uses genus_species
		      
		      bas_areas_spp_mean <- stats::aggregate(basal$basal_area, by=list(basal$site_unique, basal$genus_species), FUN=mean)
		      
		      names(bas_areas_spp_mean) <- c("site_unique", "genus_species", "basal_area_m2_ha") 
		    } #end GS
		    
		    return(bas_areas_spp_mean)
		    
		  }#end if(by.spp)
	  
	} #end if(!by.hits)
	
	
	if(by.hits) { #Number of (tree) hits in the basal wedge sweep reps, giving a score related to tree density (there is no specific area as hits not confined to trees within the plot)
		
		if(!by.spp) {
			
			dens <- plyr::count(basal, c("site_unique", "point_id"), wt_var="hits") #number of hits for unique combos of site and point ID - i.e. for each of the 9 basal wedge sample points in each plot, what is the total number of scored trees for any spp recorded?
			
			dens_mean <- stats::aggregate(dens$freq, by=list(dens$site_unique), FUN=mean) #mean of the scores for each of the 9 reps - this is the average number of total tree hits for a rep at a given plot.
			
			names(dens_mean) <- c("site_unique", "mean_hits")
			
			return(dens_mean)
			
		} #end if(!by.spp)
		
	  if(by.spp) { #Hits by species:
	    
	    if(missing(species_name)) {
	      species_name <- "SN"
	      warning("No species_name supplied, defaulting to species_name='SN'")
	    }
	    
	    if(species_name=="SN") {
	      
	      dens_spp_mean <- stats::aggregate(basal$hits, by=list(basal$site_unique, basal$standardised_name), FUN=mean)
	      
	      names(dens_spp_mean) <- c("site_unique", "standardised_name", "mean_hits")
	    } #end SN
	    
	      if(species_name=="HD") {
	        
	      ens_spp_mean <- stats::aggregate(basal$hits, by=list(basal$site_unique, basal$herbarium_determination), FUN=mean)
	      
	      names(dens_spp_mean) <- c("site_unique", "herbarium_determination", "mean_hits")
	      
	      }
	      
	      
	      if(species_name=="GS") {
	        
	        basal<-basal[!is.na(basal$genus_species), ] #assumes that if a genus_species identification is not possible, the cell value for the row will be NA
	        
	        #alternatively, we might assign it a no data term, in which case
	        #basal<-basal[!(basal$genus_species=="No ID",]
	        
	        ens_spp_mean <- stats::aggregate(basal$hits, by=list(basal$site_unique, basal$genus_species), FUN=mean)
	        
	        names(dens_spp_mean) <- c("site_unique", "genus_species", "mean_hits")
	        
	      }
	      
	      return(dens_spp_mean)
	      
	    
	  } #end if(by.spp)
	  
	  
	} #end if(by.hits)

	
} #end function