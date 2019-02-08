get_ausplots <- function(my.Plot_IDs="none", site_info=TRUE, structural_summaries=FALSE, veg.vouchers=TRUE, veg.PI =TRUE, basal.wedge=FALSE, soil_subsites=FALSE, soil_bulk_density=FALSE, soil_character=FALSE, bounding_box="none", species.level.taxonomy=FALSE) {

	
	#
	
	trim.trailing <- function (x) sub("\\s+$", "", x) #function that strips trailing white spaces from entries in d.f.
	
	#
	
	ausplots.data <- list() # an empty list that will be filled with whatever elements (soil, veg...) were requested and returned from the function

	#
	
	Plot_IDs <- list_available_plots()  #
		
	if(bounding_box[1] != "none") { #i.e. if user has supplied an extent vector
			
		if(class(bounding_box) != "numeric" | length(bounding_box) != 4) {stop("Bounding box must be a numeric vector of length 4.")}
				
		Plot_IDs <- subset(Plot_IDs, longitude > bounding_box[1] & longitude < bounding_box[2] & latitude > bounding_box[3] & latitude < bounding_box[4])
				
	} #end if(bounding_box)
		
	Plot_IDs <- as.character(Plot_IDs[,1]) #remove the longlats so we just have the IDs themselves as a character vector

	#
	
	if(my.Plot_IDs[1] != "none") {
		
		if(!class(Plot_IDs) == "character") {stop("Plot_IDs must be provided as a character vector.")}
		
		if(all(my.Plot_IDs %in% Plot_IDs)) {cat("User-supplied Plot_IDs located. \n")}
		
		if(!all(my.Plot_IDs %in% Plot_IDs)) { #rules that apply if some user plot IDs aren't matched in the available plot names
			
			if(!any(my.Plot_IDs %in% Plot_IDs)) {stop("None of the user-supplied Plot_IDs match available plot names.")}
			
			cat("Not all user-supplied Plot_IDs match available plot names, only matching plots will be extracted. \n")
			
			my.Plot_IDs <- my.Plot_IDs[which(my.Plot_IDs %in% Plot_IDs)]
			
		} #end if(!all(my.Plot_IDs %in% Plot_IDs)) {
		
		Plot_IDs <- my.Plot_IDs #now the list of plots we will use is just those provided by user after checking they match thpse available
		
	} #end 	if(my.Plot_IDs != "none")	

	#######
	
	#Plot_IDs is now a character vector of valid site_location_names that will be used to query the database below for selected data modules
	
	#######

	if(site_info) {
		
		site.info <- extract_site_info(Plot_IDs)  #

		site.info$site_unique <- do.call(paste, c(site.info[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier for surveys, will make table merges easier later
		
		ausplots.data$site.info <- site.info

	} #end if(site_info)
	
	#
	
	if(structural_summaries) {
		
		struct.summ <- extract_struct_summ(Plot_IDs) #
		
		struct.summ$site_unique <- do.call(paste, c(struct.summ[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier for surveys, will make table merges easier later
		
		ausplots.data$struct.summ <- struct.summ
		
	} #end if(structural_summaries)
	
	#
	
	
	if(soil_subsites) {
		
		soil.subsites <- extract_soil_subsites(Plot_IDs) #
		
		soil.subsites$site_unique <- do.call(paste, c(soil.subsites[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier for surveys, will make table merges easier later
		
		ausplots.data$soil.subsites <- soil.subsites
		
	} #end if(soil_subsites)
	
	#
	
	if(soil_bulk_density) {
		
		soil.bulk <- extract_bulk_density(Plot_IDs) #
		
		soil.bulk$site_unique <- do.call(paste, c(soil.bulk[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier for surveys, will make table merges easier later
		
		ausplots.data$soil.bulk <- soil.bulk
		
	} #end if(soil_bulk_density)

	#
	
	if(soil_character) {
		
		soil.char <- extract_soil_char(Plot_IDs) #
		
		soil.char$site_unique <- do.call(paste, c(soil.char[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier for surveys, will make table merges easier later
		
		ausplots.data$soil.char <- soil.char
		
	} #end if(soil_character)

	#
	
	if(basal.wedge) {
		
		basal <- extract_basal(Plot_IDs) #
		
		basal$site_unique <- do.call(paste, c(basal[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier
		
		ausplots.data$veg.basal <- basal
		
	} #end if(basal.wedge)

	#
	
	if(veg.vouchers) {
		
		vouch <- extract_vouch(Plot_IDs) #
		
		#some cleaning operations on the names:
		vouch$herbarium_determination <- trim.trailing(vouch$herbarium_determination)
		vouch$herbarium_determination <- tolower(vouch$herbarium_determination)
		vouch$herbarium_determination <- capitalize(vouch$herbarium_determination)
		
		vouch$site_unique <- do.call(paste, c(vouch[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique code for a site/visit combination
		
				
		ausplots.data$veg.vouch <- vouch

	} #end if(veg.vouchers)

	#
	
	if(veg.PI) {
		
		hits <- extract_hits(Plot_IDs) #
		
		#some cleaning operations...
		hits$herbarium_determination <- trim.trailing(hits$herbarium_determination)
		#clear white spaces that interfere with recognising duplicate entries as same species
		hits$herbarium_determination <- tolower(hits$herbarium_determination)
		hits$herbarium_determination <- capitalize(hits$herbarium_determination)
		
		#manually correct an odd transect identifier or two:
		delete <- grep("W5-.E", hits$transect)
		hits$transect[delete] <- "W5-E5"
		delete <- grep("1-N1", hits$transect)
		hits$transect[delete] <- "S1-N1"
		hits$transect <- as.factor(as.character(hits$transect)) 
		
		hits$hits_unique <- do.call(paste, c(hits[c("transect", "point_number")], sep = " ")) #create a new column in the hits data with transect and hit combined to make unique combination within plot, this is to identify unique samples for the calculation of cover and species accumulation curves etc.
		
		hits$site_unique <- do.call(paste, c(hits[c("site_location_name", "site_location_visit_id")], sep = "-")) #because there are potential multiple visits to sites, here making a new single field combining site and visit IDs to get unique plot/visit identifiers that still have the plot site name.
		
	ausplots.data$veg.PI <- hits
		
	} #end if(veg.PI)


	#Return the list output which was filled above as needed.
	
	return(ausplots.data)
	
	

} # end function