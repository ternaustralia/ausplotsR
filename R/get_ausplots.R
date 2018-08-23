get_ausplots <- function(my.Plot_IDs, site_info=TRUE, structural_summaries=FALSE, veg.vouchers=TRUE, veg.PI =TRUE, basal.wedge=FALSE, soil_subsites=FALSE, soil_bulk_density=FALSE, soil_character=FALSE, bounding_box="none", species.level.taxonomy=FALSE) {

########################################
#This function is the starting point for accessing data through the ausplotsR package. By default the function will extract and compile data for vegetation for all available plots. Arguments allow the user to select sites by ID numbers or geographic bounding box and select which modules to get data from (see below).

#Value: a list object with elements depending on selections made in function call arguments: $veg.PI, $veg.vouch, $veg.basal, $site.info, $struct.summ, $soil.bullk, $soil.sub, $soil.char

#site_info: whether site summary data are required (includes plot and visit details, landform data, coordinates, notes etc)

#structural_summaries: whether site vegetation structural summaries are reqiured

#veg.vouchers: whether vegetation vouchers data are requested - contains a complete set of species records for the plot determined by a herbarium

#veg.PI: whether point intercept data are requested; includes data on substrate, plant species, growth form and height etc at each of (typically) 1010 points per plot

#basal.wedge: whether basal wedge data are required to get basal area by species by plot

#soil_subsites: whether soil subsite information is required

#soil_bulk_density: whether soil bulk density data are required

#soil_character: whether soil characterisation data are required

#Plot_IDs: optional character vector of ausplots plot IDs to request data for specific set of plots

#bounding_box: an additional optional spatial filter for selecting ausplots based on a rectangular box, in the format of e.g. c(xmin, xmax, ymin, ymax)... Ausplots location data are are in longlat, therefore x is the longitude and y is the latitude of the box/extent object. Possible additions to this function are the ability to extract sites via IBRA regions, political boundaries or via a user-provided shapefile. ...e.g.: , bounding_box=c(120, 140, -30, -10), 

#species.level.taxonomy: placeholder argument in case we add the option to retrieve the species data at species level, i.e., excluding subspecies, varieties etc. This is basically just formatting the names to genus and specific epithet only.

#Authors: Greg Guerin, Andrew Tokmakoff, Tom Saleeba

###########################################

	require(R.utils)
	
	#
	
	trim.trailing <- function (x) sub("\\s+$", "", x) #function that strips trailing white spaces from entries in d.f.
	
	#
	
	ausplots.data <- list() # an empty list that will be filled with whatever elements (soil, veg...) were requested and returned from the function

	#
	
	Plot_IDs <- list_available_plots()  #where 'list_available_plots' is a function placeholder - the function will need to return a data frame of all available site_location_names from the database along with longlats (so that spatial filters can be applied if needed, such as providing a bounding box below) - three coloumns: site_location_name, longitude, latitude. There may be no arguments to the function call.
		
	if(bounding_box != "none") { #i.e. if user has supplied an extent vector
			
		if(class(bounding_box) != "numeric" | length(bounding_box) != 4) {stop("Bounding box must be a numeric vector of length 4.")}
				
		Plot_IDs <- subset(Plot_IDs, longitude > bounding_box[1] & longitude < bounding_box[2] & latitude > bounding_box[3] & latitude < bounding_box[4])
				
	} #end if(bounding_box)
		
	Plot_IDs <- as.character(Plot_IDs[,1]) #remove the longlats so we just have the IDs themselves as a character vector

	#
	
	if(exists("my.Plot_IDs")) {
		
		if(!class(Plot_IDs) == "character") {stop("Plot_IDs must be provided as a character vector.")}
		
		if(all(my.Plot_IDs %in% Plot_IDs)) {cat("User-supplied Plot_IDs located. \n")}
		
		if(!all(my.Plot_IDs %in% Plot_IDs)) { #rules that apply if some user plot ids aren't matched in the available plot names
			
			if(!any(my.Plot_IDs %in% Plot_IDs)) {stop("None of the user-supplied Plot_IDs match available plot names.")}
			
			cat("Not all user-supplied Plot_IDs match available plot names, only matching plots will be extracted. \n")
			
			my.Plot_IDs <- my.Plot_IDs[which(my.Plot_IDs %in% Plot_IDs)]
			
		} #end if(!all(my.Plot_IDs %in% Plot_IDs)) {
		
		Plot_IDs <- my.Plot_IDs #now the list of plots we will use is just those provided by user after checking they match thpse available
		
	} #end 	if(exists("my.Plot_IDs"))		

	#######
	
	#Plot_IDs is now a character vector of valid site_location_names that will be used to query the database below for selected data modules
	
	
	#######

	if(site_info) {
		
		site.info <- extract_site_info(Plot_IDs)  #placeholder function - will need to query and return a single table with rows as plots and column for the basic site information as per original extraction script

		ausplots.data$site.info <- site.info

	} #end if(site_info)
	
	#
	
	if(structural_summaries) {
		
		struct.summ <- extract_struct_summ(Plot_IDs) #placeholder function - will need to query and return a single table with rows as plots and columns for the structural summary fields
		
		ausplots.data$struct.summ <- struct.summ
		
	} #end if(structural_summaries)
	
	#
	
	
	if(soil_subsites) {
		
		soil.subsites <- extract_soil_subsites(Plot_IDs) #placeholder function for extracting subsite data for the set of Plot_IDs
		
		ausplots.data$soil.subsites <- soil.subsites
		
	} #end if(soil_subsites)
	
	#
	
	if(soil_bulk_density) {
		
		soil.bulk <- extract_bulk_density(Plot_IDs) #placeholder function - will need to query and return a single table of concatenated soil_bulk_density data
		
		ausplots.data$soil.bulk <- soil.bulk
		
	} #end if(soil_bulk_density)

	#
	
	if(soil_character) {
		
		soil.char <- extract_soil_char(Plot_IDs) #placeholder function - will need to query and return a single table of concatenated soil characterisation data
		
		ausplots.data$soil.char <- soil.char
		
	} #end if(soil_character)

	#
	
	if(basal.wedge) {
		
		basal <- extract_basal(Plot_IDs) #Where 'extract_basal' is a placeholder function that will call the basal wedge data from the database for a set of plots. Data contain the number of hits on each species in each plot per 9 reps, the basal area factor and calculated basal area.
		
		basal$site_unique <- do.call(paste, c(basal[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier
		
		ausplots.data$veg.basal <- basal
		
	} #end if(basal.wedge)

	#
	
	if(veg.vouchers) {
		
		vouch <- extract_vouch(Plot_IDs) #Where 'extract_vouch' is a placeholder function that will call the veg voucher  data from the database for a set of plots.
		
		#some cleaning operations on the names:
		vouch$herbarium_determination <- trim.trailing(vouch$herbarium_determination)
		vouch$herbarium_determination <- tolower(vouch$herbarium_determination)
		vouch$herbarium_determination <- capitalize(vouch$herbarium_determination)
		
		vouch$site_unique <- do.call(paste, c(vouch[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique code for a site/visit combination
		
		#species richness can be calculated easily with:
		#plyr::count(vouch, vars="site_unique")
		
		ausplots.data$veg.vouch <- vouch

	} #end if(veg.vouchers)

	#
	
	if(veg.PI) {
		
		hits <- extract_hits(Plot_IDs) #Where 'extract_hits' is a placeholder function that will call the veg point intercept data from the database for a set of plots in Plot_IDs.
		
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
		
		hits$site_unique <- do.call(paste, c(hits[c("site_location_name", "site_location_visit_id")], sep = "-")) #because there are multiple visits for some - and potentially all - sites, we have queried unique visit_id numbers from the database in the raw PI data. Here we are making a new single field combining site and visit ids to get unique plot/visit identifiers that still have the plot site name in it because a straight visit number would be harder to interpret and use for labels etc
		
	ausplots.data$veg.PI <- hits
		
	} #end if(veg.PI)


	#Return the list output which was filled above as needed.
	
	return(ausplots.data)
	
	

} # end function