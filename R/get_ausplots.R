get_ausplots <- function(plot_IDs, bounding_box, site_info=TRUE, veg.vouchers=TRUE, veg.PI =TRUE, basal.wedge=FALSE, soil_subsites=FALSE, soil_bulk_density=FALSE, soil_character=FALSE, species.level.taxonomy=FALSE) {

#This function is the starting point for accessing data through the ausplotsR package. By default the function will extract and compile data for vegetation and soils for all available plots. Arguments allow the user to select site by IDs or bounding box and select which modules to get data from (see below).

#Value: a list object with elements depending on selections made in function call arguments: $veg.PI, $veg.vouch, $veg.basal ('basal'), $site.info, $soil.bullk, $soil.sub, $soil.char

#site_info: whether site summary data is required (includes plot and visit details, landform data, coordinates etc)

#veg.vouchers: whether vegetation vouchers data is requested - contains a complete set of species records for the plot determined by a herbarium

#veg.PI: whether poiont intercept data is requested; includes data on substrate, plant species growth form and height etc at each of 1010 points per plot

#basal.wedge: whether basal wedge data are required to get basal area by species by plot

#soil_subsites: whether soil subsite information is required

#soil_bulk_density: whether soil bulk density data are required

#soil_character: whether soil characterisation data are required

#plot_IDs: optional vector of ausplots plot IDs to request data for specific set of plots

#bounding_box: optional spatial filter for selecting ausplots based on a rectangular box, in the format of e.g. c(xmin, xmax, ymin, ymax)... Ausplots location data are are in longlat, therefore x is the longitude and y is the latitude 

#species.level.taaxonomy: placeholder argument in case we add the option to retrieve the species data at species level, i.e., excluding subspecies, varieties etc.

#Authors: Greg Guerin, Andrew Tokmakoff, Tom Saleeba

	require(R.utils)

	trim.trailing <- function (x) sub("\\s+$", "", x) #function that strips trailing white spaces from entries in d.f.


	if(!exists("Plots_IDs")) {
		#placeholder, if no plot_IDs are entered, it will need to find a list of them from the database and get longlats if any spatial filter is appled such as providing a bounding box
	} #end if(!exists("Plot_IDs))

	#
	
	if(bounding_box) {
		#placeholder for subsetting the Plots_IDs spatially - will dig up code for this from previous analysis (e.g. endemism functions)
	} #end if(bounding_box)
	

	#

	if(site_info) {
		
		#placeholder - will need to query and return a single table with rows as plots and column for the basic site information as per original extraction script

	} #end if(site_info)
	
	
	#
	
	
	if(soil_subsites) {
		
		#placeholder - will need to query and return a single table of concatenated subsite data
		
	} #end if(soil_subsites)
	
	#
	
	if(soil_bulk_density) {
		
		#placeholder - will need to query and return a single table of concatenated soil_bulk_density data
		
	} #end if(soil_bulk_density)

	#
	
	if(soil_character) {
		
		#placeholder - will need to query and return a single table of concatenated soil characterisation data
		
	} #end if(soil_character)

	#
	
	if(basal.wedge) {
		
		basal <- extract_basal(plot_IDs) #Where 'extract_basal' is a placeholder function that will call the basal wedge data from the database for a set of plots. Data contain the number of hits on each species in each plot per 9 reps, the basal area factor and calculated basal area.
		
		basal$site_unique <- do.call(paste, c(basal[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier
		
	} #end if(basal.wedge)

	#
	
	if(veg.vouchers) {
		
		vouch <- extract_vouch(plot_IDs) #Where 'extract_vouch' is a placeholder function that will call the veg voucher  data from the database for a set of plots.
		
		#some cleaning operations on the names:
		vouch$herbarium_determination <- trim.trailing(vouch$herbarium_determination)
		vouch$herbarium_determination <- tolower(vouch$herbarium_determination)
		vouch$herbarium_determination <- capitalize(vouch$herbarium_determination)
		
		vouch$site_unique <- do.call(paste, c(vouch[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique code for a site/visit combination
		
		#species richness can be calculated easily with:
		#plyr::count(vouch, vars="site_unique")

	} #end if(veg.vouchers)

	#
	
	if(veg.PI) {
		
		hits <- extract_hits(PLot_IDs) #Where 'extract_hits' is a placeholder function that will call the veg point intercept data from the database for a set of plots.
		
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
		
	} #end if(veg.PI)

} # end function