get_ausplots <- function(my.Plot_IDs="none", site_info=TRUE, structural_summaries=FALSE, veg.vouchers=FALSE,
                         veg.PI=FALSE, basal.wedge=FALSE, soil_subsites=FALSE, soil_bulk_density=FALSE,
                         soil_character=FALSE, plot_search=NULL, bounding_box="none", herbarium_determination_search=NULL, 
                         family_search=NULL, standardised_name_search=NULL, dictionary=FALSE) {

	#
  ####################
  # Fail gracefully if API or internet not available (CRAN policy = no errors)
  #
  # Based on code from (12 March 2021):
  # https://github.com/lgnbhl/wikisourcer/blob/master/R/utils.R
  # 
  #See full discussion regarding CRAN policy 
  # <https://community.rstudio.com/t/internet-resources-should-fail-gracefully/49199>
  
  ### 
  try_GET <- function(x, ...) {
    tryCatch(
      httr::GET(url = x, httr::timeout(600), ...), #timeout 10 minutes
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  ###
  
  #Check internet connection
  if(!curl::has_internet()) {
    message("No internet connection")
    return(invisible(NULL))
  }
  #Check timeout
  resp <- try_GET(getOption("ausplotsR_api_url", default= "http://swarmapi.ausplots.aekos.org.au:80"))
  if(!inherits(resp, "response")) {
    message(resp)
    return(invisible(NULL))
  }
  
  #Check http error
  if(httr::http_error(resp)) { 
    httr::message_for_status(resp)
    return(invisible(NULL))
  }
  ###############################
	#

	trim.trailing <- function (x) sub("\\s+$", "", x) #function that strips trailing white spaces from entries in d.f.
	
	ausplots.data <- list() # an empty list that will be filled with whatever elements (soil, veg...) were requested and returned from the function

	#input checks
	
	plot_Search_is_not_null_or_single_char_vector <- !is.null(plot_search) && !(is.character(plot_search) && is.vector(plot_search) && length(plot_search) == 1)
	if(plot_Search_is_not_null_or_single_char_vector) stop("plot_search must be a single element character vector") #
	
	
	herb_is_not_null_or_single_char_vector <- !is.null(herbarium_determination_search) && !(is.character(herbarium_determination_search) && is.vector(herbarium_determination_search) && length(herbarium_determination_search) == 1)
	if(herb_is_not_null_or_single_char_vector) stop("herbarium_determination_search must be a single element character vector") #
	
	family_is_not_null_or_single_char_vector <- !is.null(family_search) && !(is.character(family_search) && is.vector(family_search) && length(family_search) == 1)
	if(family_is_not_null_or_single_char_vector) stop("family_search must be a single element character vector") #
	
	standard_is_not_null_or_single_char_vector <- !is.null(standardised_name_search) && !(is.character(standardised_name_search) && is.vector(standardised_name_search) && length(standardised_name_search) == 1)
	if(standard_is_not_null_or_single_char_vector) stop("standardised_name_search must be a single element character vector") #
	
	is_family_and_herbarium_name_supplied <- !is.null(family_search) && !is.null(herbarium_determination_search)
	if(is_family_and_herbarium_name_supplied) stop("you can specify one of either family_search, herbarium_determination_search, or standardised_name_search") # 
	
	is_family_and_standardised_name_supplied <- !is.null(family_search) && !is.null(standardised_name_search)
	if(is_family_and_standardised_name_supplied) stop("you can specify one of either family_search, herbarium_determination_search, or standardised_name_search") #
	
	is_herbarium_determination_and_standardised_name_supplied <- !is.null(herbarium_determination_search) && !is.null(standardised_name_search)
	if(is_herbarium_determination_and_standardised_name_supplied) stop("you can specify one of either family_search, herbarium_determination_search, or standardised_name_search") # 
	
	if(!inherits(my.Plot_IDs, "character")) {stop("my.Plot_IDs must be provided as a character vector.")}
	if(my.Plot_IDs[1] != "none" & !is.null(plot_search)) {stop("Please provide EITHER my.Plot_IDs OR plot_search, not both.")}
	if(!(all(grepl("-", my.Plot_IDs)) | !any(grepl("-", my.Plot_IDs)))) {stop("my.Plot_IDs must list either site_location_name OR site_unique (hyphenated concatenation of site_location_name and visit_number) - NOT both.")}
	
	#####list available plots

	my.Plot_IDs <- sort(my.Plot_IDs)

		Plot_IDs <- list_available_plots(Plot_IDs=my.Plot_IDs, plot_search=plot_search, bounding_box=bounding_box, herbarium_determination_search=herbarium_determination_search, family_search=family_search, standardised_name_search=standardised_name_search)
	
	
	#check that plot IDs specified by user actually exist 
	
	if(my.Plot_IDs[1] != "none") {
		
		if(all(my.Plot_IDs %in% Plot_IDs)) {message("User-supplied Plot_IDs located.")}
		
		if(!all(my.Plot_IDs %in% Plot_IDs)) { #rules that apply if some user plot IDs aren't matched in the available plot names
			
			if(!any(my.Plot_IDs %in% Plot_IDs)) {stop("None of the user-supplied Plot_IDs match available plot names for that search.")}
			
			warning("Not all user-supplied Plot_IDs match available plot names for that search, only matching plots will be extracted.")
			
			my.Plot_IDs <- my.Plot_IDs[which(my.Plot_IDs %in% Plot_IDs)]
			
		} #end if(!all(my.Plot_IDs %in% Plot_IDs)) {
		
		Plot_IDs <- sort(my.Plot_IDs) #now the list of plots we will use is just those provided by user after checking they match these available
		
	} #end 	if(my.Plot_IDs != "none")
	
	#check other search parameters specified by the user actually exist
	#return warning if other search parameters ("extra queries") return 0 results, e.g. no species have that name
	#returns warning rather than an error in case people are running a loop with a list of species names
	
	if(length(Plot_IDs) < 1) {warning("Your search parameters returned 0 results.")}
  
	#######
	
	#Plot_IDs is now a character vector of valid site_location_names that will be used to query the database below for selected data modules
	Plot_IDs <- sort(Plot_IDs) #final sort to standardise order before getting data, regardless of whether user-input or found in list_available_plots based on any search parameters
	
	#######

	if(site_info) {
		
	  site.info <- extract_site_info(Plot_IDs)  #
	  
	  if(!length(Plot_IDs) < 1) {
	     
		#site.info$site_unique <- do.call(paste, c(site.info[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier for surveys, will make table merges easier later
		site.info <- data.frame(site_unique = do.call(paste, c(site.info[c("site_location_name", "site_location_visit_id")], sep = "-")), site.info)
		
		#Add column with visit dates reformatted as class "Date" (default date/time column is read as character string).
		site.info$visit_date <- as.Date(unlist(lapply(strsplit(site.info$visit_start_date, "T"), function(x) paste(x[1]))))
		
		#Rank visit order:	#vector of plots that have a revisit
		revisited_plots <- unique(site.info$site_location_name[which(duplicated(site.info$site_location_name))])
		
		if(length(revisited_plots) > 0) {
		  
		  n <- 0
		  for(i in revisited_plots) {
		    n <- n + 1
		    current_site <- site.info[site.info$site_location_name == i,]
		    current_site <- current_site[order(current_site$visit_date),]
		    current_site$visit_number <- seq(from = 1, to = nrow(current_site))
		    if(n == 1) {current_site_master <- current_site}
		    if(n > 1) {current_site_master <- rbind(current_site_master, current_site)}
		  }
		  
		  #merge visit rank back into main table - use left join so revisited data area added to ALL rows/plots including single visit sites. #effectively just adds a visit_number column to existing site data, with NA entered for single-visits...
		  site.info <- merge(site.info, current_site_master[,c("site_unique", "visit_number")], by="site_unique", all.x=TRUE)
		  
		  #convert NAs for no revisit to '1'
		  site.info$visit_number[is.na(site.info$visit_number)] <- 1
		  
		} #close if(length(revisited) > 0)
		
		if(length(revisited_plots) == 0) {
		  
		  site.info$visit_number <- 1
		  
		} #close if(length(revisited) == 0 )
		
		ausplots.data$site.info <- site.info

	  } # end if length plot ids 1 or more
		
	} #end if(site_info)
	
	#
	
	if(structural_summaries) {
		
	  struct.summ <- extract_struct_summ(Plot_IDs) #
		
		#struct.summ$site_unique <- do.call(paste, c(struct.summ[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier for surveys, will make table merges easier later
		struct.summ <- data.frame(site_unique = do.call(paste, c(struct.summ[c("site_location_name", "site_location_visit_id")], sep = "-")), struct.summ)
		ausplots.data$struct.summ <- struct.summ
		
	} #end if(structural_summaries)
	
	#
	
	
	if(soil_subsites) {
		
	  soil.subsites <- extract_soil_subsites(Plot_IDs) #
		
		#soil.subsites$site_unique <- do.call(paste, c(soil.subsites[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier for surveys, will make table merges easier later
		soil.subsites <- data.frame(site_unique = do.call(paste, c(soil.subsites[c("site_location_name", "site_location_visit_id")], sep = "-")), soil.subsites)
		
		ausplots.data$soil.subsites <- soil.subsites
		
	} #end if(soil_subsites)
	
	#
	
	if(soil_bulk_density) {
		
	  soil.bulk <- extract_bulk_density(Plot_IDs) #
		
		#soil.bulk$site_unique <- do.call(paste, c(soil.bulk[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier for surveys, will make table merges easier later
		soil.bulk <- data.frame(site_unique = do.call(paste, c(soil.bulk[c("site_location_name", "site_location_visit_id")], sep = "-")), soil.bulk)
		
		ausplots.data$soil.bulk <- soil.bulk
		
	} #end if(soil_bulk_density)

	#
	
	if(soil_character) {
		
	  soil.char <- extract_soil_char(Plot_IDs) #
		
		#soil.char$site_unique <- do.call(paste, c(soil.char[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier for surveys, will make table merges easier later
		soil.char <- data.frame(site_unique = do.call(paste, c(soil.char[c("site_location_name", "site_location_visit_id")], sep = "-")), soil.char)
		
		ausplots.data$soil.char <- soil.char
		
	} #end if(soil_character)

	#
	
	if(basal.wedge) {
		
	  basal <- extract_basal(Plot_IDs, herbarium_determination_search, family_search, standardised_name_search) #
		
		#basal$site_unique <- do.call(paste, c(basal[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier
		basal <- data.frame(site_unique = do.call(paste, c(basal[c("site_location_name", "site_location_visit_id")], sep = "-")), basal)
		
		ausplots.data$veg.basal <- basal
		
	} #end if(basal.wedge)

	#
	
	if(veg.vouchers) {
		
	  vouch <- extract_vouch(Plot_IDs, herbarium_determination_search, family_search, standardised_name_search)
		
		#some cleaning operations on the names:
		vouch$herbarium_determination <- trim.trailing(vouch$herbarium_determination)
		vouch$herbarium_determination <- tolower(vouch$herbarium_determination)
		vouch$herbarium_determination <- capitalize(vouch$herbarium_determination)
		
		vouch$standardised_name <- trim.trailing(vouch$standardised_name)
		vouch$standardised_name <- tolower(vouch$standardised_name)
		vouch$standardised_name <- capitalize(vouch$standardised_name)
		
		#vouch$site_unique <- do.call(paste, c(vouch[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique code for a site/visit combination
		vouch <- data.frame(site_unique = do.call(paste, c(vouch[c("site_location_name", "site_location_visit_id")], sep = "-")), vouch)
		
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
		
		#some cleaning operations...
		hits$standardised_name<- trim.trailing(hits$standardised_name)
		#clear white spaces that interfere with recognising duplicate entries as same species
		hits$standardised_name <- tolower(hits$standardised_name)
		hits$standardised_name <- capitalize(hits$standardised_name)
		
		#manually correct an odd transect identifier or two:
		delete <- grep("W5-.E", hits$transect)
		hits$transect[delete] <- "W5-E5"
		delete <- grep("1-N1", hits$transect)
		hits$transect[delete] <- "S1-N1"
		hits$transect <- as.factor(as.character(hits$transect)) 
		
		hits$hits_unique <- do.call(paste, c(hits[c("transect", "point_number")], sep = " ")) #create a new column in the hits data with transect and hit combined to make unique combination within plot, this is to identify unique samples for the calculation of cover and species accumulation curves etc.
		
		#hits$site_unique <- do.call(paste, c(hits[c("site_location_name", "site_location_visit_id")], sep = "-")) #because there are potential multiple visits to sites, here making a new single field combining site and visit IDs to get unique plot/visit identifiers that still have the plot site name.
		hits <- data.frame(site_unique = do.call(paste, c(hits[c("site_location_name", "site_location_visit_id")], sep = "-")), hits)
		
	ausplots.data$veg.PI <- hits
		
	} #end if(veg.PI)

	
	if(dictionary) {
	  
	  metadata_dictionary <- .get_metadata_dictionary()
	  
	  ausplots.data$metadata.dictionary <- metadata_dictionary
	  
	} #end if(dictionary)
	
########
#add a data citation to the object
ausplots.data$citation <- paste0("TERN (", format(Sys.Date(), format="%Y"), ") AusPlots ecosystem surveillance monitoring dataset (URL: http://aekos.org.au/). Obtained via the ausplotsR R package (URL: https://github.com/ternaustralia/ausplotsR), accessed ", format(Sys.Date(), format="%d %B %Y"), ".")	
	
	#Return the list output which was filled above as needed.
	
	return(ausplots.data)
	
} # end function

utils::globalVariables(c("bioregion.f", "height", "family", "herbarium_determination", "standardised_name", "site_location_name", "site_unique", "hits_unique", "growth_form", "longitude", "latitude", "long", "lat", "group", "Tree_cover", "dead", "genus_species", "taxa_group", "visit_number", "genus"))
