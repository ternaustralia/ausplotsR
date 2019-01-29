get_samples <- function(my.Plot_IDs=c(), species_name_search=NULL) {

  result <- extract_samples(Plot_IDs=my.Plot_IDs, species_name_search=species_name_search)

  result$site_unique <- do.call(paste, c(result[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier
	
	return(result)
} # end function
