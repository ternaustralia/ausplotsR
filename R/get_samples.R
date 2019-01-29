.is_2_element_double_vector <- function(vec) {
  return(is.vector(vec) && is.double(vec) && length(vec) == 2)
}

get_samples <- function(my.Plot_IDs=c(), species_name_search=NULL, lat_boundary=c(-90, 90), lon_boundary=c(-180, 180)) {

  is_not_null_or_expected_typed_vector <- ( !is.null(my.Plot_IDs) && !is.vector(my.Plot_IDs) ) || ( length(my.Plot_IDs) > 0 && !is.character(my.Plot_IDs) )
  if(is_not_null_or_expected_typed_vector) stop("my.Plot_IDs must be a vector of strings/characters")

  is_not_null_or_single_char_vector <- !is.null(species_name_search) && !(is.character(species_name_search) && is.vector(species_name_search) && length(species_name_search) == 1)
  if(is_not_null_or_single_char_vector) stop("species_name_search must be a single element character vector")

  if(!.is_2_element_double_vector(lat_boundary)) stop("lat_boundary must be a vector with 2 double elements. First element = min, second = max")

  if(!.is_2_element_double_vector(lon_boundary)) stop("lon_boundary must be a vector with 2 double elements. First element = min, second = max")

  result <- extract_samples(Plot_IDs=my.Plot_IDs, species_name_search=species_name_search, lat_boundary, lon_boundary)

  result$site_unique <- do.call(paste, c(result[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier

  result$state = mapply(function(x) substr(x, 0, 2), result$site_location_name)
  result$state[result$state == "NS"] <- "NSW"
  result$state[result$state == "QD"] <- "QLD"
  result$state[result$state == "TC"] <- "TAS"
  result$state[result$state == "VC"] <- "VIC"
	
	return(result)
} # end function
