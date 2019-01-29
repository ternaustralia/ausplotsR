require(httr)
require(jsonlite)

.ausplots_api <- function(path, query) {
  # override this with:
  #   library("ausplotsR")
  #   options("ausplotsR_api_url" = "http://localhost:30000")
  #   ...continue to call functions
  if (is.null(getOption("ausplotsR_api_url"))) {
    options("ausplotsR_api_url" = "http://swarmapi.ausplots.aekos.org.au:80")
  }

  resp <- httr::GET(getOption("ausplotsR_api_url"), path=path, query=query)
  stop_for_status(resp)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  return(jsonlite::fromJSON(content(resp, "text"), simplifyDataFrame = TRUE))
}

.ausplots_api_with_plot_filter <- function(path, Plot_IDs, extra_query=list()) {
  query <- extra_query
  if (length(Plot_IDs) > 0) {
    plotFilter <- paste("in.(", paste(Plot_IDs, collapse=","), ")", sep="")
    query <- c(query, list(site_location_name = plotFilter))
  }
	return(.ausplots_api(path, query)) 
} 
################

list_available_plots <- function() {
  path <- "site"
  query <- list(select = "site_location_name,latitude,longitude")
	return(.ausplots_api(path, query)) 
}

################

extract_site_info <- function(Plot_IDs) {
  path <- "site"
  return(.ausplots_api_with_plot_filter(path, Plot_IDs))
} 

###############

extract_struct_summ <- function(Plot_IDs) {
  path <- "structural_summary"
  return(.ausplots_api_with_plot_filter(path, Plot_IDs))
} 

####################

extract_soil_subsites <- function(Plot_IDs) {
  path <- "soil_subsite"
  return(.ausplots_api_with_plot_filter(path, Plot_IDs))
}

##################

extract_bulk_density <- function(Plot_IDs) {
  path <- "soil_bulk_density"
  return(.ausplots_api_with_plot_filter(path, Plot_IDs))
}

##################

extract_soil_char <- function(Plot_IDs) {
  path <- "soil_characterisation"
  return(.ausplots_api_with_plot_filter(path, Plot_IDs))
}

##################

extract_basal <- function(Plot_IDs) {
  path <- "veg_basal"
  return(.ausplots_api_with_plot_filter(path, Plot_IDs))
}

############################

extract_vouch <- function(Plot_IDs) {
  path <- "veg_voucher"
  return(.ausplots_api_with_plot_filter(path, Plot_IDs))
}

############################

extract_hits <- function(Plot_IDs) {
  path <- "veg_pi"
  return(.ausplots_api_with_plot_filter(path, Plot_IDs))
}

############################

extract_samples <- function(Plot_IDs=c(), species_name_search=NULL) {
  path <- "samples"
  extra_query <- list()
  if (!is.null(species_name_search)) {
    extra_query <- list(herbarium_determination = paste("ilike.*", species_name_search, "*", sep=""))
  }
  return(.ausplots_api_with_plot_filter(path, Plot_IDs, extra_query))
}
