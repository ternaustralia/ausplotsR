cache <- new.env(parent = emptyenv())

LIMIT <- as.integer(20000)
END_POINT <- getOption("ausplotsR_api_url", default= "http://swarmapi.ausplots.aekos.org.au:80")

try_GET <- function(path, query, auth_header,...) {
  return(httr::GET(
    END_POINT,
    httr::user_agent(.make_user_agent()),
    httr::add_headers(
      Authorization = auth_header),
    path=path,
    query=query)
  )
}
# sends a request with header `Prefer` = "count=estimated" 
#   to extract table info(e.g. names of the columns or total size of the table).
# we add that header `Prefer` = "count=estimated"  to ignore nginx cache
api_info <- function(path, auth_header,...) {
  query <- list('limit'=1)
  resp <- httr::GET(
    END_POINT,
    httr::user_agent(.make_user_agent()),
    httr::add_headers(
      `Prefer` = "count=estimated",
      Authorization = auth_header),
    path=path,
    query=query)
  
  apiData <- r2r::hashmap()
  if (is.null(resp)) {
    apiData[["max_size"]] <- 0
    apiData[["columns"]] <- NULL
    return(apiData)
  }
  
  # we need total number of rows to initialize progressBar
  content_length <- try(strsplit(head(resp)$all_headers[[1]]$headers$`content-range`, split = '/')[[1]][2])
  apiData[["max_size"]] <- if (content_length > 0) content_length else 0
  # we use these to add 'order' parameter.
  # without sorting pagination won't work.
  body <- httr::content(resp, "parsed")
  jsonB <- jsonlite::toJSON(body)
  rObj <- names(jsonlite::fromJSON(jsonB))
  columns <- ''
  for (x in 1:length(rObj[])) {
    if (x==1) {
      columns <- rObj[[x]]
      next
    }
    columns <- paste(columns, rObj[[x]], sep = ",")
  }
  apiData[["columns"]] <- columns
  return(apiData)
}

.ausplots_api <- function(path, query) {
  # override this API URL with:
  #   library("ausplotsR")
  #   options("ausplotsR_api_url" = "http://localhost:30000")
  #   ...continue to call functions
  auth_header <- ""
  the_role <- getOption("ausplotsR_role")
  the_secret <- getOption("ausplotsR_secret")
  if (!is.null(the_role) && !is.null(the_secret)) {
    path <- paste(path, "_inc_unpub", sep="")
    five_days <- 60 * 60 * 24 * 5
    exp_time <- as.integer(Sys.time() + five_days)
    claim <- jose::jwt_claim(role = the_role, exp = exp_time)
    jwt_val <- jose::jwt_encode_hmac(claim, secret = charToRaw(the_secret))
    auth_header <- paste('Bearer', jwt_val)
  }
  if (getOption("ausplotsR_api_debug", default = FALSE)) {
    message('query string value = ', query)
  }
  message('Calling the database. Please wait...')
  progressBar <- NULL
  progressBar <- progress::progress_bar$new(format = "downloading (:what) [:bar] (:percent) time: :elapsedfull", total = 100, clear = FALSE, width= 60, show_after = 0)
  progressBar$tick(1, tokens = list(what = path))
  resp <- try_GET(path=path, query=query, auth_header=auth_header)
  if (httr::status_code(resp) < 500) {
    progressBar$tick(100, tokens = list(what = path))
  }
  message(httr::status_code(resp))
  
  #######################################################################################
  # if status code is 500 or 503
  # most of the cases, 503 (database connection issue) happens because of the size of data
  #   e.g. point_intercept table has more then 100827558 rows 
  #   in that case, we can use limit and offset as api parameters to get a chunk of data
  #   and then we can combine all data into one 
  #########################################################################################
  if (httr::status_code(resp) > 500) {
    message('Preparing to paginate the data, please wait... ')
    Sys.sleep(5)
    message(' Calling the database... ')
    message(' again. Please wait...')
    
    offset <- 0
    progressBar <- NULL
    isBody <- TRUE
    finalResp <- NULL
    
    query <- append(query, list("limit" = LIMIT))
    query <- append(query, list("offset" = offset))
    
    tableInfo <- api_info(path=path, auth_header=auth_header)
    if (tableInfo[['max_size']] > 0) {
      # initialize progressBar
      progressBar <- progress::progress_bar$new(format = " downloading (:what) [:bar] :current/:total (:percent) time: :elapsedfull", total = tableInfo[['max_size']], show_after = 0)
      progressBar$tick(1, tokens = list(what = path))
    }
    # data needs to be sorted by all the fields("order"="all columns of the table")
    if (!is.null(tableInfo[['columns']])) {
      query <- append(query, list("order" = tableInfo[['columns']]))
    }
    
    # It seems like R can't handle recursive, most of the time it generates memory limit exception
    # thats why we are using while loop.
    while (isBody) {
      query$offset <- as.integer(offset)
      respPagination <- try_GET(path=path, query=query, auth_header=auth_header)
      respStatus <- httr::status_code(respPagination)
      if (respStatus>=500) {
        # if one of them fails, we will retry.
        Sys.sleep(5)
        next
      }
      # parse and check the body
      body <- httr::content(respPagination, "parsed")
      jsonBody <- jsonlite::toJSON(body)
      bodySize <- nrow(jsonlite::fromJSON(jsonBody[[1]]))
      
      if (respStatus<500 && is.null(bodySize) && !is.null(finalResp)) {
        message(' finalising...')
        isBody <- FALSE
        finalResp <- gsub("\\]\\[", ",", finalResp)
        finalResp <- gsub("\\,\\]", "]", finalResp)
        result <- try(jsonlite::fromJSON(finalResp, simplifyDataFrame = TRUE))
        return(result)
      }
      if (!is.null(progressBar)) {
        progressBar$tick(as.integer(bodySize), tokens = list(what = path))
      }
      
      # combine multiple response
      if (offset == 0) {
        finalResp <- httr::content(respPagination, "text", encoding = "UTF-8")
      } else {
        finalResp <- paste(finalResp, httr::content(respPagination, "text", encoding = "UTF-8"), sep = "")
      }
      offset <- as.integer(offset) + as.integer(LIMIT)
    }
    
    stop("Server is busy at the moment. Please try again in a few minutes")
  }
  
  httr::stop_for_status(resp, task = httr::content(resp, "text"))
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  result <- try(jsonlite::fromJSON(httr::content(resp, "text"), simplifyDataFrame = TRUE))
  if(inherits(result, "try-error")) {
    stop("Data extraction aborted due to database connection issue.")
  }
  return(result)
}
################
#initial filter function
.ausplots_api_with_plot_filter <- function(path, Plot_IDs_to_filter_for, extra_query=list()) {
  query <- extra_query
  if (length(Plot_IDs_to_filter_for) > 0) {
    
    #sub-routine for visit searches via site_unique versus plot searches
    
    site_unique_search <- any(grepl("-", Plot_IDs_to_filter_for, fixed=TRUE))
    
    if(site_unique_search) {
      Plot_visits_to_filter_for <- unlist(lapply(strsplit(Plot_IDs_to_filter_for, "-", fixed=TRUE), function(x) {paste(x[2])}))
      visitFilter <- paste("in.(", paste(Plot_visits_to_filter_for, collapse=","), ")", sep="")
      if(!("site_location_name" %in% names(extra_query))) {
        query <- c(query, list(site_location_visit_id = visitFilter))
      }
    }
    
    if(!site_unique_search) {
      plotFilter <- paste("in.(", paste(Plot_IDs_to_filter_for, collapse=","), ")", sep="")
      #check site location isn't already searched via plot_search argument - may be redundant as get_ausplots stops if that's the case
      if(!("site_location_name" %in% names(extra_query))) {
        query <- c(query, list(site_location_name = plotFilter))
      }
    }
    
  }
  return(.ausplots_api(path, query)) 
} 
################
#subsequent filter function

.ausplots_api_with_specified_plot_ids <- function(path, Plot_IDs_to_retrieve_data_for, extra_query=list()) {
  
  
  site_unique_search <- any(grepl("-", Plot_IDs_to_retrieve_data_for, fixed=TRUE))
  
  if(!site_unique_search) {
    plotFilter <- paste("in.(", paste(Plot_IDs_to_retrieve_data_for, collapse=","), ")", sep="")
    query <- c(extra_query, list(site_location_name = plotFilter))
  }
  
  if(site_unique_search) {
    Plot_visits_to_filter_for <- unlist(lapply(strsplit(Plot_IDs_to_retrieve_data_for, "-", fixed=TRUE), function(x) {paste(x[2])}))
    visitFilter <- paste("in.(", paste(Plot_visits_to_filter_for, collapse=","), ")", sep="")
    query <- c(extra_query, list(site_location_visit_id = visitFilter))
  }
  
  return(.ausplots_api(path, query))
} 
################

list_available_plots <- function(Plot_IDs=c(), plot_search=NULL, bounding_box="none", herbarium_determination_search=NULL, family_search=NULL, standardised_name_search=NULL) {
  extra_query <- list()
  
  if(!is.null(family_search)) {
    extra_query = append(extra_query, list("family" = paste("ilike.*", family_search, "*", sep=""))) #search by family
  }
  if(!is.null(herbarium_determination_search)) {
    extra_query = append(extra_query, list("herbarium_determination" = paste("ilike.*", herbarium_determination_search, "*", sep=""))) #search by herbarium_determination
  } 
  if(!is.null(standardised_name_search)) {
    extra_query = append(extra_query, list("standardised_name" = paste("ilike.*", standardised_name_search, "*", sep=""))) #search by standardised_name
  }
  if(Plot_IDs[1] == "none") {
    Plot_IDs <- c()
  }
  #####
  # #insert wildcard match code for plot names 2023 or in get_a function
  if(!is.null(plot_search)) {
    extra_query = append(extra_query, list("site_location_name" = paste0("ilike.*", plot_search, "*")))
  }
  #####
  if(bounding_box[1] != "none") { #i.e. if user has supplied an extent vector
    if(!inherits(bounding_box, "numeric") | length(bounding_box) != 4) {stop("Bounding box must be a numeric vector of length 4.")}
    
    min_lon <- bounding_box[1]
    max_lon <- bounding_box[2]
    min_lat <- bounding_box[3]
    max_lat <- bounding_box[4]
    
    extra_query = append(extra_query, list("latitude" = paste("gte.", min_lat, sep="")))
    extra_query = append(extra_query, list("latitude" = paste("lte.", max_lat, sep="")))
    extra_query = append(extra_query, list("longitude" = paste("gte.", min_lon, sep="")))
    extra_query = append(extra_query, list("longitude" = paste("lte.", max_lon, sep="")))
  }
  path <- "search"
  response <- .ausplots_api_with_plot_filter(path, Plot_IDs, extra_query)
  
  site_unique_search <- any(grepl("-", Plot_IDs, fixed=TRUE))
  
  if(!site_unique_search) {result <- sort(unique(response$site_location_name))}
  
  if(site_unique_search) {result <- sort(paste0(response$site_location_name, "-", response$site_location_visit_id))}
  
  return(result)
}

################

extract_site_info <- function(Plot_IDs) {
  path <- "site"
  result <- .ausplots_api_with_specified_plot_ids(path, Plot_IDs)
  result$state = mapply(function(x) substr(x, 0, 2), result$site_location_name)
  result$state[result$state == "NS"] <- "NSW"
  result$state[result$state == "QD"] <- "QLD"
  result$state[result$state == "TC"] <- "TAS"
  result$state[result$state == "VC"] <- "VIC"
  
  return(result)
} 

###############

extract_struct_summ <- function(Plot_IDs) {
  path <- "structural_summary"
  return(.ausplots_api_with_specified_plot_ids(path, Plot_IDs))
} 

####################

extract_soil_subsites <- function(Plot_IDs) {
  path <- "soil_subsite"
  return(.ausplots_api_with_specified_plot_ids(path, Plot_IDs))
}

##################

extract_bulk_density <- function(Plot_IDs) {
  path <- "soil_bulk_density"
  return(.ausplots_api_with_specified_plot_ids(path, Plot_IDs))
}

##################

extract_soil_char <- function(Plot_IDs) {
  path <- "soil_characterisation"
  return(.ausplots_api_with_specified_plot_ids(path, Plot_IDs))
}

##################

extract_basal <- function(Plot_IDs, herbarium_determination_search=NULL, family_search=NULL, standardised_name_search=NULL) {
  extra_query <- list() 
  path <- "veg_basal"
  if(!is.null(family_search)) {
    extra_query = append(extra_query, list("family" = paste("ilike.*", family_search, "*", sep="")))#search by family 
  }
  if(!is.null(herbarium_determination_search)) {
    extra_query = append(extra_query, list("herbarium_determination" = paste("ilike.*", herbarium_determination_search, "*", sep=""))) #search by herbarium_determination
  } 
  if(!is.null(standardised_name_search)) { 
    extra_query = append(extra_query, list("standardised_name" = paste("ilike.*", standardised_name_search, "*", sep="")))#search by standardised_name
  } 
  return(.ausplots_api_with_specified_plot_ids(path, Plot_IDs, extra_query))
}

############################

extract_vouch <- function(Plot_IDs, herbarium_determination_search=NULL, family_search=NULL, standardised_name_search=NULL) {
  extra_query <- list()
  path <- "veg_voucher"
  if(!is.null(family_search)) {
    extra_query = append(extra_query, list("family" = paste("ilike.*", family_search, "*", sep="")))#search by family 
  }
  if(!is.null(herbarium_determination_search)) {
    extra_query = append(extra_query, list("herbarium_determination" = paste("ilike.*", herbarium_determination_search, "*", sep=""))) #search by herbarium_determination
  } 
  if(!is.null(standardised_name_search)) { 
    extra_query = append(extra_query, list("standardised_name" = paste("ilike.*", standardised_name_search, "*", sep="")))#search by standardised_name
  } 
  return(.ausplots_api_with_specified_plot_ids(path, Plot_IDs, extra_query))
}

############################

extract_hits <- function(Plot_IDs) {
  path <- "veg_pi"
  return(.ausplots_api_with_specified_plot_ids(path, Plot_IDs))
}

############################

cache$user_agent <- NULL

.make_user_agent <- function() {
  # thanks https://github.com/r-lib/httr/blob/af25ebd0e3b72d2dc6e1423242b94efc25bc97cc/R/config.r#L137
  if (is.null(cache$user_agent)) {
    if (getOption("ausplotsR_api_debug", default = FALSE)) {
      message('building user_agent string and storing in cache')
    }
    versions <- c(
      ausplotsR = as.character(utils::packageVersion("ausplotsR")),
      libcurl = curl::curl_version()$version,
      `r-curl` = as.character(utils::packageVersion("curl")),
      httr = as.character(utils::packageVersion("httr"))
    )
    result <- paste0(names(versions), "/", versions, collapse = " ")
    cache$user_agent <- result
  }
  cache$user_agent
}

############################

cache$metadata_dictionary <- NULL

.get_metadata_dictionary <- function(force_refresh = FALSE) {
  if (is.null(cache$metadata_dictionary) || force_refresh) {
    if (getOption("ausplotsR_api_debug", default = FALSE)) {
      message('refreshing metadata_dictionary and storing in cache')
    }
    path <- "metadata-dictionary.json"
    result <- .ausplots_api(path, NULL)
    cache$metadata_dictionary <- result
  }
  cache$metadata_dictionary
}


################################
