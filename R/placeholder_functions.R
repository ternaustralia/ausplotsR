#Placeholder functions called within 'get_ausplots' that will need to query the database.

#Here they just read the same information written to file on local drive.

################

list_available_plots <- function(x) {
	
	data <- read.csv("~/TERN_analysis/extractedData/site/sitesAndDescriptions.csv")
	
	data <- data[ ,c("site_location_name", "longitude", "latitude")]
	
	return(data)
	
}


################

extract_site_info <- function(Plot_IDs) {
	
	data <- read.csv("~/TERN_analysis/extractedData/site/sitesAndDescriptions.csv")
	
	data <- data[which(data$site_location_name %in% Plot_IDs),]
	
	return(data)
	
} 

###############


extract_struct_summ <- function(Plot_IDs) {
	
	data <- read.csv("~/TERN_analysis/extractedData/site/structuralSummaries.csv")
	
	data <- data[which(data$site_location_name %in% Plot_IDs),]
	
	return(data)
	
} 

###############




#function to open files off a list, and append the data to a single output file (this is used for below):
	
	extract_csv <- function(file_list) {
		working_file <- read.csv(file=file_list[1])	
		document <- as.data.frame(matrix(nrow=1, ncol=ncol(working_file))) 
		colnames(document) <- colnames(working_file)
		for (i in 1:length(file_list)) {
			working_file <- try(read.csv(file=file_list[i]))
			if(class(working_file)=="try-error") {
			working_file <- NULL
		}
		if(class(working_file)=="data.frame") {
			document <- rbind(document, working_file)
		}
		message("Dataset added")
		message(i)
		}
	document <- document[-1,]
	return(document)
	} #close function



####################

extract_soil_subsites <- function(Plot_IDs) {
	
	file_list_sub <- Sys.glob("~/TERN_analysis/extractedData/soils/soilSubsites*.csv") #list of files with same prefix, in this case the soil subsites
	
	file_list_sub <- file_list_sub[unlist(lapply(Plot_IDs, function (x) {grep(x, file_list_sub)} ))] #cut available files to those matching Plot_IDs
	
	data <- extract_csv(file_list_sub) #run the function on the file list

	return(data)
	
}


##################
extract_bulk_density <- function(Plot_IDs) {
	
	file_list_bulk <- Sys.glob("~/TERN_analysis/extractedData/soils/soilBulkDensity*.csv") #list of files with same prefix, in this case the soil bulk density
	
	file_list_bulk <- file_list_bulk[unlist(lapply(Plot_IDs, function (x) {grep(x, file_list_bulk)} ))] #cut available files to those matching Plot_IDs
	
	data <- extract_csv(file_list_bulk) #run the function on the file list

	return(data)
	
}


##################

extract_soil_char <- function(Plot_IDs) {
	
	file_list_char <- Sys.glob("~/TERN_analysis/extractedData/soils/soilCharacterisation*.csv") #list of files with same prefix, in this case the soil characterisation
	
	file_list_char <- file_list_char[unlist(lapply(Plot_IDs, function (x) {grep(x, file_list_char)} ))] #cut available files to those matching Plot_IDs
	
	data <- extract_csv(file_list_char) #run the function on the file list
	
	return(data)
	
}


##################


extract_basal <- function(Plot_IDs) {

	file_list_bas <- Sys.glob("~/TERN_analysis/extractedData/veg/vegBasal*.csv") #list of files with same prefix, in this case the veg basal wedge
	
	file_list_bas <- file_list_bas[unlist(lapply(Plot_IDs, function (x) {grep(x, file_list_bas)} ))] #cut available files to those matching Plot_IDs
	
	data <- extract_csv(file_list_bas) #run the function on the file list
	
	return(data)
	
}



############################

extract_vouch <- function(Plot_IDs) {

	file_list_vouch <- Sys.glob("~/TERN_analysis/extractedData/veg/vegVouch*.csv") #list of files with same prefix, in this case the veg vouchers
	
	file_list_vouch <- file_list_vouch[unlist(lapply(Plot_IDs, function (x) {grep(x, file_list_vouch)} ))] #cut available files to those matching Plot_IDs
	
	data <- extract_csv(file_list_vouch) #run the function on the file list
	
	return(data)
	
}



############################


extract_hits <- function(Plot_IDs) {
	
	file_list_hits <- Sys.glob("~/TERN_analysis/extractedData/veg/vegPI*.csv") #list of files with same prefix, in this case the veg vouchers
	
	file_list_hits <- file_list_hits[unlist(lapply(Plot_IDs, function (x) {grep(x, file_list_hits)} ))] #cut available files to those matching Plot_IDs
	
	data <- extract_csv(file_list_hits) #run the function on the file list
	
	return(data)
	
}