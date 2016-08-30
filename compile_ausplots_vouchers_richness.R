#Vouchered species records.

#This script compiles the vouchered species records from each AusPlots site. This is a separate exercise to compiling species records from the point intercept hits. As each species observed in the plot is vouchered, this represents a more complete list of species, although abundances are only available from PI the dataset. 
#
#Authors:
#Greg Guerin

#setwd()

library(R.utils) #for capitalisation of names
library(plyr) #for count function

file_list_vouch <- Sys.glob("./extractedData/veg/vegVouchers*.csv") #list of files with same prefix, in this case the vegVouchers files (list of vouchered species records from plots)

#function to open files off a list, and append the data to a single output file:
extract_vouch <- function(file_list) {
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
write.csv(document, file="vegVouchers_compiled.txt")
} #close function

extract_vouch(file_list_vouch) #run the function on the file list

vouch <- read.csv(file="vegVouchers_compiled.txt") #read this file back into R

#some cleaning operations on the names:
vouch$herbarium_determination <- trim.trailing(vouch$herbarium_determination)
vouch$herbarium_determination <- tolower(vouch$herbarium_determination)
vouch$herbarium_determination <- capitalize(vouch$herbarium_determination)

vouch$site_unique <- do.call(paste, c(vouch[c("site_location_name", "site_location_visit_id")], sep = "-"))
