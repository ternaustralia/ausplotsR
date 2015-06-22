#Vouchered records and species richness
#This script compiles the vouchered species records from each plot, as a separate exercise to compiling species records from the point intercept hits. As each species observed in the plot is vouchered, this represents a more complete list of species, although abundances only from PI datasets. 
#
#Authors:
#Greg Guerin

#setwd()

library(R.utils) #for capitalisation of names
library(plyr) #for count function

file_list_vouch <- Sys.glob("~/TERN_analysis/extractedData/veg/vegVouchers*.csv") #list of files with same prefix, in this case the vegVouchers files (list of vouchered species records from plots)

#function to open files off a list, and append the data to a single output file:
extract_vouch <- function(file_list) {
	working_file <- read.csv(file=file_list[1], sep=",", header=TRUE)	
	document <- as.data.frame(matrix(nrow=1, ncol=ncol(working_file))) 
	colnames(document) <- colnames(working_file)
	for (i in 1:length(file_list)) {
	working_file <- try(read.csv(file=file_list[i], sep=",", header=TRUE))
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
}

extract_vouch(file_list_vouch)

vouch <- read.table(file="vegVouchers_compiled.txt", sep=",", header=TRUE) #read this file back into R

#some cleaning operations on the names:
vouch$herbarium_determination <- trim.trailing(vouch$herbarium_determination)
vouch$herbarium_determination <- tolower(vouch$herbarium_determination)
vouch$herbarium_determination <- capitalize(vouch$herbarium_determination)

#calculate species richness per plot

richness <- count(vouch, c("site_location_name", "herbarium_determination")) #counts number of rows with same site and species name, and therefore removes duplicates
richness$freq <- 1 #to avoid weighting (each row adds one to count below)
richness <- count(richness, "site_location_name") #now count how many rows (spp) per plot - 'richness' is now a df of plots with their species richness

write.csv(richness, file="spp_rich.txt")