#Basal Area
#This script compiles basal wedge data from plots and calculates metrics for Basal Area/tree density per plot and species.
#
#Authors:
#Greg Guerin

#setwd()

library(plyr) #for count function

file_list_basal <- Sys.glob("~/TERN_analysis/extractedData/veg/vegBasal*.csv") #list of files with same prefix, in this case the vegBasal files

extract_basal <- function(file_list) {
	working_file <- try(read.csv(file=file_list[16], sep=",", header=TRUE)) #the 16th file happens to have data, whereas the first 15 are empty
	if(class(working_file)=="try-error") {
		stop("The template file may be empty.")
	}
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
write.csv(document, file="vegBasal_compiled.txt")
}

extract_basal(file_list_basal) #note:: at present if first file is empty, function crashes when attempting to read it as a template for compiling the rest. Have changed to read file no. 16 on the list as the template, as this has data. Would also be possible to create the same d.f. structure desired, but the current way allows that if the columns are changed in the database, it will still work


basal <- read.table(file="vegBasal_compiled.txt", sep=",", header=TRUE) #read this file back into R. Contains the number of hits on each species in each plot per 9 reps, the basl area factor and calculated basal area.

bas_areas <- count(basal, c("site_location_name", "point_id"), wt_var="basal_area") #the basal area scores for unique combos of site and point ID - i.e. for each of the 9 sample points in each plot, what is the total BA?

bas_areas_mean <- aggregate(bas_areas$freq, by=list(bas_areas$site_location_name), FUN=mean) #mean of the basal areas for each of 9 sampling points in the plot. This is to give one value for each sampled plot, averaged across the 9 different sampling points and all species considered together.

#Basal Area by species per plot, not combined as above:
bas_areas_spp_mean <- aggregate(basal$basal_area, by=list(basal$site_location_name, basal$herbarium_determination), FUN=mean)


#####
#Next, number of (tree) hits in the basal wedge sweep reps, giving a score related to tree density (there is no specific area as hits not confined to trees within the plot)

dens <- count(basal, c("site_location_name", "point_id"), wt_var="hits") #number of hits for unique combos of site and point ID - i.e. for each of the 9 basal wedge sample points in each plot, what is the total number of scored trees for any spp recorded?

dens_mean <- aggregate(dens$freq, by=list(dens$site_location_name), FUN=mean) #mean of the scores for each of the 9 reps - this is the average number of total tree hits for a rep at a given plot.

#Hits by species:
dens_spp_mean <- aggregate(basal$hits, by=list(basal$site_location_name, basal$herbarium_determination), FUN=mean)

#write files
write.csv(bas_areas_mean, file="vegBasal_plot_means.txt")
write.csv(bas_areas_spp_mean, file="vegBasal_sppBYplot_means.txt")
write.csv(dens_mean, file="vegBasal_hits_plot_means.txt")
write.csv(dens_spp_mean, file="vegBasal_hits_sppBYplot_means.txt")



