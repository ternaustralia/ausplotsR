#Basal Area
#This script compiles basal wedge data from plots and calculates metrics for Basal Area/tree density per plot and species.
#
#Authors:
#Greg Guerin

library(plyr)

file_list_basal <- Sys.glob("./extractedData/veg/vegBasal*.csv") #list of files with same prefix, in this case the vegBasal files

extract_basal <- function(file_list) {
	n <- 1
	working_file <- try(read.csv(file=file_list[n], sep=",", header=TRUE)) #some files may be empty if no scores taken or no tree for example, so needs to be generic 
	while(class(working_file)=="try-error") {
		n <- n + 1
		working_file <- try(read.csv(file=file_list[n], sep=",", header=TRUE))
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

extract_basal(file_list_basal) #run the compiling function

basal <- read.csv(file="vegBasal_compiled.txt") #read this file back into R. Contains the number of hits on each species in each plot per 9 reps, the basal area factor and calculated basal area.

basal$site_unique <- do.call(paste, c(basal[c("site_location_name", "site_location_visit_id")], sep = "-")) #add unique site/visit identifier

###################
#above will be in the get_ausplots function, below has been incorporated into the basal_area function
###########


bas_areas <- count(basal, c("site_unique", "point_id"), wt_var="basal_area") #the basal area scores for unique combos of site and point ID - i.e. for each of the 9 sample points in each plot, what is the total BA?

bas_areas_mean <- aggregate(bas_areas$freq, by=list(bas_areas$site_unique), FUN=mean) #mean of the basal areas for each of 9 sampling points in the plot. This is to give one value for each sampled plot, averaged across the 9 different sampling points and all species considered together.
names(bas_areas_mean) <- c("site_unique", "basal_area_m2_ha")

#Basal Area by species per plot, not combined as above:
bas_areas_spp_mean <- aggregate(basal$basal_area, by=list(basal$site_unique, basal$herbarium_determination), FUN=mean)
names(bas_areas_spp_mean) <- c("site_unique", "herbarium_determination", "basal_area_m2_ha")

#####
#Next, number of (tree) hits in the basal wedge sweep reps, giving a score related to tree density (there is no specific area as hits not confined to trees within the plot)

dens <- count(basal, c("site_unique", "point_id"), wt_var="hits") #number of hits for unique combos of site and point ID - i.e. for each of the 9 basal wedge sample points in each plot, what is the total number of scored trees for any spp recorded?

dens_mean <- aggregate(dens$freq, by=list(dens$site_unique), FUN=mean) #mean of the scores for each of the 9 reps - this is the average number of total tree hits for a rep at a given plot.
names(dens_mean) <- c("site_unique", "mean_hits")

#Hits by species:
dens_spp_mean <- aggregate(basal$hits, by=list(basal$site_unique, basal$herbarium_determination), FUN=mean)
names(dens_spp_mean) <- c("site_unique", "herbarium_determination", "mean_hits")

#write files
write.csv(bas_areas_mean, file="vegBasal_plot_means.txt")
write.csv(bas_areas_spp_mean, file="vegBasal_sppBYplot_means.txt")
write.csv(dens_mean, file="vegBasal_hits_plot_means.txt")
write.csv(dens_spp_mean, file="vegBasal_hits_sppBYplot_means.txt")



