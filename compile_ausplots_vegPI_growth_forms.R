#Growth forms
##This script compiles information on plant growth forms present in plots based on point intercept records, for example lists of growth forms and their number of species/abundance plus growth form occurrence matrices for plot-plot comparison

#Compilation must follow: 'compile_ausplots_vegPI.R', as it uses previous objects

#Authors:
#Greg Guerin

#setwd()

library(simba) #for 'mama' function to generate occurrence matrices
library(plyr) # for 'count' function

#number of unique growth forms for each plot (single value)
growth_Form_diversity <- aggregate(hits$growth_form, by=list(hits$site_location_name), FUN=function(x) {length(unique(x))}) #where 'hits' is compiled point intercept data
colnames(growth_Form_diversity) <- c("site_location_name", "growth_form_diversity")
write.csv(growth_Form_diversity, file="growth_Form_diversity.txt", row.names=FALSE)


#list for each plot with growth forms that occur
growth_Form_types <- aggregate(hits$growth_form, by=list(hits$site_location_name), function(x) {as.character(unique(x))})
colnames(growth_Form_types) <- c("site_location_name", "growth_forms")
growth_Form_types$growth_forms <- as.character(growth_Form_types$growth_forms) # as has been made into a list
write.csv(growth_Form_types, file="growth_Form_types.txt", row.names=FALSE)

###
#Create presence/absence matrix for growth forms:
form_rows <- data.frame(site_location_name=hits$site_location_name, growth_form=hits$growth_form, dummy=rep(1, length(hits$growth_form))) #list of PI hits with what growth form they were
form_rows_NoDups <- form_rows[-which(duplicated(form_rows)==TRUE),] #remove duplicate rows, so left with individual rows for unique growth forms in each plot and a dummy '1' (for 'mama')
growthForm_PA <- mama(form_rows_NoDups)
write.csv(growthForm_PA, file="growthForm_PA.txt") #PA matrix of growth forms in plots

#############
#PI weights
#Repeating above generation of occurrence matrix for growth forms in plots but adding cover-abundance from point intercepts as weights
growthForm_matrix_PIweights <- mama(count(form_rows)[,-3]) #values are the number of hits a particular growth form got in PI
write.csv(growthForm_matrix_PIweights, file="growthForm_matrix_PIweights.txt")

#species 'richness' weights
#Repeating again to generate an occurrence matrix for growth forms in plots weighted this time by the number of species of a given growth form, rather than the PI scores of the growth form:
form_rows2 <- data.frame(site_location_name=hits$site_location_name, growth_form=hits$growth_form, herbarium_determination=hits$herbarium_determination)
form_rows_NoDups2 <- form_rows2[-which(duplicated(form_rows2)==TRUE),] #remove duplicates of species records
species_weights <- aggregate(form_rows_NoDups2, by=list(form_rows_NoDups2$site_location_name, form_rows_NoDups2$growth_form), FUN=function(x) {length(unique(x))}) #number of unique species with the same growth form in the same plot
species_weights <- species_weights[,-(3:4)] #remove redundant columns
growthForm_matrix_SPPweights <- mama(species_weights) #plots as rows, growth forms as columns, values are the number of species. Note sometimes the same species is recorded with different growth forms in a plot and therefore the rowSums of this matrix can be higher than the observed species richness because the same spp can count towards the weights for multiple growth forms
write.csv(growthForm_matrix_SPPweights, file="growthForm_matrix_SPPweights.txt") 
