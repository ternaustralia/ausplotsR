#Growth forms
##This script compiles information on plant growth forms present in plots based on point intercept records, for example lists of growth forms and their number of species/abundance plus growth form occurrence matrices for plot-plot comparison.

#Probably the most useful dataset from this is the growth forms against site/visit with percent cover, which can be used in manova, ordination, classification etc at continental scale where species turnover is too high for some methods to provide meaningful results, also give a kind of multivariate structural and functional response variable.

#Compilation must follow: 'compile_ausplots_vegPI.R', as it uses previous objects, especially 'hits'

#Authors:
#Greg Guerin

library(simba)
library(plyr)

#number of unique growth forms for each plot (single value)
growth_Form_diversity <- aggregate(hits$growth_form, by=list(hits$site_unique), FUN=function(x) {length(unique(x))}) #where 'hits' is the already compiled point intercept data
colnames(growth_Form_diversity) <- c("site_unique", "growth_form_diversity")
write.csv(growth_Form_diversity, file="growth_Form_diversity.txt", row.names=FALSE)


#list for each plot with growth forms that occur
growth_Form_types <- aggregate(hits$growth_form, by=list(hits$site_unique), function(x) {as.character(unique(x))})
colnames(growth_Form_types) <- c("site_unique", "growth_forms")
growth_Form_types$growth_forms <- as.character(growth_Form_types$growth_forms) # as has been made into a list
write.csv(growth_Form_types, file="growth_Form_types.txt", row.names=FALSE)

###
#Create presence/absence matrix for growth forms:
form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were. Hits are currently included and not differentiated if they are in canopy sky, but this can be updated. It may not matter for this part as it is just presence of GFs and if e.g. TREE is scores as in canopy sky, then it presumably is always going to be a present GF.
form_rows_NoDups <- form_rows[-which(duplicated(form_rows)==TRUE),] #remove duplicate rows, so left with individual rows for unique growth forms in each plot and a dummy '1' (for 'mama')
growthForm_PA <- mama(form_rows_NoDups)
write.csv(growthForm_PA, file="growthForm_PA.txt") #PA (presence/absence) matrix of growth forms in plots

#############
#PI weights
#Repeating above generation of occurrence matrix for growth forms in plots but adding cover-abundance from point intercepts as weights instead of just presence as '1'.
##################
#NOTE: At present, this is calculating opaque cover that includes in canopy sky hits as this is often appropriate/needed for interpretation/validation of satellite images, where as in the species cover matrices calculated in other compilation scripts, in canopy sky hits have been excluded to get projected foliage cover (but this can be changed and a function should have the option as an argument). As noted in the previous PI script, it would be useful to roll into a function in which an argument lets the user chose between opaque and projected foliage cover
################
growthForm_matrix_PIweights <- mama(count(form_rows)[,-3]) #values are the raw number of hits a particular growth form got in PI for each plot/visit
#However, to convert that to a percent cover, it needs to be divided by the actual number of unique PI hits for that particular plot and multiplied by 100 (not all plots have exactly 1010 unique hits)
for(i in row.names(growthForm_matrix_PIweights)) {
	growthForm_matrix_PIweights[i,] <- growthForm_matrix_PIweights[i,]/total.points$total.points[which(total.points$site_unique == i)]*100
} #where total.points is the previous calculation of the actual total number of PI hits for each plot - just a df with the site_uniuque field and a value for hits which may be 1010 or otherwise. NOT the above clunky process was needed because each row needed to be divided by a different number.
write.csv(growthForm_matrix_PIweights, file="growthForm_matrix_PIweights.txt")

#species 'richness' weights
#Repeating again to generate an occurrence matrix for growth forms in plots weighted this time by the number of species of a given growth form, rather than the PI scores of the growth form:
form_rows2 <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, herbarium_determination=hits$herbarium_determination)
form_rows_NoDups2 <- form_rows2[-which(duplicated(form_rows2)==TRUE),] #remove duplicates of species records
species_weights <- aggregate(form_rows_NoDups2, by=list(form_rows_NoDups2$site_unique, form_rows_NoDups2$growth_form), FUN=function(x) {length(unique(x))}) #number of unique species with the same growth form in the same plot
species_weights <- species_weights[,-(3:4)] #remove redundant columns
growthForm_matrix_SPPweights <- mama(species_weights) #plots as rows, growth forms as columns, values are the number of species. Note sometimes the same species is recorded with different growth forms in a plot and therefore the rowSums of this matrix can be higher than the observed species richness because the same spp can count towards the weights for multiple growth forms
write.csv(growthForm_matrix_SPPweights, file="growthForm_matrix_SPPweights.txt") 
