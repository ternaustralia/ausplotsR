#Following 'compile_ausplots_vegPI.R', this script generates a species~sites occurrence matrix with the frequency of species within the 10 point intercept transects within each plot as values. 
#
#Authors:
#Greg Guerin

#setwd()




###now calcuate frequency - based on number of transects within plot recorded in

#code to tidy up redundant transect labels first:
hits$standard <- hits$transect #copy the vector which says which PI line the hit was on
delete <- grep("E1", hits$transect) #list of positions in the vector in which the text containts E1
hits$standard[delete] <- "E1-W1" # subsets the labels to those containing E1 and changes them to the same label

delete <- grep("E2", hits$transect)
hits$standard[delete] <- "E2-W2" 

delete <- grep("E3", hits$transect)
hits$standard[delete] <- "E3-W3"

delete <- grep("E4", hits$transect)
hits$standard[delete] <- "E4-W4"

delete <- grep("E5", hits$transect)
hits$standard[delete] <- "E5-W5"

delete <- grep("N1", hits$transect)
hits$standard[delete] <- "N1-S1"

delete <- grep("N2", hits$transect)
hits$standard[delete] <- "N2-S2" 

delete <- grep("N3", hits$transect)
hits$standard[delete] <- "N3-S3"

delete <- grep("N4", hits$transect)
hits$standard[delete] <- "N4-S4"

delete <- grep("N5", hits$transect)
hits$standard[delete] <- "N5-S5"
   

hits$standard <- as.factor(as.character(hits$standard)) #reset to get rid of existing 'levels'

#check:
levels(hits$standard) # should give the 10 standard ones only - although there now seems to be also "W2-S2" and "W5-.E" which make no sense [actually sometimes observer needs to do something fancy to fit a transect in, say if there is odd terrain...]

##now the freqs

transects <- count(hits, c("site_location_name", "herbarium_determination", "standard")) #count PI records for each uniqe plot/species/transect combo
transects$freq <- 1 #revert them to 1 (presence) - zero counts not included in outut
freqs <- count(transects, c("site_location_name", "herbarium_determination")) #now count transect presences per species per plot : if all smoothed out, should range from 1:10 for presences, but if 'weird' transects still in there, could be more
#OUTDATED...>#!!!!THERE ARE A COUPLE OF e.g.s where the transects are duplicated so you get preent E2-W2 as well as W2-E2, which appears to be the same transect... Maireana sedifolia, Atriplex stipitata ##glitch tidied above by converting all labels to 10 standard format ones, but still a couple of odd ones left

freqs$freq <- freqs$freq/10 #divide by ten to convert to actual frequency (should range from 0.1 to 1)

#Now, to be in the dataset, each species must have a freq>0 in at least one plot, so just 'mama' the freqs above
freq_matrix <- mama(freqs) #this is another species V sites matrix with the frequencies as values, and zeros if species occur on no transect for a plot
write.csv(freq_matrix, file="~/TERN_analysis/Compiled_datasets/sitesVspecies_freq_transects.txt")


