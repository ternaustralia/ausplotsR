
species_table <- function(veg.PI, m_kind=c("PA", "percent_cover", "freq", "IVI"), cover_type=c("PFC", "OCC"), species_name=c("HD","SN","GS")) {

  
#assuming the follwing columns are available in standardised name, family, genus, genus_species, infraspecific taxa split up, taxonomic issue flag etc etc.
  

hits <- veg.PI

###################using herbarium determination################

if(species_name=="HD"){ 
  
  if(m_kind == "PA") {
      
      hits <- hits[which(!duplicated(hits[,c("site_unique", "herbarium_determination"),])), c("site_unique", "herbarium_determination")] #remove duplicated hits (i.e. same species in a given plot - we just want binary presence/absence here)
      
      hits <- stats::na.omit(hits) #remove hots not determined as a species
      
      hits$presence <- rep(1, nrow(hits)) #add a column of '1's for presence (for mama function)
      
      cover_matrix_binary <- simba::mama(hits) #generate an occurrence matrix (binary)
      
      species_matrix <- cover_matrix_binary
    } # end PA section
    

if(m_kind=="percent_cover" | m_kind == "IVI") {
	

#count combined (PI) cover scores for each species in each plot:
total.points.fun <- function(x) {return(length(unique(hits[which(hits$site_unique == x),]$hits_unique)))} #function to go through a list of plot names and count how many unique hits there were (for a standard plot, this will equal 1010)


total.points <- data.frame(site_unique = unique(hits$site_unique), total.points = unlist(lapply(unique(hits$site_unique), total.points.fun))) #site/visit and associated number of unique PI hits taken, by applying the above function

#remove in canopy sky hits if projected foliage cover required:
if(cover_type == "PFC") {
	hits <- hits[which(as.character(hits$in_canopy_sky) == "FALSE"),]
} #close if PFC


covers <- plyr::count(hits, c("site_unique", "herbarium_determination")) #counts number of rows with same site and species name

covers <- merge(covers, total.points, by="site_unique") #merge to get the hit counts for species in a df along with the total hits taken for each plot

covers$percent <- covers$freq/covers$total.points*100 #add a new column to convert from number of hits to percent. Some plots have less or more than the standard 1010 PI hits for various reasons, so this ensures dividing by the actual number of unique hits, which is calculated from the raw data and unique transect/number combos

covers <- stats::na.omit(covers) #to remove percents for records with no herbarium determination


#generate sites by species matrix:
cover_matrix <- simba::mama(covers[,-c(3, 4)]) #third/fourth columns (count of individual hits and total hits) excluded as we want to the mama function to read our fourth column 'percent' [percent cover] as the abundance value (assumed to be the 3rd column): this generates a data.frame with sites as rows and species as columns, with percent covers from PI hits as values

species_matrix <- cover_matrix
} # close cover matrix section



if(m_kind=="freq" | m_kind == "IVI") {
	
	
	hits <- stats::na.omit(hits) #remove NA herbarium determinations
	
	transects <- plyr::count(hits, c("site_unique", "herbarium_determination", "transect")) #count PI records for each uniqe plot/species/transect combo
	
	transects$freq <- 1 #revert to 1 rather than a count (presence on a transect within a plot)
	
	freqs <- plyr::count(transects, c("site_unique", "herbarium_determination")) #count transect presences per species per plot, e.g. 5 means that species was recorded on 5 of the transects
	
	number.transects <- plyr::count(transects[!duplicated(transects[,c("site_unique", "transect")]),], "site_unique") #some plots have more or less than 10 transects so this is a data frame of site_unique IDs and the number of transects
	
	freqs <- merge(freqs, number.transects, by="site_unique") #this maps the number of recorded transects for each plot to the species records and number of transect hits for each. Columns are now freq.x and freq.y
	
	freqs$freq.z <- freqs$freq.x/freqs$freq.y #divide by the actual number of transects (should range from 0.1 to 1)
	
	freq_matrix <- simba::mama(freqs[,c(1,2,5),]) #create a species ~ sites matrix with the frequencies as values, and zeros if species occur on no transect for a plot; selecting columns 1,2 and 5 to exclude the freq.x and freq.y cols for mam
	
	species_matrix <- freq_matrix


} #end M-kind == freq



if(m_kind == "IVI") {
	freq_matrix_percents <- freq_matrix*100 #convert to %
		
	IVI <- (cover_matrix + freq_matrix_percents)/2  #combine freq and cover to get IVI
	
	species_table <-  IVI #note that the frequencies appear to dominate the output as they are overall much higher than the covers

} #end IVI section
} # end species_name=="HD"


########################using standardised_name###############

if(species_name=="SN"){ #using standardised_name
  
  if(m_kind == "PA") {
    
    hits <- hits[which(!duplicated(hits[,c("site_unique", "standardised_name"),])), c("site_unique", "standardised_name")] #remove duplicated hits (i.e. same species in a given plot - we just want binary presence/absence here)
    
    hits <- stats::na.omit(hits) #remove hots not determined as a species
    
    hits$presence <- rep(1, nrow(hits)) #add a column of '1's for presence (for mama function)
    
    cover_matrix_binary <- simba::mama(hits) #generate an occurrence matrix (binary)
    
    species_matrix <- cover_matrix_binary
  } # end PA section
  
  
  if(m_kind=="percent_cover" | m_kind == "IVI") {
    
    
    #count combined (PI) cover scores for each species in each plot:
    total.points.fun <- function(x) {return(length(unique(hits[which(hits$site_unique == x),]$hits_unique)))} #function to go through a list of plot names and count how many unique hits there were (for a standard plot, this will equal 1010)
    
    
    total.points <- data.frame(site_unique = unique(hits$site_unique), total.points = unlist(lapply(unique(hits$site_unique), total.points.fun))) #site/visit and associated number of unique PI hits taken, by applying the above function
    
    #remove in canopy sky hits if projected foliage cover required:
    if(cover_type == "PFC") {
      hits <- hits[which(as.character(hits$in_canopy_sky) == "FALSE"),]
    } #close if PFC
    
    
    covers <- plyr::count(hits, c("site_unique", "standardised_name")) #counts number of rows with same site and species name
    
    covers <- merge(covers, total.points, by="site_unique") #merge to get the hit counts for species in a df along with the total hits taken for each plot
    
    covers$percent <- covers$freq/covers$total.points*100 #add a new column to convert from number of hits to percent. Some plots have less or more than the standard 1010 PI hits for various reasons, so this ensures dividing by the actual number of unique hits, which is calculated from the raw data and unique transect/number combos
    
    covers <- stats::na.omit(covers) #to remove percents for records with no herbarium determination
    
    
    #generate sites by species matrix:
    cover_matrix <- simba::mama(covers[,-c(3, 4)]) #third/fourth columns (count of individual hits and total hits) excluded as we want to the mama function to read our fourth column 'percent' [percent cover] as the abundance value (assumed to be the 3rd column): this generates a data.frame with sites as rows and species as columns, with percent covers from PI hits as values
    
    species_matrix <- cover_matrix
  } # close cover matrix section
  
  
  
  if(m_kind=="freq" | m_kind == "IVI") {
    
    
    hits <- stats::na.omit(hits) #remove NA herbarium determinations
    
    transects <- plyr::count(hits, c("site_unique", "standardised_name", "transect")) #count PI records for each uniqe plot/species/transect combo
    
    transects$freq <- 1 #revert to 1 rather than a count (presence on a transect within a plot)
    
    freqs <- plyr::count(transects, c("site_unique", "standardised_name")) #count transect presences per species per plot, e.g. 5 means that species was recorded on 5 of the transects
    
    number.transects <- plyr::count(transects[!duplicated(transects[,c("site_unique", "transect")]),], "site_unique") #some plots have more or less than 10 transects so this is a data frame of site_unique IDs and the number of transects
    
    freqs <- merge(freqs, number.transects, by="site_unique") #this maps the number of recorded transects for each plot to the species records and number of transect hits for each. Columns are now freq.x and freq.y
    
    freqs$freq.z <- freqs$freq.x/freqs$freq.y #divide by the actual number of transects (should range from 0.1 to 1)
    
    freq_matrix <- simba::mama(freqs[,c(1,2,5),]) #create a species ~ sites matrix with the frequencies as values, and zeros if species occur on no transect for a plot; selecting columns 1,2 and 5 to exclude the freq.x and freq.y cols for mam
    
    species_matrix <- freq_matrix
    
    
  } #end M-kind == freq
  
  
  
  if(m_kind == "IVI") {
    freq_matrix_percents <- freq_matrix*100 #convert to %
    
    IVI <- (cover_matrix + freq_matrix_percents)/2  #combine freq and cover to get IVI
    
    species_table <-  IVI #note that the frequencies appear to dominate the output as they are overall much higher than the covers
    
  } #end IVI section
} # end species_name=="HD"


#########genus_species##########################


if(species_name=="GS"){ #using genus species
  
  #need to remove no data celss
  
  hits<-hits[!is.na(hits$genus_species), ]#assumes that if a genus_species identification is not possible, the cell value for the row will be NA
  
  #alternatively, we might assign it a no data term, in which case
  #hits<-hits[!(hits$genus_species=="No ID",]
  
  
  if(m_kind == "PA") {
    
    hits <- hits[which(!duplicated(hits[,c("site_unique", "genus_species"),])), c("site_unique", "genus_species")] #remove duplicated hits (i.e. same species in a given plot - we just want binary presence/absence here)
    
    hits <- stats::na.omit(hits) #remove hots not determined as a species
    
    hits$presence <- rep(1, nrow(hits)) #add a column of '1's for presence (for mama function)
    
    cover_matrix_binary <- simba::mama(hits) #generate an occurrence matrix (binary)
    
    species_matrix <- cover_matrix_binary
  } # end PA section
  
  
  if(m_kind=="percent_cover" | m_kind == "IVI") {
    
    
    #count combined (PI) cover scores for each species in each plot:
    total.points.fun <- function(x) {return(length(unique(hits[which(hits$site_unique == x),]$hits_unique)))} #function to go through a list of plot names and count how many unique hits there were (for a standard plot, this will equal 1010)
    
    
    total.points <- data.frame(site_unique = unique(hits$site_unique), total.points = unlist(lapply(unique(hits$site_unique), total.points.fun))) #site/visit and associated number of unique PI hits taken, by applying the above function
    
    #remove in canopy sky hits if projected foliage cover required:
    if(cover_type == "PFC") {
      hits <- hits[which(as.character(hits$in_canopy_sky) == "FALSE"),]
    } #close if PFC
    
    
    covers <- plyr::count(hits, c("site_unique", "genus_species")) #counts number of rows with same site and species name
    
    covers <- merge(covers, total.points, by="site_unique") #merge to get the hit counts for species in a df along with the total hits taken for each plot
    
    covers$percent <- covers$freq/covers$total.points*100 #add a new column to convert from number of hits to percent. Some plots have less or more than the standard 1010 PI hits for various reasons, so this ensures dividing by the actual number of unique hits, which is calculated from the raw data and unique transect/number combos
    
    covers <- stats::na.omit(covers) #to remove percents for records with no herbarium determination
    
    
    #generate sites by species matrix:
    cover_matrix <- simba::mama(covers[,-c(3, 4)]) #third/fourth columns (count of individual hits and total hits) excluded as we want to the mama function to read our fourth column 'percent' [percent cover] as the abundance value (assumed to be the 3rd column): this generates a data.frame with sites as rows and species as columns, with percent covers from PI hits as values
    
    species_matrix <- cover_matrix
  } # close cover matrix section
  
  
  
  if(m_kind=="freq" | m_kind == "IVI") {
    
    
    hits <- stats::na.omit(hits) #remove NA herbarium determinations
    
    transects <- plyr::count(hits, c("site_unique", "genus_species", "transect")) #count PI records for each uniqe plot/species/transect combo
    
    transects$freq <- 1 #revert to 1 rather than a count (presence on a transect within a plot)
    
    freqs <- plyr::count(transects, c("site_unique", "genus_species")) #count transect presences per species per plot, e.g. 5 means that species was recorded on 5 of the transects
    
    number.transects <- plyr::count(transects[!duplicated(transects[,c("site_unique", "transect")]),], "site_unique") #some plots have more or less than 10 transects so this is a data frame of site_unique IDs and the number of transects
    
    freqs <- merge(freqs, number.transects, by="site_unique") #this maps the number of recorded transects for each plot to the species records and number of transect hits for each. Columns are now freq.x and freq.y
    
    freqs$freq.z <- freqs$freq.x/freqs$freq.y #divide by the actual number of transects (should range from 0.1 to 1)
    
    freq_matrix <- simba::mama(freqs[,c(1,2,5),]) #create a species ~ sites matrix with the frequencies as values, and zeros if species occur on no transect for a plot; selecting columns 1,2 and 5 to exclude the freq.x and freq.y cols for mam
    
    species_matrix <- freq_matrix
    
    
  } #end M-kind == freq
  
  
  
  if(m_kind == "IVI") {
    freq_matrix_percents <- freq_matrix*100 #convert to %
    
    IVI <- (cover_matrix + freq_matrix_percents)/2  #combine freq and cover to get IVI
    
    species_table <-  IVI #note that the frequencies appear to dominate the output as they are overall much higher than the covers
    
  } #end IVI section
} # end species_


if(species_name=="HD") warning("'herbarium_determination' species names are provided by state herbariums and are the most commonly used scientific names in the given state. However, scientific names may vary between states due to disagreements on taxonomy/nomenclature. To ensure consistency between all plots, we recommend using the 'standardised_name' or 'genus_species' for species_table calculations")

return(species_matrix)
	
} #end function

