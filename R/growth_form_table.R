growth_form_table <- function(veg.PI, m_kind=c("PA", "percent_cover", "richness"), cover_type=c("PFC", "OCC"), cumulative=TRUE, by_strata=FALSE, species_name=c("HD","SN","GS")) {


#input checks
if(!class(veg.PI) == "data.frame") {stop("veg.PI must be a data.frame")}
if(any(!c("growth_form", "site_unique") %in% names(veg.PI))) {stop("Can't match names of veg.PI; data frame should be returned from get_ausplots")}
if(!is.logical(cumulative)) {stop("cumulative? must be logical (TRUE/FALSE)")}
if(!is.logical(by_strata)) {stop("by_strata? must be logical (TRUE/FALSE)")}
if(!is.character(m_kind)) {stop("m_kind must be a character vector")}
if(!is.character(cover_type)) {stop("cover_type must be a character vector")}
if(species_name=c("HD")) warning("'herbarium_determination' species names are provided by state herbariums and are the most commonly used scientific names in the given state. However, scientific names may vary between states due to disagreements on taxonomy/nomenclature. To ensure consistency between all plots, we recommend using the 'standardised_name' of 'genus_species' for growth form richness calculations")

#veg-hits
#this will need to change to be more specific because of additional columns
#default will be SN

hits<-veg.PI[!is.na(veg.PI$standardised_name), ] #remove hits not determined as a species - it should only be substrate-only hits that have any NAs, as all fields are relevant when there is a species hit; this is used for summing GF cover


#remove 'in canopy sky' hits if projected foliage cover required:
if(cover_type == "PFC") {
	hits <- hits[which(as.character(hits$in_canopy_sky) == "FALSE"),]
} #close if PFC



if(m_kind == "PA") {
	
	form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were - simplified from input df for processing
	
	form_rows_NoDups <- form_rows[-which(duplicated(form_rows)),] #remove duplicate rows, so left with individual rows for unique growth forms in each plot and a dummy '1' (for 'mama')

	growthForm_PA <- simba::mama(form_rows_NoDups)

	growth_form_matrix <- growthForm_PA

} # end PA section




if(m_kind =="percent_cover") {
	
	if(by_strata) {
	hits$growth_form[hits$growth_form %in% c("Tree Mallee", "Tree/Palm", "Tree-fern", "Epiphyte")] <- "Upper"
	hits$growth_form[hits$growth_form %in% c("Shrub Mallee", "Shrub", "Grass-tree", "Chenopod", "Heath-shrub")] <- "Mid"
	hits$growth_form[hits$growth_form %in% c("Tussock grass", "Forb", "Vine", "Hummock grass", "Fern", "Sedge", "Rush")] <- "Lower"
	hits <- hits[(hits$growth_form %in% c("Upper", "Mid", "Lower")),]
}
	
	if(cumulative) {
		form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were - simplified from input df for processing	
	}
	
	
	if(!cumulative) {
		form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, hits_unique=hits$hits_unique, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were - simplified from input df for processing
		form_rows <- form_rows[-which(duplicated(form_rows)),] #in this case, remove rows that are duplicated, i.e. same site_unique, same growth form, AND same hits_unique, so that there can only be one score for each GF at a point, meaning absolute cover with a max of 100% for each growth form.
		form_rows <- form_rows[,-3] #remove the hits_unique column now that it is unique per GF for mama processing later. The data now only include unique hite by GF by point, not unique by species too.	
	}
	
	
	total.points.fun <- function(x) {return(length(unique(veg.PI[which(veg.PI$site_unique == x),]$hits_unique)))} #function to go through a list of plot names and count how many unique hits there were (for a standard plot, this will equal 1010 but not always the case). NB using the veg.PI input that has all hits, not the 'hits' object that has only plant hits included.
	
	total.points <- data.frame(site_unique = unique(veg.PI$site_unique), total.points = unlist(lapply(unique(veg.PI$site_unique), total.points.fun))) #site/visit and associated number of unique PI hits taken, by applying the above function

	growthForm_matrix_PIweights <- simba::mama(count(form_rows)[,-3]) #values are the raw number of hits a particular growth form got in PI for each plot/visit
	
	#However, to convert that to a percent cover, it needs to be divided by the actual number of unique PI hits for that particular plot and multiplied by 100 (not all plots have exactly 1010 unique hits):
	for(i in row.names(growthForm_matrix_PIweights)) {
		growthForm_matrix_PIweights[i,] <- growthForm_matrix_PIweights[i,]/total.points$total.points[which(total.points$site_unique == i)]*100
		} #where total.points is the previous calculation of the actual total number of PI hits for each plot - just a df with the site_uniuque field and a value for hits which may be 1010 or otherwise. NOTE the above clunky process was needed because each row needed to be divided by a different number. There may be a simpler matrix operation that would do this correctly, but this although slow ensures the correct matching site_unique values are matched.
		
		growth_form_matrix <- growthForm_matrix_PIweights	
	} #end m_kind = %cover



if(m_kind=="richness") {
	
	form_rows2 <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, standardised_name=hits$standardised_name)
	
	form_rows_NoDups2 <- form_rows2[-which(duplicated(form_rows2)==TRUE),] #remove duplicates of species records, so just a single hit per species per plot
	
	species_weights <- stats::aggregate(form_rows_NoDups2, by=list(form_rows_NoDups2$site_unique, form_rows_NoDups2$growth_form), FUN=function(x) {length(unique(x))}) #number of unique species with the same growth form in the same plot
	
	species_weights <- species_weights[,-(3:4)] #remove redundant columns
	
	growthForm_matrix_SPPweights <- simba::mama(species_weights) #plots as rows, growth forms as columns, values are the number of species.

growth_form_matrix <- growthForm_matrix_SPPweights


} #end richness weights

####################if species_name=HD
  
if(species_name=="HD"){
  hits<-veg.PI[!is.na(veg.PI$herbarium_determination), ] #remove hits not determined as a species - it should only be substrate-only hits that have any NAs, as all fields are relevant when there is a species hit; this is used for summing GF cover
  
  #remove 'in canopy sky' hits if projected foliage cover required:
  if(cover_type == "PFC") {
    hits <- hits[which(as.character(hits$in_canopy_sky) == "FALSE"),]
  } #close if PFC
  
  
  
  if(m_kind == "PA") {
    
    form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were - simplified from input df for processing
    
    form_rows_NoDups <- form_rows[-which(duplicated(form_rows)),] #remove duplicate rows, so left with individual rows for unique growth forms in each plot and a dummy '1' (for 'mama')
    
    growthForm_PA <- simba::mama(form_rows_NoDups)
    
    growth_form_matrix <- growthForm_PA
    
  } # end PA section
  
  
  
  
  if(m_kind =="percent_cover") {
    
    if(by_strata) {
      hits$growth_form[hits$growth_form %in% c("Tree Mallee", "Tree/Palm", "Tree-fern", "Epiphyte")] <- "Upper"
      hits$growth_form[hits$growth_form %in% c("Shrub Mallee", "Shrub", "Grass-tree", "Chenopod", "Heath-shrub")] <- "Mid"
      hits$growth_form[hits$growth_form %in% c("Tussock grass", "Forb", "Vine", "Hummock grass", "Fern", "Sedge", "Rush")] <- "Lower"
      hits <- hits[(hits$growth_form %in% c("Upper", "Mid", "Lower")),]
    }
    
    if(cumulative) {
      form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were - simplified from input df for processing	
    }
    
    
    if(!cumulative) {
      form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, hits_unique=hits$hits_unique, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were - simplified from input df for processing
      form_rows <- form_rows[-which(duplicated(form_rows)),] #in this case, remove rows that are duplicated, i.e. same site_unique, same growth form, AND same hits_unique, so that there can only be one score for each GF at a point, meaning absolute cover with a max of 100% for each growth form.
      form_rows <- form_rows[,-3] #remove the hits_unique column now that it is unique per GF for mama processing later. The data now only include unique hite by GF by point, not unique by species too.	
    }
    
    
    total.points.fun <- function(x) {return(length(unique(veg.PI[which(veg.PI$site_unique == x),]$hits_unique)))} #function to go through a list of plot names and count how many unique hits there were (for a standard plot, this will equal 1010 but not always the case). NB using the veg.PI input that has all hits, not the 'hits' object that has only plant hits included.
    
    total.points <- data.frame(site_unique = unique(veg.PI$site_unique), total.points = unlist(lapply(unique(veg.PI$site_unique), total.points.fun))) #site/visit and associated number of unique PI hits taken, by applying the above function
    
    growthForm_matrix_PIweights <- simba::mama(count(form_rows)[,-3]) #values are the raw number of hits a particular growth form got in PI for each plot/visit
    
    #However, to convert that to a percent cover, it needs to be divided by the actual number of unique PI hits for that particular plot and multiplied by 100 (not all plots have exactly 1010 unique hits):
    for(i in row.names(growthForm_matrix_PIweights)) {
      growthForm_matrix_PIweights[i,] <- growthForm_matrix_PIweights[i,]/total.points$total.points[which(total.points$site_unique == i)]*100
    } #where total.points is the previous calculation of the actual total number of PI hits for each plot - just a df with the site_uniuque field and a value for hits which may be 1010 or otherwise. NOTE the above clunky process was needed because each row needed to be divided by a different number. There may be a simpler matrix operation that would do this correctly, but this although slow ensures the correct matching site_unique values are matched.
    
    growth_form_matrix <- growthForm_matrix_PIweights	
  } #end m_kind = %cover
  
  
  
  if(m_kind=="richness") {
    
    form_rows2 <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, herbarium_determination=hits$herbarium_determination)
    
    form_rows_NoDups2 <- form_rows2[-which(duplicated(form_rows2)==TRUE),] #remove duplicates of species records, so just a single hit per species per plot
    
    species_weights <- stats::aggregate(form_rows_NoDups2, by=list(form_rows_NoDups2$site_unique, form_rows_NoDups2$growth_form), FUN=function(x) {length(unique(x))}) #number of unique species with the same growth form in the same plot
    
    species_weights <- species_weights[,-(3:4)] #remove redundant columns
    
    growthForm_matrix_SPPweights <- simba::mama(species_weights) #plots as rows, growth forms as columns, values are the number of species.
    
    growth_form_matrix <- growthForm_matrix_SPPweights
    
    
  } #end richness weights
} #end species_name=HD


####################if species_name=GS

if(species_name=="GS"){
  hits<-veg.PI[!is.na(veg.PI$genus_species), ] #remove hits not determined as a species - it should only be substrate-only hits that have any NAs, as all fields are relevant when there is a species hit; this is used for summing GF cover
  
  #remove 'in canopy sky' hits if projected foliage cover required:
  if(cover_type == "PFC") {
    hits <- hits[which(as.character(hits$in_canopy_sky) == "FALSE"),]
  } #close if PFC
  
  
  
  if(m_kind == "PA") {
    
    form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were - simplified from input df for processing
    
    form_rows_NoDups <- form_rows[-which(duplicated(form_rows)),] #remove duplicate rows, so left with individual rows for unique growth forms in each plot and a dummy '1' (for 'mama')
    
    growthForm_PA <- simba::mama(form_rows_NoDups)
    
    growth_form_matrix <- growthForm_PA
    
  } # end PA section
  
  
  
  
  if(m_kind =="percent_cover") {
    
    if(by_strata) {
      hits$growth_form[hits$growth_form %in% c("Tree Mallee", "Tree/Palm", "Tree-fern", "Epiphyte")] <- "Upper"
      hits$growth_form[hits$growth_form %in% c("Shrub Mallee", "Shrub", "Grass-tree", "Chenopod", "Heath-shrub")] <- "Mid"
      hits$growth_form[hits$growth_form %in% c("Tussock grass", "Forb", "Vine", "Hummock grass", "Fern", "Sedge", "Rush")] <- "Lower"
      hits <- hits[(hits$growth_form %in% c("Upper", "Mid", "Lower")),]
    }
    
    if(cumulative) {
      form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were - simplified from input df for processing	
    }
    
    
    if(!cumulative) {
      form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, hits_unique=hits$hits_unique, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were - simplified from input df for processing
      form_rows <- form_rows[-which(duplicated(form_rows)),] #in this case, remove rows that are duplicated, i.e. same site_unique, same growth form, AND same hits_unique, so that there can only be one score for each GF at a point, meaning absolute cover with a max of 100% for each growth form.
      form_rows <- form_rows[,-3] #remove the hits_unique column now that it is unique per GF for mama processing later. The data now only include unique hite by GF by point, not unique by species too.	
    }
    
    
    total.points.fun <- function(x) {return(length(unique(veg.PI[which(veg.PI$site_unique == x),]$hits_unique)))} #function to go through a list of plot names and count how many unique hits there were (for a standard plot, this will equal 1010 but not always the case). NB using the veg.PI input that has all hits, not the 'hits' object that has only plant hits included.
    
    total.points <- data.frame(site_unique = unique(veg.PI$site_unique), total.points = unlist(lapply(unique(veg.PI$site_unique), total.points.fun))) #site/visit and associated number of unique PI hits taken, by applying the above function
    
    growthForm_matrix_PIweights <- simba::mama(count(form_rows)[,-3]) #values are the raw number of hits a particular growth form got in PI for each plot/visit
    
    #However, to convert that to a percent cover, it needs to be divided by the actual number of unique PI hits for that particular plot and multiplied by 100 (not all plots have exactly 1010 unique hits):
    for(i in row.names(growthForm_matrix_PIweights)) {
      growthForm_matrix_PIweights[i,] <- growthForm_matrix_PIweights[i,]/total.points$total.points[which(total.points$site_unique == i)]*100
    } #where total.points is the previous calculation of the actual total number of PI hits for each plot - just a df with the site_uniuque field and a value for hits which may be 1010 or otherwise. NOTE the above clunky process was needed because each row needed to be divided by a different number. There may be a simpler matrix operation that would do this correctly, but this although slow ensures the correct matching site_unique values are matched.
    
    growth_form_matrix <- growthForm_matrix_PIweights	
  } #end m_kind = %cover
  
  
  
  if(m_kind=="richness") {
    
    form_rows2 <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, genus_species=hits$genus_species)
    
    form_rows_NoDups2 <- form_rows2[-which(duplicated(form_rows2)==TRUE),] #remove duplicates of species records, so just a single hit per species per plot
    
    species_weights <- stats::aggregate(form_rows_NoDups2, by=list(form_rows_NoDups2$site_unique, form_rows_NoDups2$growth_form), FUN=function(x) {length(unique(x))}) #number of unique species with the same growth form in the same plot
    
    species_weights <- species_weights[,-(3:4)] #remove redundant columns
    
    growthForm_matrix_SPPweights <- simba::mama(species_weights) #plots as rows, growth forms as columns, values are the number of species.
    
    growth_form_matrix <- growthForm_matrix_SPPweights
    
    
  } #end richness weights
} #end species_name=GS
  

return(growth_form_matrix)
	
} #end function
