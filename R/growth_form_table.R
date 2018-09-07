
growth_form_table <- function(veg.PI, m_kind=c("PA", "percent_cover", "richness"), cover_type=c("PFC", "OCC")) {


hits <- veg.PI #to match older code with newer input variable name
hits <- stats::na.omit(hits) #remove hits not determined as a species


#remove in canopy sky hits if projected foliage cover required:
if(cover_type == "PFC") {
	hits <- hits[which(as.character(hits$in_canopy_sky) == "FALSE"),]
} #close if PFC




if(m_kind == "PA" | m_kind == "percent_cover") {
	form_rows <- data.frame(site_unique=hits$site_unique, growth_form=hits$growth_form, dummy=rep(1, length(hits$growth_form))) #list of individual PI hits with what growth form they were
	} #end if PA or %cover




if(m_kind == "PA") {
	
	form_rows_NoDups <- form_rows[-which(duplicated(form_rows)==TRUE),] #remove duplicate rows, so left with individual rows for unique growth forms in each plot and a dummy '1' (for 'mama')

	growthForm_PA <- simba::mama(form_rows_NoDups)

	growth_form_matrix <- growthForm_PA

} # end PA section




if(m_kind =="percent_cover") {
	
	total.points.fun <- function(x) {return(length(unique(hits[which(hits$site_unique == x),]$hits_unique)))} #function to go through a list of plot names and count how many unique hits there were (for a standard plot, this will equal 1010b but not always the case)
	
	total.points <- data.frame(site_unique = unique(hits$site_unique), total.points = unlist(lapply(unique(hits$site_unique), total.points.fun))) #site/visit and associated number of unique PI hits taken, by applying the above function

	growthForm_matrix_PIweights <- simba::mama(count(form_rows)[,-3]) #values are the raw number of hits a particular growth form got in PI for each plot/visit
	
	#However, to convert that to a percent cover, it needs to be divided by the actual number of unique PI hits for that particular plot and multiplied by 100 (not all plots have exactly 1010 unique hits)
	
	for(i in row.names(growthForm_matrix_PIweights)) {
		growthForm_matrix_PIweights[i,] <- growthForm_matrix_PIweights[i,]/total.points$total.points[which(total.points$site_unique == i)]*100
		} #where total.points is the previous calculation of the actual total number of PI hits for each plot - just a df with the site_uniuque field and a value for hits which may be 1010 or otherwise. NOTE the above clunky process was needed because each row needed to be divided by a different number.
		
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


return(growth_form_matrix)
	
} #end function

