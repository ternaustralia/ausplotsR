single_cover_value <- function(veg.PI, in_canopy_sky=FALSE, by.growth_form=TRUE, min.height=5, my.growth_forms=c("Tree/Palm", "Tree Mallee")) {

#
#input checks
if(!class(veg.PI) == "data.frame") {stop("veg.PI must be a data.frame")}
if(any(!c("growth_form", "site_unique", "height") %in% names(veg.PI))) {stop("Can't match names of veg.PI; data frame should be returned from get_ausplots")}
if(!is.logical(in_canopy_sky)) {stop("in_canopy_sky must be logical (TRUE/FALSE)")}
if(!is.logical(by.growth_form)) {stop("by.growth_form must be logical (TRUE/FALSE)")}
if(!is.numeric(min.height)) {stop("min.height must be numeric")}
if(!is.character(my.growth_forms)) {stop("my.growth_forms must be a character vector")}
if(by.growth_form) {if(!any(my.growth_forms %in% unique(veg.PI$growth_form))) {stop("One or more growth forms supplied in my.growth_forms is not present in the veg.PI dataset")}}

###################
	
hits <- veg.PI #to match the raw input to historical label in below

#######
	
	#calculate total of all hits by plot (denominator)
	total.points.fun <- function(x) {return(length(unique(hits[which(hits$site_unique == x),]$hits_unique)))} #function that will be run through a list of plot names (x) and count how many unique hits there were (for a standard plot, this will equal 1010)
	
	total.points <- data.frame(site_unique = unique(hits$site_unique), total.points = unlist(lapply(unique(hits$site_unique), total.points.fun))) #site/visit and associated number of unique PI hits taken, by applying the above function
	
###########
	
	#subset by growth form, height, remove dead, and find unique hits to find cover::
	if(by.growth_form == TRUE) { #subset to requested growth forms
		hits <- subset(hits, growth_form %in% my.growth_forms)
		} #close if by growth form TRUE
	
	if(min.height > 0) {hits <- subset(hits, height >= min.height)} #subset to hits with height at least that set in call - substrate only has NA height (zero!) so should already be removed.
	
	if(min.height == 0) {hits <- subset(hits, !is.na(growth_form))} #user wishes to get all cover regardless of height, so in this case we need to make sure that a small number of records are included where there was a growth form on the hit but height was recorded as NA. To remove substrate only hits, we remove NA growth forms.
	
	if(in_canopy_sky==FALSE) { #remove in canopy sky if needed - removed by default
		hits <- subset(hits, in_canopy_sky == FALSE)
	} #close in_canopy_sky FALSE
	
	hits <- subset(hits, dead == FALSE) #remove rows that are scored as dead
	
	hits <- hits[-which(duplicated(hits[,c("site_unique", "hits_unique")])),] #remove duplicate hits by unique transect point ID - hits_unique (combo of transect and point no.) combined with site ID
	
########

#sum remaining/included hits by site
cover.by.site <- plyr::count(hits, vars=c("site_unique")) #

cover.table <- merge(total.points, cover.by.site, by="site_unique", all.x=TRUE) #'total.points' is a table of the actual number of point intercepts taken in each plot which is usually 1010 but can be anything. We now have a row per site_unique and columns for total hits and cover

cover.table$percentCover <- cover.table$freq/cover.table$total.points*100 # calculate as a percent of the number of PIs

cover.table$percentCover[is.na(cover.table$percentCover)] <- 0 #in case there were no relevant hits

######

#double-check that the cover is not over 100% for any plot
check_percent <- plyr::count(cover.table, vars="site_unique", wt_var="percentCover")
if(any(round(check_percent$freq, digits=0) > 100)) {warning("Cover for one or more sites is greater than 100%, check output?")}

###################

#return the output:
cover_output <- cover.table[,c("site_unique", "percentCover")]
cover_output$percentCover <- round(cover_output$percentCover, digits=2)

return(cover_output)

} #end function
