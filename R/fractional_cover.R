fractional_cover <- function(veg.PI, ground_fractional="FALSE", in_canopy_sky="FALSE") {
	
	hits <- veg.PI #to match the raw input to historical label in below
	
	
	
	if(ground_fractional == "FALSE") {
		

	
	#class individual point intercept data rows as 'green', 'brown' or 'bare':
	n <- 0
	fraction <- rep(NA, nrow(hits))
	for(i in 1:nrow(hits)) { #a
		n <- n + 1
		temp <- hits[n,]
		if(is.na(temp$growth_form) | temp$in_canopy_sky && in_canopy_sky == "FALSE") { #b
			if(temp$substrate %in% c("Litter", "CWD", "Crypto")) { #c
				fraction[n] <- "brown"
			} #/c
			if(temp$substrate %in% c("Bare", "Outcrop", "Rock", "Gravel")) { #d
			fraction[n] <- "bare"
			} #/d
		} else if(temp$dead) { #/b; #f 
			fraction[n] <- "brown"
			} else #/f
			if(!temp$dead) { #g
				fraction[n] <- "green"
		} #/g

	
	}#/a
	

#there may be multiple hits at a given intercept, and substrate (brown/bare) may overlap with these for in canopy sky hits, so below remove duplicates by applying a height rule in which highest hit is selected, as this is relevant for satellite image analysis, e.g. tree > shrub > grass > substrate (but calculated by the height in m)

#processing steps

total.points.fun <- function(x) {return(length(unique(hits[which(hits$site_unique == x),]$hits_unique)))} #function to go through a list of plot names and count how many unique hits there were (for a standard plot, this will equal 1010)

total.points <- data.frame(site_unique = unique(hits$site_unique), total.points = unlist(lapply(unique(hits$site_unique), total.points.fun))) #site/visit and associated number of unique PI hits taken, by applying the above function



fractional_df <- data.frame(site_unique=hits$site_unique, hits_unique = hits$hits_unique, fractional=fraction, height = hits$height) #table of individual PI hits and what fraction they have been assigned to 

fractional_df$height[is.na(fractional_df$height)] <- 0 #assign all NA heights to zero as these should be substrate hits (for sorting)

fractional_df <- fractional_df[with(fractional_df, order(as.character(site_unique), as.character(hits_unique), height)), ] #order by site, hit and then height so that we can remove duplicates from unique intercepts by the height rule with the highest row coming last

fractional_df <- fractional_df[-which(duplicated(fractional_df[,1:2], fromLast=TRUE)),] #this removes rows if they are duplicates of the same PI and they are not the highest sampled point/hit, by leaving the last entry (tallest) if there are intercept duplicates

##calculate fractional cover:
fractional_df_sites <- plyr::count(fractional_df, vars=c("site_unique", "fractional")) #count number of occurrences of each fractional cover category by plot


###THERE ARE NAS in THE 'FRACTIONAL' column!!!!!! NSABHC0002-53597  has a lot of NC substrate only hits - check how this is handled

fractional_df_sites <- merge(fractional_df_sites, total.points, by="site_unique") #'total.points' is a table of the actual number of point intercepts taken in each plot which is usually 1010 but can be anything

fractional_df_sites$fractionalPercent <- fractional_df_sites$freq/fractional_df_sites$total.points*100 # calculate as a percent of the number of PIs
######

#double-check that the fractional covers add up to 100% for each plot
check_percent <- plyr::count(fractional_df_sites, vars="site_unique", wt_var="fractionalPercent")
if(any(round(check_percent$freq, digits=0) != 100)) {warning("Fractional cover for one or more sites does not sum to 100, check output")}

###################

#write the output to file:
fractional_cover_output <- fractional_df_sites[,c(1,2,5)]
names(fractional_cover_output) <- c("Plot", "Fraction", "Percent") #these names are not returned in the output - just formatting for matrix conversion
fractional_cover_output$Percent <- round(fractional_cover_output$Percent, digits=2)

##create a matrix version to condense the data, columns are fractions, rows are plots:
#change NAs for fraction to 'other'
fractional_cover_output$Fraction[is.na(fractional_cover_output$Fraction)] <- "other"
fractional_cover_output.matrix <- ma_ausplot_ma(fractional_cover_output)
fractional_cover_output.matrix$site_unique <- row.names(fractional_cover_output.matrix)

COL <- ncol(fractional_cover_output.matrix)

fractional_cover_output.matrix <- fractional_cover_output.matrix[,c(COL, 1:COL-1)]

fractional_cover_output.matrix[,c(2:5)] <- round(fractional_cover_output.matrix[,c(2:5)], digits=1)

return(fractional_cover_output.matrix)

	} #close ground fractional false

######################################

#Calculate fractional GROUND cover

if(ground_fractional == "TRUE") {
	
	n <- 0
	ground.fraction <- rep(NA, nrow(hits))
	for(i in 1:nrow(hits)) { #a
		n <- n + 1
		temp <- hits[n,]
		if(is.na(temp$growth_form) | !temp$growth_form %in% c("Fern", "Forb", "Hummock grass", "Rush", "Sedge", "Tussock grass")) { #b
			if(temp$substrate %in% c("Litter", "CWD", "Crypto")) { #c
				ground.fraction[n] <- "brown"
			} #/c
			if(temp$substrate %in% c("Bare", "Outcrop", "Rock", "Gravel")) { #d
			ground.fraction[n] <- "bare"
			} #/d
		} else if(temp$dead) { #/b; #f 
				ground.fraction[n] <- "brown"
				} else #/f
			if(!temp$dead) { #g
				ground.fraction[n] <- "green"
			} #/g

	}#/a
	

#processing steps


total.points.fun <- function(x) {return(length(unique(hits[which(hits$site_unique == x),]$hits_unique)))} #function to go through a list of plot names and count how many unique hits there were (for a standard plot, this will equal 1010)

total.points <- data.frame(site_unique = unique(hits$site_unique), total.points = unlist(lapply(unique(hits$site_unique), total.points.fun))) #site/visit and associated number of unique PI hits taken, by applying the above function


ground.fractional_df <- data.frame(site_unique=hits$site_unique, hits_unique = hits$hits_unique, fractional=ground.fraction, height = hits$height) #table of individual PI hits and what fraction they have been assigned to 

ground.fractional_df$height[is.na(ground.fractional_df$height)] <- 0 #assign all NA heights to zero as these should be substrate hits

ground.fractional_df <- ground.fractional_df[with(ground.fractional_df, order(as.character(site_unique), as.character(hits_unique), height)), ] #order by site, hit and then height so that we can remove duplicates from unique hits by the height rule with the highest row coming last

ground.fractional_df <- ground.fractional_df[-which(duplicated(ground.fractional_df[,1:2], fromLast=TRUE)),] #this removes rows if they are duplicates of the same PI and they are not the highest sampled point/hit

##calculate fractional cover:
ground.fractional_df_sites <- plyr::count(ground.fractional_df, vars=c("site_unique", "fractional")) #count number of occurrences of each fractional cover category by plot

ground.fractional_df_sites <- merge(ground.fractional_df_sites, total.points, by="site_unique") #total.points is the table of the actual number of point intercepts taken in each plot which is usually 1010 but can be anything

ground.fractional_df_sites$fractionalPercent <- ground.fractional_df_sites$freq/ground.fractional_df_sites$total.points*100 # calculate as a percent of the number of PIs
######

#double-check that the fractional covers add up to 100% for each plot
check_percent <- plyr::count(ground.fractional_df_sites, vars="site_unique", wt_var="fractionalPercent")
if(any(round(check_percent$freq, digits=0) != 100)) {warning("Fractional cover for one or more sites does not sum to 100, check output")}


ground.fractional.output <- ground.fractional_df_sites[,c(1,2,5)]

names(ground.fractional.output) <- c("Plot", "Fraction", "Percent") #these names are not returned in the output - just formatting for matrix conversion

ground.fractional.output$Percent <- round(ground.fractional.output$Percent, digits=2)

#generate a matrix to condense the data - columns are fractions and rows are plots
#change NAs for fraction to 'other'
ground.fractional.output$Fraction[is.na(ground.fractional.output$Fraction)] <- "other"
ground.fractional.output.matrix <- ma_ausplot_ma(ground.fractional.output)

ground.fractional.output.matrix$site_unique <- row.names(ground.fractional.output.matrix)

ground.fractional.output.matrix <- ground.fractional.output.matrix[,c(5,1,2,3,4)]

ground.fractional.output.matrix[,c(2:5)] <- round(ground.fractional.output.matrix[,c(2:5)], digits=1)

return(ground.fractional.output.matrix)



} # end ground fractional TRUE



} #end function
