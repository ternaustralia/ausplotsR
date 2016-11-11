#11/2016

#Fractional cover for AusPlots

#This scripts take the previously compiled point intercept data (object 'hits' from the script compile_ausplots_vegPI.R) and calculates fractional cover: i.e. the percent cover of green vegetation, dead vegetation and bare substrate.

#Note:: relies on previous compilation scripts having been run for input data

#Cover fractions are assigned according to the following:
#'Green' or 'photosynthetic vegetation' is living vascular plant cover
#'Brown' or 'non-photosynthetic vegetation' is either vascular plant cover scored as 'dead' or substrate scored as litter, coarse woody debris or cryptogam (see below) that has no veg cover
#'Bare' or 'bare ground' is substrate that is rock, outcrop, gravel or bare soil with no veg cover
#A height rule is applied so that coding to green/brown/bare of the uppermost substrate/vegetation stratum hit at a given point intercept location overrides the others, that is, a dead tree overrides a living shrub beneath; substrate coding is overriden by any vegetation cover etc. This means for each of the (usually) 1010 intercepts, there is a single coding and percentage is the number of hits assigned to each fraction, divided by the total number of PIs taken (usually 1010 but can vary) times 100.
#'In canopy sky' is excluded (only the substrate is considered for those hits)
#Currently, cryptogam substrate is assigned to the non-photosynthetic fraction
#Occasionally substrate type was not collected ('NC') or could not be assigned to one of the above categories ('Unknwn'), in which case a percent cover will be returned under an 'NA' fraction if there was no veg cover above

#Author: Greg R. guerin

library(plyr) #for the 'count' function

#the following loop takes individual point intercept data rows and classes them as green, brown or bare for subsequent sorting and fractional cover calculation over the plot
n <- 0
fraction <- rep(NA, nrow(hits))
for(i in 1:nrow(hits)) { #a
	n <- n + 1
	temp <- hits[n,]
	if(is.na(temp$growth_form) | temp$in_canopy_sky) { #b
		if(temp$substrate == "Litter" | temp$substrate == "CWD" | temp$substrate == "Crypto") { #c
			fraction[n] <- "brown"
		} #/c
		if(temp$substrate == "Bare" | temp$substrate == "Outcrop" | temp$substrate == "Rock" | temp$substrate == "Gravel") { #d
			fraction[n] <- "bare"
		} #/d
	} else if(temp$dead) { #/b; #f 
			fraction[n] <- "brown"
			} else #/f
		if(!temp$dead) { #f
			fraction[n] <- "green"
		} #/g

	print(n)
	print(fraction[n])
	}#/a
	

#at this point there may be multiple hits at same intercept location and also substrate (brown/bare) may overlap with these for in canopy sky hits, so below we remove duplicates by applying a height rule in which highest hit is selected, as this is relevant for satellite image analysis, e.g. tree > shrub > grass > substrate (but calculated by the height in m)

#processing steps
fractional_df <- data.frame(site_unique=hits$site_unique, hits_unique = hits$hits_unique, fractional=fraction, height = hits$height) #table of individual PI hits and what fraction they have been assigned to 

fractional_df$height[is.na(fractional_df$height)] <- 0 #assign all NA heights to zero as these should be substrate hits (for sorting)

fractional_df <- fractional_df[with(fractional_df, order(as.character(site_unique), as.character(hits_unique), height)), ] #order by site, hit and then height so that we can remove duplicates from unique hits by the height rule with the highest row coming last
fractional_df <- fractional_df[-which(duplicated(fractional_df[,1:2], fromLast=TRUE)),] #this removes rows if they are duplicates of the same PI and they are not the highest sampled point/hit

##calculate fractional cover:
fractional_df_sites <- count(fractional_df, vars=c("site_unique", "fractional")) #count number of occurrences of each fractional cover category by plot
fractional_df_sites <- merge(fractional_df_sites, total.points, by="site_unique") #from previous script, object 'total.points' is a table of the actual number of point intercepts taken in each plot which is usually 1010 but can be anything
fractional_df_sites$fractionalPercent <- fractional_df_sites$freq/fractional_df_sites$total.points*100 # calculate as a percent of the number of PIs
######
#the following line can be used to double-check that the fractional covers add up to 100% for each plot
#count(fractional_df_sites, vars="site_unique", wt_var="fractionalPercent") #these make 100

###################

#write the output to file:
output <- fractional_df_sites[,c(1,2,5)]
names(output) <- c("Plot", "Fraction", "Percent")
output$Percent <- round(output$Percent, digits=2)
write.csv(output, file="AusPlotsFractionalCover.txt", row.names=FALSE)

#also, output a matrix version:
library(simba)
output.matrix <- mama(output)
output.matrix$Plot <- row.names(output.matrix)
output.matrix <- output.matrix[,c(5,1,2,3,4)]
write.csv(output.matrix, file="AusPlotsFractionalCoverMatrix.txt", row.names=FALSE)

#############

#Visualise fractional cover for each plot as pie chart similar to those produced for AusPlots data on Soils2Satellites:
pdf("FractionalCoverAusPlots.pdf")
palette(c("brown","orange","darkgreen", "gray"))
par(mfrow=c(2,2), mar=c(2,2,2,2))
for(i in unique(fractional_df_sites$site_unique)) {
	temp <- fractional_df_sites[which(fractional_df_sites$site_unique == i),]
	
	temp$fractional <- factor(temp$fractional,levels=c(levels(temp$fractional), "NA"))
	temp$fractional[is.na(temp$fractional)] <- "NA"
	pie(temp$fractionalPercent, col=temp$fractional, main=i, labels=round(temp$fractionalPercent, digits=1))
	plot(0, type="n", axes=F, xlab="", ylab="")
	legend("left", col=c("brown","orange","darkgreen", "gray"), pch=20, pt.cex=3, legend=c("Bare ground", "Non-photosynthetic vegetation", "Photosynthetic vegetation", "NA"), bty="n")
	} #
dev.off()



######################################
######################################
#Alternatively, calculate fractional GROUND cover
#The process is the same as above except that only ground cover growth forms are considered: i.e. grasses (hummock, tussock, other); sedge; rush; forb; fern;
#?vine - presently excluded as it may or may not form a ground cover
#?cryptogam - presently included with non-photosynthetic fraction, need to verify this.


n <- 0
ground.fraction <- rep(NA, nrow(hits))
for(i in 1:nrow(hits)) { #a
	n <- n + 1
	temp <- hits[n,]
	if(is.na(temp$growth_form) | temp$in_canopy_sky | !temp$growth_form %in% c("Fern", "Forb", "Hummock grass", "Rush", "Sedge", "Tussock grass")) { #b
		if(temp$substrate == "Litter" | temp$substrate == "CWD" | temp$substrate == "Crypto") { #c
			ground.fraction[n] <- "brown"
		} #/c
		if(temp$substrate == "Bare" | temp$substrate == "Outcrop" | temp$substrate == "Rock" | temp$substrate == "Gravel") { #d
			ground.fraction[n] <- "bare"
		} #/d
	} else if(temp$dead) { #/b; #f 
			ground.fraction[n] <- "brown"
			} else #/f
		if(!temp$dead) { #f
			ground.fraction[n] <- "green"
		} #/g

	print(n)
	print(ground.fraction[n])
	}#/a
	

#processing steps
ground.fractional_df <- data.frame(site_unique=hits$site_unique, hits_unique = hits$hits_unique, fractional=ground.fraction, height = hits$height) #table of individual PI hits and what fraction they have been assigned to 

ground.fractional_df$height[is.na(ground.fractional_df$height)] <- 0 #assign all NA heights to zero as these should be substrate hits

ground.fractional_df <- ground.fractional_df[with(ground.fractional_df, order(as.character(site_unique), as.character(hits_unique), height)), ] #order by site, hit and then height so that we can remove duplicates from unique hits by the height rule with the highest row coming last
ground.fractional_df <- ground.fractional_df[-which(duplicated(ground.fractional_df[,1:2], fromLast=TRUE)),] #this removes rows if they are duplicates of the same PI and they are not the highest sampled point/hit

##calculate fractional cover:
ground.fractional_df_sites <- count(ground.fractional_df, vars=c("site_unique", "fractional")) #count number of occurrences of each fractional cover category by plot
ground.fractional_df_sites <- merge(ground.fractional_df_sites, total.points, by="site_unique") #from previous script, total.points is a table of the actual number of point intercepts taken in each plot which is usually 1010 but can be anything
ground.fractional_df_sites$fractionalPercent <- ground.fractional_df_sites$freq/ground.fractional_df_sites$total.points*100 # calculate as a percent of the number of PIs
######
#the following line can be used to double-check that the fractional covers add up to 100% for each plot
#count(fractional_df_sites, vars="site_unique", wt_var="fractionalPercent")

###################

#write the output to file:
ground.output <- ground.fractional_df_sites[,c(1,2,5)]
names(ground.output) <- c("Plot", "Fraction", "Percent")
ground.output$Percent <- round(ground.output$Percent, digits=2)
write.csv(ground.output, file="AusPlotsFractionalGroundCover.txt", row.names=FALSE)

#also, output a matrix version:
library(simba)
ground.output.matrix <- mama(ground.output)
ground.output.matrix$Plot <- row.names(ground.output.matrix)
ground.output.matrix <- ground.output.matrix[,c(5,1,2,3,4)]
write.csv(ground.output.matrix, file="AusPlotsFractionalGroundCoverMatrix.txt", row.names=FALSE)

#############

#Visualise fractional cover as pie chart similar to those produced for AusPlots data on Soils2Satellites
pdf("FractionalGroundCoverAusPlots.pdf")
palette(c("brown","orange","darkgreen", "gray"))
par(mfrow=c(2,2), mar=c(2,2,2,2))
for(i in unique(ground.fractional_df_sites$site_unique)) {
	temp <- ground.fractional_df_sites[which(ground.fractional_df_sites$site_unique == i),]
	
	temp$fractional <- factor(temp$fractional,levels=c(levels(temp$fractional), "NA"))
	temp$fractional[is.na(temp$fractional)] <- "NA"
	pie(temp$fractionalPercent, col=temp$fractional, main=i, labels=round(temp$fractionalPercent, digits=1))
	plot(0, type="n", axes=F, xlab="", ylab="")
	legend("left", col=c("brown","orange","darkgreen", "gray"), pch=20, pt.cex=3, legend=c("Bare ground", "Non-photosynthetic vegetation", "Photosynthetic vegetation", "NA"), bty="n")
	} #
dev.off()

###############



