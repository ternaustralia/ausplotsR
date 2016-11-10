#11/2016

#####in DRAFT only, needs refinement

#This script take the previously compiled point intercept data ('hits') and calculates fractional cover, the percent cover of green vegetation, dead vegetation and bare substrate.

#Note relies on previous script to compile objects and load packages

#Author: Greg R. guerin

#following loop takes intercept data rows and classes them as green brown or bare for fractional cover calculation
n <- 0
fraction <- rep(NA, nrow(hits))
for(i in 1:nrow(hits)) {
	n <- n + 1
	temp <- hits[n,]
	if(is.na(temp$dead)) {
		if(temp$substrate == "Litter" | temp$substrate == "CWD") {
			fraction[n] <- "brown"
		}
		if(temp$substrate != "Litter" & temp$substrate != "CWD") {
			fraction[n] <- "bare"
		}
	} else
	if(!temp$dead) {
		fraction[n] <- "green"
	} else
	
	if(temp$dead) {
		fraction[n] <- "brown"
	}
	
	print(n)
	print(fraction[n])
	}#cls for i
	

#at this point there may be dead and green veg hits at same intercept (bare ground shouldn't overlap because they would have dead as T or F etc), need a rule that green overrides brown for same unique intercept location

fractional_df <- data.frame(site_unique=hits$site_unique, hits_unique = hits$hits_unique, fractional=fraction) #list of individual PI hits with what fractional cover assignment they have been given
fractional_df_NoDups <- fractional_df[-which(duplicated(fractional_df)==TRUE),] #remove duplicate rows, so left with individual rows for unique fractional cover codes per intercept location
fractional_df_NoDups_sites <- count(fractional_df_NoDups, vars=c("site_unique", "fractional")) #count number of occurrences of each fractional cover category by plot
fractional_df_NoDups_sites <- merge(fractional_df_NoDups_sites, total.points, by="site_unique") #from previous script, total.points is a table of the actual number of point intercepts taken in each plot
fractional_df_NoDups_sites$fractionalPercent <- fractional_df_NoDups_sites$freq/fractional_df_NoDups_sites$total.points*100

#start to visualise the data
par(mfrow=c(4,4)); for(i in fractional_df_NoDups_sites$site_unique) {pie(fractional_df_NoDups_sites[which(fractional_df_NoDups_sites$site_unique == i),]$fractionalPercent, col=c("brown", "orange", "green"), main=i)} #note if not all types present the colours might be wrong at present
