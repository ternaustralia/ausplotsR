#29 August 2016

#The R code below reproduces plots from Guerin et al.: 'Opportunities for integrated ecological analysis across inland Australia with standardised data from Ausplots Rangelands'.

#The three associated static datasets (.csv text files) must be saved to the R working directory (or change the below file paths accordingly).

#The script requires some packages to be installed first. 
###################
#load packages
library(ggplot2)
library(gtools)
library(plyr) 
library(simba)
library(vegan)
library(sads)
library(plotrix)
library(MASS)

##################
#load data files

#sites and stats
AusPlots_enviro_stats <- read.csv(file="AusPlots_sites_enviro_stats_table.csv")

#point intercept hits 
hits <- read.csv(file="AusPlots_point_intercept_hits_table.csv")

#percent cover for species against sites data
cover_matrix <- read.csv("AusPlots_percent_cover_matrix.csv", row.names=1)

######################
#All plots plots:

#map of site locations
dev.new()
mapWorld <- borders("world", colour="gray50",fill="gray50") 
ggplot() + 
  mapWorld +
  coord_equal(xlim = c(110, 155),ylim = c(-45, -10))+
  geom_point(data = AusPlots_enviro_stats, aes(x=Longitude, y=Latitude), col="red3", size=3)+
  labs(x="Longitude")+
  labs(y="Latitude")

##############
#boxplots of variables
dev.new()
par(mfrow=c(3,2))
par(mar=c(2,4,4,1))
titles <- c("MAP (mm)", expression(paste("Mean temperature (", degree, "C)")), "Precipitation seasonality (CV)", expression(paste("Temperature 'max' (", degree, "C)")), "Species richness (intercepts)", "Species richness (vouchers)", "Total N (%)", "Total P (%)", "Sand (%)", "Soil depth (m)", "Topographic relief (m)", "Water holding capacity (%)")
n <- 0
sub.labels <- c("a)", "b)", "c)", "d)", "e)", "f)", "a)", "b)", "c)", "d)", "e)", "f)")
for(i in c("MAP", "MAT", "Precipitation.Seasonality", "Mean.Maximum.Temperature.Warmest.Month", "Species.richness_intercepts", "Species.richness_vouchers", "PerSoilN", "PerSoilP", "perSand", "Soil.Depth", "Topographic.relief", "perSoil.WHC")) {
  n <- n + 1
  boxplot(AusPlots_enviro_stats[,i], col=sample(colors(), 1), main=titles[n], outline=TRUE, pch=20, cex=1, las=1)
  mtext(sub.labels[n], side=3, adj=0, padj=-1.5, font=2, at=0.4, cex=1.2)
}
#
################################
#abudance profile shape regressions:

#pareto
dev.new()
par(mfrow=c(3,2))
par(mar=c(5,4.5,3,1))
#
plot(0,type='n',axes=FALSE,ann=FALSE) #blank template plot
legend("center",legend=c("Tussock grasslands", "Eucalypt woodlands", "Chenopod shrublands",  "Acacia shrublands", "Acacia woodlands"), lwd=c(3,3,3,3,3), col=c("darkred", "blue", "hotpink", "orange", "green"), cex=1.5, bty='n')
#
plot(Pareto.alpha ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Tussock Grasslands"),], las=1, bty="l", pch=20, cex=1, col="darkred", ylim=c(0,1), xlab="MAP (mm)", ylab=expression(paste("Pareto ", alpha)), cex.axis=1.2, cex.lab=1.4)
ablineclip(rlm(Pareto.alpha ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Tussock Grasslands"),], psi=psi.bisquare), lwd=3, col="darkred", x1=min(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Tussock Grasslands"),]$MAP), x2=max(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Tussock Grasslands"),]$MAP))
mtext("a)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
#

plot(Pareto.alpha ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Eucalypt Woodlands"),], las=1, bty="l", pch=20, cex=1, col="blue", ylim=c(0,1.6), xlab="MAP (mm)", ylab=expression(paste("Pareto ", alpha)), cex.axis=1.2, cex.lab=1.4)
ablineclip(rlm(Pareto.alpha ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Eucalypt Woodlands"),], psi=psi.bisquare), lwd=3, col="blue", x1=min(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Eucalypt Woodlands"),]$MAP)), x2=max(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Eucalypt Woodlands"),]$MAP)))
mtext("b)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
#

plot(Pareto.alpha ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Chenopod Shrublands, Samphire Shrublands and Forblands"),], las=1, bty="l", pch=20, cex=1, col="hotpink", ylim=c(0,1.2), xlab="MAP (mm)", ylab=expression(paste("Pareto ", alpha)), cex.axis=1.2, cex.lab=1.4)
ablineclip(rlm(Pareto.alpha ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Chenopod Shrublands, Samphire Shrublands and Forblands"),], psi=psi.bisquare), lwd=3, col="hotpink", x1=min(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Chenopod Shrublands, Samphire Shrublands and Forblands"),]$MAP)), x2=max(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Chenopod Shrublands, Samphire Shrublands and Forblands"),]$MAP)))
mtext("c)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
#

plot(Pareto.alpha ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Shrublands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia shrublands"),], las=1, bty="l", pch=20, cex=1, col="orange", ylim=c(0,1), xlab="MAP (mm)", ylab=expression(paste("Pareto ", alpha)), cex.axis=1.2, cex.lab=1.4)
ablineclip(rlm(Pareto.alpha ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Shrublands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia shrublands"),], psi=psi.bisquare), lwd=3, col="orange", x1=min(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Shrublands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia shrublands"),]$MAP)), x2=max(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Shrublands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia shrublands"),]$MAP)))
mtext("d)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
#

plot(Pareto.alpha ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Forests and Woodlands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia Open Woodlands"),], las=1, bty="l", pch=20, cex=1, col="green", ylim=c(0,1.2), xlab="MAP (mm)", ylab=expression(paste("Pareto ", alpha)), cex.axis=1.2, cex.lab=1.4)
ablineclip(rlm(Pareto.alpha ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Forests and Woodlands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia Open Woodlands"),], psi=psi.bisquare), lwd=3, col="green", x1=min(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Forests and Woodlands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia Open Woodlands"),]$MAP), x2=max(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Forests and Woodlands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia Open Woodlands"),]$MAP))
mtext("e)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
#
################
#lognormal
dev.new()
par(mfrow=c(3,2))
par(mar=c(5,4.5,3,1))
#
plot(0,type='n',axes=FALSE,ann=FALSE) #blank template plot
legend("center",legend=c("Tussock grasslands", "Eucalypt woodlands", "Chenopod shrublands",  "Acacia shrublands", "Acacia woodlands"), lwd=c(3,3,3,3,3), col=c("darkred", "blue", "hotpink", "orange", "green"), cex=1.5, bty='n')
#
plot(Lognorm.sigma ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Tussock Grasslands"),], las=1, bty="l", pch=20, cex=1, col="darkred",  xlab="MAP (mm)", ylab=expression(paste("Lognorm.sigmaal ", sigma)), cex.axis=1.2, cex.lab=1.4)
ablineclip(rlm(Lognorm.sigma ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Tussock Grasslands"),], psi=psi.bisquare), lwd=3, col="darkred", x1=min(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Tussock Grasslands"),]$MAP), x2=max(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Tussock Grasslands"),]$MAP))
mtext("a)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
#


plot(Lognorm.sigma ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Eucalypt Woodlands"),], las=1, bty="l", pch=20, cex=1, col="blue", xlab="MAP (mm)", ylab=expression(paste("Lognorm.sigmaal ", sigma)), cex.axis=1.2, cex.lab=1.4)
ablineclip(rlm(Lognorm.sigma ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Eucalypt Woodlands"),], psi=psi.bisquare), lwd=3, col="blue", x1=min(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Eucalypt Woodlands"),]$MAP)), x2=max(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Eucalypt Woodlands"),]$MAP)))
mtext("b)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
#

plot(Lognorm.sigma ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Chenopod Shrublands, Samphire Shrublands and Forblands"),], las=1, bty="l", pch=20, cex=1, col="hotpink", xlab="MAP (mm)", ylab=expression(paste("Lognorm.sigmaal ", sigma)), cex.axis=1.2, cex.lab=1.4)
ablineclip(rlm(Lognorm.sigma ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Chenopod Shrublands, Samphire Shrublands and Forblands"),], psi=psi.bisquare), lwd=3, col="hotpink", x1=min(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Chenopod Shrublands, Samphire Shrublands and Forblands"),]$MAP)), x2=max(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Chenopod Shrublands, Samphire Shrublands and Forblands"),]$MAP)))
mtext("c)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
#

plot(Lognorm.sigma ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Shrublands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia shrublands"),], las=1, bty="l", pch=20, cex=1, col="orange", xlab="MAP (mm)", ylab=expression(paste("Lognorm.sigmaal ", sigma)), cex.axis=1.2, cex.lab=1.4)
ablineclip(rlm(Lognorm.sigma ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Shrublands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia shrublands"),], psi=psi.bisquare), lwd=3, col="orange", x1=min(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Shrublands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia shrublands"),]$MAP)), x2=max(na.omit(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Shrublands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia shrublands"),]$MAP)))
mtext("d)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
#

plot(Lognorm.sigma ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Forests and Woodlands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia Open Woodlands"),], las=1, bty="l", pch=20, cex=1, col="green", xlab="MAP (mm)", ylab=expression(paste("Lognorm.sigmaal ", sigma)), cex.axis=1.2, cex.lab=1.4)
ablineclip(rlm(Lognorm.sigma ~ MAP, data=AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Forests and Woodlands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia Open Woodlands"),], psi=psi.bisquare), lwd=3, col="green", x1=min(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Forests and Woodlands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia Open Woodlands"),]$MAP), x2=max(AusPlots_enviro_stats[which(AusPlots_enviro_stats$Major.Vegetation.Group == "Acacia Forests and Woodlands" | AusPlots_enviro_stats$Major.Vegetation.Group =="Acacia Open Woodlands"),]$MAP))
mtext("e)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
#

#######################
#Selected plots plots:

#cumulative cover
select.plots.plots <- function(plot.list) { #where 'plot.list' is a character vector of plot-visit names, see below...
	transect.order <- c("N1-S1", "S1-N1", "S2-N2", "N2-S2", "N3-S3", "S3-N3", "S4-N4", "N4-S4", "N5-S5", "S5-N5", "W1-E1", "E1-W1", "E2-W2", "W2-E2", "W3-E3", "E3-W3", "E4-W4", "W4-E4", "W5-E5", "E5-W5")
	for(i in plot.list) {
		dev.new()
		par(mfrow=c(2,2))
		par(mar=c(5,5,2,1))
		print(i)
		plot.hits.temp <- subset(hits, site_unique==i, select=c("herbarium_determination", "hits_unique", "in_canopy_sky"))
		plot.hits.temp <- plot.hits.temp[mixedorder(plot.hits.temp$hits_unique),]
		trans.ord.vec <- plot.hits.temp$hits_unique[order(match(gsub(" .*$", "", plot.hits.temp$hits_unique), transect.order))]
		plot.hits.temp <- plot.hits.temp[match(trans.ord.vec, plot.hits.temp$hits_unique),]
		uniq.hits.plot.i <- length(unique(plot.hits.temp$hits_unique))
		x.ca <- list()
		if(length(na.omit(unique(plot.hits.temp$herbarium_determination))) > 0) {
			spp.num <- 0
			for(x in na.omit(unique(plot.hits.temp$herbarium_determination))) {
				print(x)
				spp.num <- spp.num + 1
				x.ca[[spp.num]] <- rep(NA, floor(uniq.hits.plot.i/10)) 
				n <- 0
				for(k in 10*c(1:(uniq.hits.plot.i/10))) {
					n <- n+1
					CA.temp <- subset(plot.hits.temp, hits_unique %in% unique(plot.hits.temp$hits_unique)[1:k])
					CA.temp.x <- subset(CA.temp, herbarium_determination==x)
					CA.temp.x <- CA.temp.x[which(as.character(CA.temp.x$in_canopy_sky) == "FALSE"),]
					temp.cover.score <- as.numeric((count(CA.temp.x, vars="herbarium_determination")$freq)/k*100)
					if(length(temp.cover.score) != 0) {x.ca[[spp.num]][n] <-temp.cover.score}
				} #close for k
				print(temp.cover.score)
			}#close for x (loop over species in a plot)
			max.cover.plot.i <- max(na.omit(unlist(x.ca)))
			plot(1, ylim=c(0, max.cover.plot.i), xlim=c(0, 1100), type="n", xlab="Number of point intercepts", ylab="Estimated %CA", main=i, las=1, bty="l", cex.main=0.8)
			mtext("a)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
			spp.col <- sample(colors(), length(x.ca))
			zzz <- 0
			for(j in x.ca) {
				zzz <- zzz + 1
				points(10*c(1:(uniq.hits.plot.i/10))[1:length(j)], j, type="l", col=spp.col[zzz])
			} #close for j in x.ca
			plot(0,type='n',axes=FALSE,ann=FALSE)
			legend.data <- data.frame(cover = sapply(x.ca, FUN = function(x) {tail(x, n=1)}), species = na.omit(unique(plot.hits.temp$herbarium_determination)), colour = spp.col)
			legend.data <- na.omit(legend.data[order(legend.data[,"cover"], decreasing=TRUE),][1:5,])
			legend("topleft", legend=legend.data$species, lty=rep(1, length(legend.data$species)), col=as.character(legend.data$colour), cex=1, bty='n', lwd=rep(3, length(legend.data$species)))
		}#close if length... > 0
		
		##################
		#species accumulation curves
		temp <- hits[which(hits$site_unique == i),]
		temp <- data.frame(hit=temp$hits_unique, herbarium_determination=temp$herbarium_determination, presence=rep(1, nrow(temp))) 
		temp2 <- try(mama(temp))
		temp2 <- temp2[,-grep("NA.", colnames(temp2))]
		Single_plot_accum_eg <- try(specaccum(temp2, method="random", permutations=1000))
		if(class(temp2) != "try-error" && class(Single_plot_accum_eg) != "try-error") {
			plot(Single_plot_accum_eg, col="black", main=i, cex.main=0.8, xlab="Number of point intercepts", ylab="Cumulative species", ci.col="orange3", bty="l", las=1, xlim=c(0, nrow(temp2)))
			mtext("b)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
			print(i)
		}
		
		##################
		#abudance profiles
		quantiles <- as.numeric(cover_matrix[i, which(as.numeric(cover_matrix[i,])>0)])
		pareto <- try(coef(fitsad(quantiles, sad="pareto"))[1])
		lnormal <- try(coef(fitsad(quantiles, sad="lnorm"))[1])
		try(plot(rad(quantiles), pch=20, cex=1, bty="l", las=1, main=i, cex.main=0.8))
		try(lines(radpred(fitsad(quantiles, sad="pareto")), col="darkred", lwd=2))
		try(lines(radpred(fitsad(quantiles, sad="lnorm")), col="blue", lwd=2))
		mtext("c)", side=3,  padj=-1, font=2, adj=0, cex=1.2)
		legend("topright", legend=c("Pareto", "Log normal"), lwd=c(3,3), col=c("darkred", "blue"), bty='n')
	}# close loop
} #close select.plots.plots function


#example of running function for two plots:
select.plots.plots(c("NSABHC0006-53601", "WAAPIL0010-57607")) #run: rownames(cover_matrix) to get list of available plots.