ausplots_trajectory <- function(my.ausplots.object, choices=c("PCoA", "diversity"), min.revisits=3) {
  
#work in progress - not intended for public use as of 3/2023

#library(ecotraj)
#library(vegan)

#remove sites with less than min.revisits
my.ausplots.object$site.info <- subset(my.ausplots.object$site.info, site_location_name %in% subset(my.ausplots.object$site.info, visit_number>=3)$site_location_name)

#occurrence atrix
diss <- species_table(my.ausplots.object$veg.PI, m_kind="percent_cover", cover_type="PFC", species_name="HD", strip_bryophytes=TRUE)

#remove sites not in revisited sites table
diss <- diss[which(rownames(diss) %in% my.ausplots.object$site.info$site_unique),]

#remove sites not in veg data
my.ausplots.object$site.info <- my.ausplots.object$site.info[which(my.ausplots.object$site.info$site_unique %in% rownames(diss)),]


#dissimilarity
dissdiss <- vegdist(diss)

ecotraj::trajectoryPCoA(dissdiss, sites=meep$site.info$site_location_name, 
surveys=meep$site.info$visit_number, selection=NULL, axes = c(1, 2), 
survey.labels=TRUE, lwd=1.5, lty=1, angle=15, length=0.2, 
traj.colors=rainbow(length(unique(meep$site.info$site_location_name))))

#OR - selected sites, e.g.


ecotraj::trajectoryPCoA(dissdiss, sites=meep$site.info$site_location_name, 
               surveys=meep$site.info$visit_number, selection=c("SAAFLB0030", "SAAFLB0003", "SAAFLB0031"), axes = c(1, 2), 
               survey.labels=TRUE, lwd=1.5, lty=1, angle=15, length=0.2, 
               traj.colors=rainbow(length(unique(meep$site.info$site_location_name))))



} #end function