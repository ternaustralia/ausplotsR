ausplots_trajectory <- function(my.ausplots.object, choices=c("PCoA", "diversity"), min.revisits=3, plot_select=NULL) {
  
#work in progress - not intended for public use as of 3/2023


  #################################################
  #check data specified
  if(missing(my.ausplots.object)) {
    stop("my.ausplots.object must be specified, see get_ausplots")
  }
  ###############################################
  
  
#library(ecotraj)
#library(vegan)

#remove sites with less than min.revisits
my.ausplots.object$site.info <- subset(my.ausplots.object$site.info, site_location_name %in% subset(my.ausplots.object$site.info, visit_number>=min.revisits)$site_location_name)

#occurrence atrix
diss <- species_table(my.ausplots.object$veg.PI, m_kind="percent_cover", cover_type="PFC", species_name="SN")


#remove sites not in revisited sites table
diss <- diss[which(rownames(diss) %in% my.ausplots.object$site.info$site_unique),]
diss <- diss[,colSums(diss) > 0]

#remove sites not in veg data
my.ausplots.object$site.info <- my.ausplots.object$site.info[which(my.ausplots.object$site.info$site_unique %in% rownames(diss)),]


if("PCoA" %in% choices) {
  
  #dissimilarity
  dissdiss <- vegan::vegdist(diss)
  
if(is.null(plot_select)) {
  ecotraj::trajectoryPCoA(dissdiss, sites=my.ausplots.object$site.info$site_location_name, 
  surveys=my.ausplots.object$site.info$visit_number, selection=NULL, axes = c(1, 2), 
  survey.labels=TRUE, lwd=1.5, lty=1, angle=15, length=0.2, 
  traj.colors=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))))
  
  legend("topleft", legend=my.ausplots.object$site.info$site_location_name, lty=1, lwd=2, col=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))), cex=0.5, bty="n")
} #end not select

#OR - selected sites,
if(!is.null(plot_select)) {
  ecotraj::trajectoryPCoA(dissdiss, sites=my.ausplots.object$site.info$site_location_name, 
  surveys=my.ausplots.object$site.info$visit_number, selection=my.ausplots.object$site.info$site_location_name %in% plot_select, axes = c(1, 2), 
  survey.labels=TRUE, lwd=1.5, lty=1, angle=15, length=0.2, 
  traj.colors=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))))
} #end plot select
  

} #end if PCoA

if("diversity" %in% choices) {
  
  DIV <- data.frame(shannon=vegan::diversity(diss), richness=vegan::diversity(diss, index="simpson"))
 
  if(is.null(plot_select)) {
    ecotraj::trajectoryPlot(DIV, sites=my.ausplots.object$site.info$site_location_name, 
                            surveys=my.ausplots.object$site.info$visit_number, selection=NULL, axes = c(1, 2), 
                            survey.labels=TRUE, lwd=1.5, lty=1, angle=15, length=0.2, 
                            traj.colors=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))))
  } #end not select
  
  #OR - selected sites,
  if(!is.null(plot_select)) {
    ecotraj::trajectoryPlot(DIV, sites=my.ausplots.object$site.info$site_location_name, 
                            surveys=my.ausplots.object$site.info$visit_number, selection=plot_select, axes = c(1, 2), 
                            survey.labels=TRUE, lwd=1.5, lty=1, angle=15, length=0.2, 
                            traj.colors=rainbow(length(unique(my.ausplots.object$site.info$site_location_name))))
  } #end plot select
  

} #end if diversity

} #end function