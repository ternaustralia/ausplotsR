#Generates a 3d surface map of vegetation structure within a plot

#This function requires the compiled point intercept dataset from 'compile_ausplots_vegPI.R'

##Authors:
#Greg Guerin

##Example for single plot:
#e.g.
#plot_veg_structure("WAACOO0027")

##Example use to run across all plots:
#pdf("AusPlots_3d_plots.pdf");sapply(unique(as.character(hits$site_location_name)), plot_veg_structure); dev.off()


plot_veg_structure <- function(site_name, vertical.exag = 4, return.matrix = FALSE){

require(lattice)
	
current.plot <- hits[which(hits$site_location_name == site_name),]
if(max(current.plot$point_number) > 100) {return("Whacky points!")}
current.plot$x_coord <- NA
current.plot$y_coord <- NA
current.plot$x_coord[grep("N1-S1", current.plot$standard)] <- 10
current.plot$x_coord[grep("N2-S2", current.plot$standard)] <- 30
current.plot$x_coord[grep("N3-S3", current.plot$standard)] <- 50
current.plot$x_coord[grep("N4-S4", current.plot$standard)] <- 70
current.plot$x_coord[grep("N5-S5", current.plot$standard)] <- 90

current.plot$y_coord[grep("E1-W1", current.plot$standard)] <- 10
current.plot$y_coord[grep("E2-W2", current.plot$standard)] <- 30
current.plot$y_coord[grep("E3-W3", current.plot$standard)] <- 50
current.plot$y_coord[grep("E4-W4", current.plot$standard)] <- 70
current.plot$y_coord[grep("E5-W5", current.plot$standard)] <- 90

current.plot$y_coord[grep("N1-S1", current.plot$standard)] <- current.plot$point_number[grep("N1-S1", current.plot$standard)]
current.plot$y_coord[grep("N2-S2", current.plot$standard)] <-  current.plot$point_number[grep("N2-S2", current.plot$standard)]
current.plot$y_coord[grep("N3-S3", current.plot$standard)] <-  current.plot$point_number[grep("N3-S3", current.plot$standard)]
current.plot$y_coord[grep("N4-S4", current.plot$standard)] <-  current.plot$point_number[grep("N4-S4", current.plot$standard)]
current.plot$y_coord[grep("N5-S5", current.plot$standard)] <-  current.plot$point_number[grep("N5-S5", current.plot$standard)]

current.plot$x_coord[grep("E1-W1", current.plot$standard)] <-  current.plot$point_number[grep("E1-W1", current.plot$standard)]
current.plot$x_coord[grep("E2-W2", current.plot$standard)] <-  current.plot$point_number[grep("E2-W2", current.plot$standard)]
current.plot$x_coord[grep("E3-W3", current.plot$standard)] <-  current.plot$point_number[grep("E3-W3", current.plot$standard)]
current.plot$x_coord[grep("E4-W4", current.plot$standard)] <-  current.plot$point_number[grep("E4-W4", current.plot$standard)]
current.plot$x_coord[grep("E5-W5", current.plot$standard)] <-  current.plot$point_number[grep("E5-W5", current.plot$standard)]

#create 101 x 101 (m) matrix with NAs between transects, this represent points in the field plot as a regular grid, including both edges

current.plot$x_coord <- current.plot$x_coord+1
current.plot$y_coord <- current.plot$y_coord+1 #can't have zero values as in the raw data which has point numbers from 0-100

plot_frame <- matrix(nrow=101, ncol=101) #empty matrix representing the plot

n <- 0
for(i in 1:nrow(current.plot)) {
	n <- n + 1
	plot_frame[current.plot[n,"x_coord"], current.plot[n, "y_coord"]] <- current.plot[n,"height"] #assign height value from intercept hit to equivalent coordinate position in matrix representing plot	
}

plot_frame[is.na(plot_frame)] <- 0 #set non-records to a height of zero

max.h <- max(plot_frame) #highest
z.aspect.factor <- vertical.exag*max.h/100


the.plot <- wireframe(plot_frame, pretty=TRUE, drape=TRUE, at=c(0, 0.1*max.h, 0.2*max.h, 0.3*max.h,0.4*max.h, 0.5*max.h, 0.6*max.h, 0.7*max.h, 0.8*max.h, 0.9*max.h, max.h), col.regions=rev(heat.colors(10)), aspect=c(1, z.aspect.factor), xlab="East", ylab="North", zlab="Height", screen = list(z = 20, x = -80, y = -10), zoom=0.8, main=paste("PI hits & veg. structure for:", site_name))

results <- list()

if(return.matrix) {results$plot_frame <- plot_frame; results$the.plot <- the.plot} else {results$the.plot <- the.plot}

return(results)

} #close plot_veg_structure function



