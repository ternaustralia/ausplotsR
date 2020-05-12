ausplots_visual <- function(my.ausplots.object = NULL, map = TRUE, map.attribute = TRUE, fraction.pie = TRUE, growthform.pie = TRUE, cumulative.cover = TRUE, whittaker = TRUE, outfile = NULL) {
	
	#check input formats
	
	
	
	
	#check data
	if(missing(my.ausplots.object)) {
		cat("Obtaining sample data... \n")
		my.ausplots.object <- get_ausplots(bounding_box=c(134, 140, -25, -20), veg.vouchers=FALSE)
	}

	#start plot
	if(missing(outfile)) {
		outfile = paste("ausplots_demo_plots", format(Sys.time(), "%b_%d_%Y"), ".pdf")
		}
	pdf(outfile, width=9, height=6)
	
	cat("Generating plots... \n")
	
	#call individual graphical functions that are switched on
	if(map) {map_ausplots(my.ausplots.object)} #
	if(map.attribute) {map_attribute(my.ausplots.object)}
	if(fraction.pie) {fraction_pie(my.ausplots.object)}
	if(growthform.pie) {growthform_pie(my.ausplots.object)}
	if(cumulative.cover) {cumulative_cover(my.ausplots.object)}
	if(whittaker) {whitt.plot(my.ausplots.object)}
	
	#end plot
	dev.off()
	cat("Plots written to file. \n")

} #end function