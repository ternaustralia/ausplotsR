ausplots_visual <- function(my.ausplots.object = NULL, map = TRUE, map.attribute = TRUE, fraction.pie = TRUE, growthform.pie = TRUE, cumulative_cover = TRUE, whittaker = TRUE, outfile=NULL) {
	
	#check input formats
	
	
	
	
	#check data
	if(!missing(my.ausplots.object)) {
		my.ausplots.object <- get_ausplots(bounding_box=c(130, 140, -25, -15), veg.vouchers=FALSE)
	}

	#start plot
	if(missing(outfile)) {
		outfile = paste0(deparse(substitute(my.ausplots.object)), ".pdf")
		}
	pdf(outfile)
	
	#call individual graphical functions that are switched on
	if(map) {} #
	if(map.attribute) {}
	if(fraction.pie) {}
	if(growthform.pie) {}
	if(cumulative_cover) {}
	if(whittaker) {}
	
	
	#end plot
	dev.off()

} #end function