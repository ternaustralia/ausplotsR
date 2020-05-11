ausplots_visual <- function(my.ausplots.object, map = TRUE, fractional.pie = TRUE, growthform.pie = TRUE, cumulative_cover = TRUE, whittaker = TRUE, outfile=NULL) {
	
	#check input formats



	#start plot
	if(missing(outfile)) {
		outfile = paste0(deparse(substitute(my.ausplots.object)), ".pdf")
		}
	pdf(outfile)
	
	#call individual graphical functions that are switched on
	if(fractional.pie) {}
	if(growthform.pie) {}
	if(cumulative_cover) {}
	if(whittaker) {}
	
	
	#end plot
	dev.off()

} #end function