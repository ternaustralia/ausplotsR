ausplots_visual <- function(my.ausplots.object = NULL, map = TRUE, map.attribute = TRUE, fraction.pie = TRUE, growthform.pie = TRUE, cumulative.cover = TRUE, whittaker = TRUE, outfile = NULL) {
	
	#############################
	#check input formats
	if(!missing(my.ausplots.object)) {
		if(any(map.attribute, fraction.pie, growthform.pie, cumulative.cover, whittaker)) {
			if(!("veg.PI" %in% names(my.ausplots.object))) {
				stop("If you provide a my.ausplots.object it must contain $veg.PI data to generate those plots!")
			}
		} #close if veg plots requested
		if(any(map, map.attribute)) {
			if(!("site.info" %in% names(my.ausplots.object))) {
				stop("If you provide a my.ausplots.object it must contain $site.info data to generate maps!")
			}
		} #close if map requested
	} #close if object provided
	
	#################################################
	#check data and extract if missing
	if(missing(my.ausplots.object)) {
		cat("Obtaining sample data... \n")
		my.ausplots.object <- get_ausplots(bounding_box=c(136, 140, -24, -20), veg.vouchers=FALSE)
	}
	
	###############################################
	#start plot
	if(missing(outfile)) {
		outfile = paste("ausplots_demo_plots", format(Sys.time(), "%b_%d_%Y"), ".pdf")
		}
		
	pdf(outfile, width=9, height=6)
	
	cat("Generating plots... \n")
	
	#####
	#call individual graphical functions that are switched on
	if(map) {
		map_ausplots(my.ausplots.object)
		}

	if(map.attribute) {
		map_attribute(my.ausplots.object)
		}
		
		
	par(mfrow=c(2,2))
	n <- 0
	for(i in unique(my.ausplots.object$veg.PI$site_unique)) {
		
		n <- n + 1
		
	if(fraction.pie) {
		if(n == 1) {
			frac <- fractional_cover(my.ausplots.object$veg.PI)
		}
		par(mar=c(0,0,2,0))
		fraction_pie(frac, n)
		}

	if(growthform.pie) {
		if(n == 1) {
			GF <- growth_form_table(my.ausplots.object$veg.PI, m_kind="percent_cover", cover_type=("PFC"))
			GF_col <- rev(terrain.colors(length(names(GF))))
			names(GF_col) <- names(GF)
		}
		par(mar=c(0,0,2,0))
		growthform_pie(GF[n,], GF_col)
		}

	if(cumulative.cover) {
		cumulative_cover(my.ausplots.object$veg.PI)
		}

	if(whittaker) {
		if(n == 1) {
			sppBYsites <- species_table(my.ausplots.object$veg.PI, m_kind="percent_cover", cover_type="PFC")
			}
		par(mar=c(5,5,5,5))
		whitt.plot(sppBYsites[i,])
		}
		
		} #end loop through site_unique
	
	dev.off()
	cat("Plots written to file. \n")

} #end function