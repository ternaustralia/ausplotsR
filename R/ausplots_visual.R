ausplots_visual <- function(my.ausplots.object = NULL, map = TRUE, map.attribute = TRUE, fraction.pie = TRUE, growthform.pie = TRUE, cumulative.cover = TRUE, whittaker = TRUE, outfile = NULL, max.plots=5) {
	
	#############################
	#check input formats
	if(!missing(my.ausplots.object)) {
		if(any(map.attribute, fraction.pie, growthform.pie, cumulative.cover, whittaker)) {
			if(!("veg.PI" %in% names(my.ausplots.object))) {
				stop("If you provide a my.ausplots.object it must contain $veg.PI data to generate those plots!")
			} #cls if no veg.PI data
			if(length(unique(my.ausplots.object$veg.PI$site_unique)) > 20) {
				cat("")
			} #close if more than 20 plots
		} #close if veg plots requested
		if(any(map, map.attribute)) {
			if(!("site.info" %in% names(my.ausplots.object))) {
				stop("If you provide a my.ausplots.object it must contain $site.info data to generate maps!")
			}
		} #close if map requested
	} #close if object provided
		if(any(fraction.pie, cumulative.cover)) {
			if(length(unique(my.ausplots.object$veg.PI$site_unique)) > max.plots) {
				cat("You are trying to plot fractional or cumulative cover for too many sites, which can be slow. Only plotting a subset: you can also increase 'max.plots' in your call...")
				strip_ <- 1
			}
		}
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
		
		if(any(fraction.pie, growthform.pie, cumulative.cover, whittaker)) {
			
		cat("Working on individual plots for ", i, "\n")
		if(strip_ == 1) {
			my.ausplots.object$veg.PI <- subset(my.ausplots.object$veg.PI, site_unique %in% unique(my.ausplots.object$veg.PI$site_unique)[1:max.plots])
		}
		
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
		cumulative_cover(subset(my.ausplots.object$veg.PI, site_unique == unique(my.ausplots.object$veg.PI$site_unique)[n]))
		}

	if(whittaker) {
		if(n == 1) {
			sppBYsites <- species_table(my.ausplots.object$veg.PI, m_kind="percent_cover", cover_type="PFC")
			}
		par(mar=c(5,5,5,5))
		whitt.plot(sppBYsites[i,])
		}
		
		} #end if any veg.PI plots
		
		} #end loop through site_unique
	
	dev.off()
	cat("Plots written to file. \n")

} #end function