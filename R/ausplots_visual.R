ausplots_visual <- function(my.ausplots.object = NULL, map = TRUE, map.attribute = TRUE, fraction.pie = TRUE, growthform.pie = TRUE, cumulative.cover = TRUE, whittaker = TRUE, outfile = NULL, max.plots=5) {
	
  #############################
  #reset graphical defaults
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
	#############################
	#check input formats
	if(!any(map, map.attribute, fraction.pie, growthform.pie, cumulative.cover, whittaker)) {stop("You have not requested any outputs! Set at least one option to TRUE")}
	if(!missing(my.ausplots.object)) {
		if(any(map.attribute, fraction.pie, growthform.pie, cumulative.cover, whittaker)) {
			if(!("veg.PI" %in% names(my.ausplots.object))) {
				stop("If you provide a my.ausplots.object it must contain $veg.PI data to generate those plots!")
			} #close if no veg.PI data
		} #close if veg plots requested
		if(any(map, map.attribute)) {
			if(!("site.info" %in% names(my.ausplots.object))) {
				stop("If you provide a my.ausplots.object it must contain $site.info data to generate maps!")
			} #close if no site info
		} #close if map requested
		##########
	#check number of heavy plots requested
		if(any(fraction.pie, cumulative.cover)) {
			if(length(unique(my.ausplots.object$veg.PI$site_location_name)) > max.plots) {
				warning("You are trying to plot fractional or cumulative cover for many sites, which can be slow. Only plotting a subset: you can increase the number of sites with 'max.plots' in your call...")
				strip_ <- 1
			} #cls if more than max plots message
		} #close if fraction or cover	
	} #close if object provided
	
	#################################################
	#check data and extract if missing
	if(missing(my.ausplots.object)) {
		message("Obtaining sample data...") #combine two calls to get all sites but only veg.PI for that bounding box so make it quicker.
		my.ausplots.object <- get_ausplots(veg.vouchers=FALSE, veg.PI=FALSE) #get all sites
		my.ausplots.object$veg.PI <- get_ausplots(my.Plot_IDs=sample(my.ausplots.object$site.info$site_location_name, max.plots), site_info=FALSE, veg.vouchers=FALSE)$veg.PI #get veg PI for random plots  - according to max.plots
	}
	
	###############################################
	#start plot
	if(missing(outfile)) {
		outfile = paste("ausplots_demo_plots", format(Sys.time(), "%b_%d_%Y"), ".pdf")
		}
	
#####################################################	
	pdf(outfile, width=9, height=6)
	
	message("Generating plots...")
	
	##########################################
	#call individual graphical functions that are switched on
	if(map) {
		map_ausplots(my.ausplots.object)
		}

	if(map.attribute) {
		map_attribute(my.ausplots.object)
		}
		
		if(any(fraction.pie, growthform.pie, cumulative.cover, whittaker)) { #

		
		if(exists("strip_")) {
			my.ausplots.object$veg.PI <- subset(my.ausplots.object$veg.PI, site_location_name %in% unique(my.ausplots.object$veg.PI$site_location_name)[1:max.plots])
		} #cls if strip_ exists
		
	par(mfrow=c(2,2))
	n <- 0
	for(i in sort(unique(my.ausplots.object$veg.PI$site_unique))) {
		
		n <- n + 1
		
				
		message("Working on individual plots for ", i)
		
	if(fraction.pie) {
		if(n == 1) {
			frac <- suppressWarnings(fractional_cover(my.ausplots.object$veg.PI))
		}
		par(mar=c(0,0,2,0))
		fraction_pie(frac, n)
		}

	if(growthform.pie) {
		if(n == 1) {
			GF <- growth_form_table(my.ausplots.object$veg.PI, m_kind="percent_cover", cover_type=("PFC"), species_name="SN")
			GF_col <- rev(terrain.colors(length(names(GF))))
			names(GF_col) <- names(GF)
		}
		par(mar=c(0,0,2,0))
		growthform_pie(GF[n,], GF_col)
		}

	if(cumulative.cover) {
		par(mar=c(5,5,5,2))
		cumulative_cover(subset(my.ausplots.object$veg.PI, site_unique == i))
		}

	if(whittaker) {
		if(n == 1) {
			sppBYsites <- species_table(my.ausplots.object$veg.PI, m_kind="percent_cover", cover_type="PFC", species_name="SN")
			}
		par(mar=c(5,5,5,2))
		whitt.plot(sppBYsites[i,])
		}
		
		} #end loop through site_unique

	} #end if any individual veg.PI plots
	
	dev.off()
	message("Plots written to file.")

} #end function