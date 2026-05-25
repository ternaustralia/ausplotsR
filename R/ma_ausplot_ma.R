####################################
#Based on the 'mama' function of package Simba: https://raw.githubusercontent.com/cran/simba/master/R/mama.R also see https://cran.r-project.org/package=simba

#Code from 'mama' function, see:
# Package:	simba
# Type:	Package
# Version:	0.3-5
# Date:	2012-12-06
# License:	GPL version 2

# Gerald Jurasinski and with contributions from Vroni Retzer
  # (2012). simba: A Collection of functions for similarity analysis
  # of vegetation data. R package version 0.3-5.
  # https://CRAN.R-project.org/package=simba

#To be used as an internal function only in ausplotsR to replace import of simba::mama as simba was archived on CRAN.

ma_ausplot_ma <-
function(dat, spl = TRUE) {
  tryCatch({
	.sentry_capture_message(paste0("ausplotsR: ma_ausplot_ma called with nrow(dat)=", nrow(dat), ", ncol(dat)=", ncol(dat), ", spl=", spl), level = "info")
	dat <- data.frame(dat)
	if(ncol(dat) < 2)
		stop("Mama needs at least two columns (vectors)!")
	plot <- as.character(dat[,1])
	spec <- as.character(dat[,2])
	if(ncol(dat) < 3){
		dat$pres <- 1	
		}
	pres <- dat[,3]
	dat <- data.frame(plot, spec, pres)
	wide <- reshape(dat, v.names="pres", idvar="plot", timevar="spec", direction="wide")
	wide.nms <- sub("pres\\.", "", names(wide))
	if(spl){
		if(is.factor(pres)){
			wide <- sapply(c(1:ncol(wide)), function(x) as.character(wide[,x]))
		}
		wide[is.na(wide)] <- 0
	}
	rownames(wide) <- wide[,1]
	wide <- data.frame(wide)
	names(wide) <- wide.nms
	wide <- data.frame(wide[,-1, drop=FALSE])
	if(length(rownames(wide)) > 1) {
	  wide <- wide[order(rownames(wide)), ]
	}
	if(length(names(wide)) > 1) {
	  wide <- wide[,order(names(wide))]
	}
  .sentry_capture_message("ausplotsR: ma_ausplot_ma reshaped successfully", level = "info")
	wide
  }, error = function(e) {
    .sentry_capture_exception(e, "An exception occurred in ma_ausplot_ma")
    stop(paste0("Failed to reshape site-by-species matrix: ", conditionMessage(e)), call. = FALSE)
  })
}