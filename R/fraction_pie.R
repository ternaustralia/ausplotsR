fraction_pie <- function(frac, row) {
  tryCatch({
  .sentry_capture_message(paste0("ausplotsR: fraction_pie called with row=", row, ", ncol(frac)=", ncol(frac)), level = "info")
	try(pie(x=as.numeric(frac[row,c(2:ncol(frac))]), labels=as.numeric(frac[row,c("bare", "brown", "green", "other")[1:(ncol(frac)-1)]]), main=row, col=c("brown", "orange", "darkgreen", "gray")))
	
	try(legend("topleft", legend=c("Bare ground", "Non-photosynthetic vegetation", "Photosynthetic vegetation", "NA"), pch=20, col=c("brown", "orange", "darkgreen", "gray"), bty="n", cex=0.8, pt.cex=1))
  .sentry_capture_message("ausplotsR: fraction_pie plotted successfully", level = "info")
  }, error = function(e) {
    .sentry_capture_exception(e, "An exception occurred in fraction_pie")
    stop(paste0("Failed to generate fractional cover pie chart: ", conditionMessage(e)), call. = FALSE)
  })
}