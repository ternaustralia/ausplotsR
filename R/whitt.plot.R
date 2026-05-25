whitt.plot <- function(abund_vect) {
  tryCatch({
  .sentry_capture_message(paste0("ausplotsR: whitt.plot called with site=", rownames(abund_vect), ", n_species=", ncol(abund_vect)), level = "info")
	try(plot(vegan::rad.lognormal(round(abund_vect, digits=0), log="y"), pch=20, main=rownames(abund_vect), bty="l"))
	try(legend("topright", lwd=3, legend=c("Lognormal"), bty="n"))
  .sentry_capture_message("ausplotsR: whitt.plot rendered successfully", level = "info")
  }, error = function(e) {
    .sentry_capture_exception(e, "An exception occurred in whitt.plot")
    stop(paste0("Failed to generate Whittaker plot: ", conditionMessage(e)), call. = FALSE)
  })
}