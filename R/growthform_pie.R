growthform_pie <- function(GF, colours) {
  tryCatch({

  .sentry_capture_message(paste0("ausplotsR: growthform_pie called with site=", row.names(GF), ", ncol(GF)=", ncol(GF), ", growth_forms=", paste(names(GF)[colSums(GF)>0], collapse=",")), level = "info")

      GF <- GF[, which(colSums(GF) > 0), drop=FALSE]
      
      try(pie(x=as.numeric(GF), labels=round(as.numeric(GF), digits=0), main=row.names(GF), col=colours[names(GF)]))

	try(legend("right", legend=names(GF), pch=20, col=colours[names(GF)], bty="n", cex=0.7, pt.cex=1))
  .sentry_capture_message("ausplotsR: growthform_pie plotted successfully", level = "info")

  }, error = function(e) {
    .sentry_capture_exception(e, "An exception occurred in growthform_pie")
    stop(paste0("Failed to generate growth form pie chart: ", conditionMessage(e)), call. = FALSE)
  })
}
