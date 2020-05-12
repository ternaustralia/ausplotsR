fraction_pie <- function(frac, row) {
	pie(x=as.numeric(frac[row,c("bare", "brown", "green", "NA.")]), labels=as.numeric(frac[row,c("bare", "brown", "green", "NA.")]), main=row.names(frac)[row], col=c("brown", "orange", "darkgreen", "gray"))
	
	legend("right", legend=c("Bare ground", "Non-photosynthetic vegetation", "Photosynthetic vegetation", "NA"), pch=20, col=c("brown", "orange", "darkgreen", "gray"), bty="n", cex=0.7)
}