fraction_pie <- function(frac, row) {
	try(pie(x=as.numeric(frac[row,c(2:ncol(frac))]), labels=as.numeric(frac[row,c("bare", "brown", "green", "NA.")[1:(ncol(frac)-1)]]), main=row.names(frac)[row], col=c("brown", "orange", "darkgreen", "gray")))
	
	try(legend("topleft", legend=c("Bare ground", "Non-photosynthetic vegetation", "Photosynthetic vegetation", "NA"), pch=20, col=c("brown", "orange", "darkgreen", "gray"), bty="n", cex=0.8, pt.cex=1))
}