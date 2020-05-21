growthform_pie <- function(GF, colours) {
	
	GF <- GF[,-(which(colSums(GF) == 0))]
	
	pie(x=as.numeric(GF), labels=round(as.numeric(GF), digits=0), main=row.names(GF), col=colours[names(GF)])
	try(legend("right", legend=names(GF), pch=20, col=colours[names(GF)], bty="n", cex=0.7, pt.cex=1))
	
}
