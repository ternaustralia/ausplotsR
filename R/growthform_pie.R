growthform_pie <- function(GF, row) {
	pie(x=as.numeric(GF[row,]), labels=round(as.numeric(GF[row,]),digits=0), main=row.names(GF)[row], col=rev(terrain.colors(length(names(GF)))))
	legend("right", legend=names(GF), pch=20, col=rev(terrain.colors(length(names(GF)))), bty="n", cex=0.7)
	
}