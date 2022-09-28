whitt.plot <- function(abund_vect) {
	try(plot(vegan::rad.lognormal(round(abund_vect, digits=0), log="y"), pch=20, main=rownames(abund_vect), bty="l"))
	try(legend("topright", lwd=3, legend=c("Lognormal"), bty="n"))                       
}