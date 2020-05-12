whitt.plot <- function(abund_vect) {
	plot(vegan::radfit(round(abund_vect, digits=0), log="xy"), pch=20)                          
}