\name{basal_area}
\alias{basal_area}
\title{Basal area of tree trunks in TERN AusPlots
}
\description{Calculates basal area (or number of basal wedge hits) for each plot, using the raw basal wedge data returned from \code{get_ausplots}.
}
\usage{
basal_area(veg.basal, by.spp=FALSE, by.hits=FALSE)
}
\arguments{
  \item{veg.basal}{Raw basal wedge data generated from \code{\link{get_ausplots}}.
}
  \item{by.spp}{Set to TRUE to get values per individual species in plots rather than combined values per plot.
}
  \item{by.hits}{Set to TRUE to return the number of individual basal wedge hits rather than basal area per se.
}
}
\details{Basal area is the cross-sectional area of tree trunks per unit area, and is measured at each plot using a basal wedge sweep. Measurement is by species at nine points. Basal area is averaged, while raw tree stem counts are also recorded.
}
\value{Returns a dataframe with rows for plots (or species by plots) and basal area (m2/ha) or hit scores.
}
\author{Greg R. Guerin
}

\seealso{
\code{\link{get_ausplots}}
}
\examples{
#get basal wedge data for selected plots:
my.data <- get_ausplots(my.Plot_IDs=c("SATFLB0004", "QDAMGD0022", "NTASTU0002"),
	veg.vouchers=FALSE, site_info=FALSE, veg.PI=FALSE, basal.wedge=TRUE)

#calculate basal area:
basal_area(my.data$veg.basal)
}
