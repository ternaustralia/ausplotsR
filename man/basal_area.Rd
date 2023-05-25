\name{basal_area}
\alias{basal_area}
\title{Basal area of tree trunks in TERN AusPlots
}
\description{Calculates basal area (or number of basal wedge hits) for each plot, using the raw basal wedge data returned from \code{get_ausplots}.
}
\usage{
basal_area(veg.basal, by.spp=FALSE, by.hits=FALSE, 
species_name=c("SN","HD","GS"))
}
\arguments{
  \item{veg.basal}{Raw basal wedge data generated from \code{\link{get_ausplots}}.
}
  \item{by.spp}{Set to TRUE to get values per individual species in plots rather than combined values per plot.
}
  \item{by.hits}{Set to TRUE to return the number of individual basal wedge hits rather than basal area per se.
}
\item{species_name}{If \code{by.spp=TRUE}, set to "SN" to use "standardised_names" (the default), "HD" to use "herbarium_determination", and "GS" to use "genus_species". See details for explanation of each setting.
}
}

\details{Basal area is the cross-sectional area of tree trunks per unit area, and is measured at each plot using a basal wedge sweep. Measurement is by species at nine points. Basal area is averaged, while raw tree stem counts are also recorded.

\code{species_name} provides multiple options for calculating basal area if \code{by.spp=TRUE}. For example, if \code{species_name="HD"}, the function will use the "herbarium_determination" provided by state herbaria to identify unique species. "herbarium_determination" are made to the lowest possible taxonomic rank, which may be the family, genus, species, subspecies or variety. If \code{species_name="SN"}, the default, the function will use the the "standardised_name" to identify unique species. Values are based on the \code{veg.PI$herbarium_determination} but have been matched to the accepted scientific name according to the Australian Plant Census (APC: https://www.anbg.gov.au/cpbr/program/hc/hc-APC.html) and the Australian Plant Name Index (APNI: https://www.anbg.gov.au/cpbr/program/hc/hc-APNI.html). The standardised name field therefore helps to ensure taxonomic consistency. It excludes any records that don't match the APC. If \code{species_name="GS"}, only the standardised genus species rank ("genus_species" in the veg.basal input table) is used to identify species. \code{species_name="GS"} maximises consistency in species comparisons between plots, but reduces the taxonomic detail.


}
\value{Returns a dataframe with rows for plots (or species by plots) and basal area (m2/ha) or hit scores.
}
\author{Greg R. Guerin, Samantha E.M. Munroe
}

\seealso{
\code{\link{get_ausplots}}
}
\examples{
\dontrun{
#get basal wedge data for selected plots:
my.data <- get_ausplots(my.Plot_IDs=c("SATFLB0004", "QDAMGD0022", "NTASTU0002"),
site_info=FALSE, basal.wedge=TRUE)

#calculate basal area:
basal_area(my.data$veg.basal)
}
}
