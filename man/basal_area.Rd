\name{basal_area}
\alias{basal_area}
\title{Basal area of tree trunks in TERN AusPlots
}
\description{Calculates basal area (or number of basal wedge hits) for each plot, using the raw basal wedge data returned from \code{get_ausplots}.
}
\usage{
basal_area(veg.basal, by.spp=FALSE, by.hits=FALSE, species_name=c("HD","SN","GS"))
}
\arguments{
  \item{veg.basal}{Raw basal wedge data generated from \code{\link{get_ausplots}}.
}
  \item{by.spp}{Set to TRUE to get values per individual species in plots rather than combined values per plot.
}
  \item{by.hits}{Set to TRUE to return the number of individual basal wedge hits rather than basal area per se.
}
\item{species_name}{If \code{by.spp=TRUE}, set to "SN" (default) to use "standardised_names"", "HD" to use "herbarium_determination"", and "GS" to use "genus_species". See details for explaination of each setting.

}
}

\details{ \code{species_name} provides multiple options for calculating basal area if \code{by.spp=TRUE}. If \code{species_name=c("HD")}, the function will use the "herbarium_determination" provided by state herbaria to indentify unique species. Herbarium determinations are the most commonly used scientific names in the given state and vary between states due to disagreements on taxonomy/nomenclature. Herbarium determinations are also made to the lowest possible taxonomic rank, which may be the family, genus, species, subspecies or variety. These attributes may interfere with between-plot comparisons. If \code{species_name=c("SN")}, the default, scientific names are based on herbarium determinations but have been edited to match the most commonly used or widely accepted scientific name in Australia, referred to as the "standardised_name" in the veg.basal input table. This helps ensure scientific names are more consistent between plots in different states. However, standardised_names are also assessed to the lowest possible taxonomic rank. If \code{species_name=c("GS")}, only the standardised Genus species rank ("genus_species" in the veg.basal input table) is used to caluclate basal area or the number of basal wedge hits. This helps maximise comparability and consistency between plots, but reduces the taxonomic detail and records where only the family or genus was identified are excluded.
}
\value{Returns a data frame with rows for plots (or species by plots) and basal area (m2/ha) or hit scores.
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
