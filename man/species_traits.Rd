\name{species_traits}
\alias{species_traits}

\title{Merge species with gap-filled leaf-heigth-seed (LHS) traits and photosynthetic pathways}

\description{
This function merges plant species names with a static table of gap-filled
leaf-heigth-seed (LHS) traits derived from AusTraits (Falster et al. 2021), and photosynthetic pathways
from Munroe et al. (2022). This static dataset is included internally
in the ausplotsR package and does not need to be supplied by the user.
Traits extrated from AusTraits displayed by this function include: leaf length, leaf width, 
leaf area, leaf mass per area, maximum plant height, seed length, and seed dry mass. 
Gap-filled methodology used here is the one described in Andrew et al. (2022) and 
Martin-Fores et al. (2023).
Species can be supplied as a character vector, as a dataframe subset, 
or alternatively, extracted automatically from an AusPlots dataset by default.

If no species list or dataframe is provided, the function extracts all species
present in the \code{veg.vouch} table of the supplied AusPlots dataset.

Genus-only records are removed automatically and the final output is returned
as an alphabetically ordered table.
}

\usage{
species_traits(ausplots = NULL,
               species_list = NULL,
               species_df = NULL,
               species_name = c("SN", "HD"))
}

\arguments{

\item{ausplots}{
An AusPlots dataset generated using \code{\link{get_ausplots}}.
If \code{species_list} and \code{species_df} are not provided, all species
present in \code{veg.vouch} will be extracted and matched with the trait dataset.
}

\item{species_list}{
Optional character vector of species names to merge with the trait dataset.
If provided, these names are used instead of extracting species from AusPlots.
}

\item{species_df}{
Optional dataframe containing a column called \code{species_name}. The species
listed in this column will be matched with the trait dataset.
If provided, these names are used instead of extracting species from AusPlots.
}

\item{species_name}{
Which species name field to extract from \code{veg.vouch} when using an
AusPlots dataset.

Options include:
\describe{
\item{"SN"}{Standardised species name (default).}
\item{"HD"}{Herbarium determination name.}
}
}

}

\details{

This function facilitates the integration of plant species data with plant
traits derived from AusTraits and gap-filled when possible following the methodology 
described in Andrew et al. (2022). 
Trait data are stored internally in the package as the dataset
\code{trait_data_pp}. When the function is called, species names
are matched against this dataset to retrieve available trait
information. Species without trait data return NA values.
Species names may originate from three sources:

\itemize{

\item A user-supplied vector of species names (\code{species_list})

\item A dataframe containing a \code{species_name} column (\code{species_df})

\item All species present in the \code{veg.vouch} table of an AusPlots dataset
}

Species names are matched with the supplied trait table using the
\code{species_name} field.

Records containing only a genus name (single-word names) are removed, as trait
information is typically species-specific.

The final output is sorted alphabetically by \code{species_name}.

}

\value{

A dataframe containing species names and all trait columns from the provided
trait dataset. One row is returned per species.

}

\seealso{

\code{\link{get_ausplots}}

\code{\link{species_table}}

}

\author{
Irene Martin-Fores
}

\references{

Falster, D., Gallagher, R., Wenk, E.H. et al. (2021) AusTraits, a curated plant trait 
database for the Australian flora. Scientific Data 8, 254.

Munroe, S. E. M., Guerin, G. R., McInerney, F. A., Martín-Forés, I., Welti, N., 
Farrell, M., Atkins, R., & Sparrow, B. (2022). A vegetation carbon isoscape for Australia 
built by combining continental-scale field surveys with remote sensing. Landscape Ecology, 37(8), 
1987-2006. https://doi.org/10.1007/s10980-022-01476-y

Andrew, SC, Mokany, K, Falster, DS, Wenk, E, Wright, IJ, Merow, C, Adams, V, Gallagher, RV (2021) 
Functional diversity of the Australian flora: strong links to species richness and climate. 
Journal of Vegetation Science 32, e13018. 

Martín-Forés, I., Andrew, S. C., Guerin, G. R., & Rachael V, G. (2023). Linking the functional traits 
of Australian Acacia species to their geographic distribution and invasion status. In Wattles: Australian 
Acacia Species Around the World (pp. 74-92). GB: CABI.
}

\examples{
  
\dontrun{
\examples{

library(ausplotsR)

ausplots_data <- get_ausplots(veg.vouch = TRUE)

# Extract traits for all species in an AusPlots dataset
traits <- species_traits(ausplots = ausplots_data)

# Extract traits for a custom species list
traits <- species_traits(
  species_list = c("Eucalyptus baxteri", "Triodia basedowii")
)

}
}
}
