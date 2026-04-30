\name{get_trait_data_pp}
\alias{get_trait_data_pp}
\title{Fetch pre-processed plant trait data from the AusPlots API}
\description{
  Retrieves species-level functional traits from the AusPlots REST API
  (\code{/species_level_functional_traits}) and returns them as a data frame.
  The column \code{d13c} returned by the API is renamed to \code{d13C} to match
  the internal schema expected by \code{\link{species_traits}} and
  \code{\link{community_traits}}.
}
\usage{
  get_trait_data_pp()
}
\value{
  A data frame with one row per taxon. Columns include:
  \code{species_name}, \code{family_name}, \code{leaf_length},
  \code{leaf_width}, \code{leaf_area}, \code{leaf_mass_per_area},
  \code{max_height}, \code{seed_dry_mass}, \code{seed_length},
  \code{photosynthetic_pathway}, \code{d13C}.
  Numeric trait values are log-transformed where noted in the trait
  documentation. \code{photosynthetic_pathway} is a character column with
  values including \code{"C3"}, \code{"C4"}, \code{"C3-CAM"},
  \code{"C4-CAM"}, \code{"-CAM"}, and \code{"U"} (unknown).
}
\examples{
\dontrun{
traits <- get_trait_data_pp()
head(traits)
}
}
\seealso{
  \code{\link{species_traits}}, \code{\link{community_traits}}
}
\author{
  Irene Martin-Fores, Samantha Munroe, Greg Guerin
}
