\name{community_traits}
\alias{community_traits}

\title{Calculate plot-level community trait metrics from AusPlots point-intercept data}

\description{
  Calculates plot-level community-weighted trait metrics from AusPlots
  point-intercept data using the internal \code{trait_data_pp} dataset.
  
  The function returns community-weighted means (CWMs) for numeric traits,
  proportions of C3 and C4 photosynthetic pathways, and functional diversity,
  functional redundancy, and functional uniqueness metrics for each plot.
}

\usage{
  community_traits(ausplots,
                   plots = NULL,
                   species_name = c("SN", "HD"),
                   cwm_traits = NULL,
                   fd_traits = c("max_height", "seed_dry_mass", "leaf_mass_per_area"),
                   categorical_trait = "photosynthetic_pathway")
}

\arguments{
  
  \item{ausplots}{
    An AusPlots dataset generated using \code{\link{get_ausplots}} containing
    at least the \code{veg.PI} table.
  }
  
  \item{plots}{
    Optional character vector of \code{site_unique} plot identifiers for which
    metrics are to be calculated. If \code{NULL}, all available plots are used.
  }
  
  \item{species_name}{
    Which species name field to extract from \code{veg.PI}.
    
    Options include:
      \describe{
        \item{"SN"}{Standardised species name (default).}
        \item{"HD"}{Herbarium determination name.}
      }
  }
  
  \item{cwm_traits}{
    Optional character vector specifying which numeric trait columns should be used
    to calculate community-weighted means (CWMs). If \code{NULL}, all numeric
    traits available in the internal trait dataset are used.
  }
  
  \item{fd_traits}{
    Character vector specifying which numeric traits should be used to calculate
    functional diversity, functional redundancy, and functional uniqueness.
    The default is \code{c("max_height", "seed_dry_mass", "leaf_mass_per_area")}.
  }
  
  \item{categorical_trait}{
    Character string specifying the categorical trait column used to calculate
    pathway proportions. The default is \code{"photosynthetic_pathway"}.
  }
  
}

\details{
  
  This function uses species-level point-intercept hits from \code{veg.PI} as an
  abundance proxy to calculate plot-level trait metrics.
  
  Trait values are retrieved from the internal dataset \code{trait_data_pp},
  which contains gap-filled plant trait data and photosynthetic pathway
  information.
  
  The function calculates:
    
    \itemize{
      \item Community-weighted means (CWMs) for selected numeric traits
      \item Proportions of species hits assigned to C3 and C4 pathways
      \item Functional richness (FRic)
      \item Functional evenness (FEve)
      \item Functional dispersion (FDis)
      \item Rao`s quadratic entropy (RaoQ)
      \item Functional redundancy
      \item Functional uniqueness
}

Photosynthetic pathway values are simplified so that:
\itemize{
  \item \code{C3-CAM} is treated as \code{C3}
  \item \code{C4-CAM} is treated as \code{C4}
}

Only \code{prop_C3} and \code{prop_C4} are returned.

Functional diversity, redundancy, and uniqueness are calculated only from the
traits listed in \code{fd_traits}, whereas CWMs can be calculated for all
numeric traits in the internal dataset.

Plots for which functional indices cannot be calculated are retained in the
output, with the relevant columns returned as \code{NA}.

Genus-only records are excluded before calculations.

}

\value{

A dataframe with one row per \code{site_unique}, containing:

\itemize{
  \item \code{site_unique}: plot identifier
  \item \code{CWM_*}: community-weighted means for selected numeric traits
  \item \code{prop_C3}: proportion of point-intercept hits assigned to C3 species
  \item \code{prop_C4}: proportion of point-intercept hits assigned to C4 species
  \item \code{FRic}: functional richness
  \item \code{FEve}: functional evenness
  \item \code{FDis}: functional dispersion
  \item \code{RaoQ}: Rao`s quadratic entropy
  \item \code{functional_redundancy}: community-level functional redundancy
  \item \code{functional_uniqueness}: community-level functional uniqueness
    }

}

\seealso{
  \code{\link{get_ausplots}}
  \cr
  \cr
  \code{\link{species_traits}}
  \cr
  \cr
  \code{\link{biodiversity_metrics}}
}

\author{
  Irene Martín-Forés
}

\examples{
  
  \dontrun{
    
    library(ausplotsR)
    
    ausplots_data <- get_ausplots(veg.PI = TRUE)
    
    # Calculate community trait metrics for all plots
    community_metrics <- community_traits(ausplots_data)
    
    # Calculate metrics for a subset of plots
    community_metrics_subset <- community_traits(
      ausplots = ausplots_data,
      plots = c("SAAKAN0009-58901", "NSABBS0001-58580")
    )
    
    # Use all numeric traits for CWMs but only selected traits for
    # functional diversity metrics
    community_metrics_selected <- community_traits(
      ausplots = ausplots_data,
      fd_traits = c("max_height", "seed_dry_mass", "leaf_mass_per_area")
    )
    
  }
}