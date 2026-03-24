\name{biodiversity_metrics}
\alias{biodiversity_metrics}

\title{Calculate biodiversity metrics for AusPlots plots}

\description{
  Calculates a suite of biodiversity metrics for all plots, or for a selected subset of plots, directly from an object returned by \code{\link{get_ausplots}}. The function combines species presence/absence information from \code{veg.vouchers} with point-intercept hit counts from \code{veg.PI} to return one row per plot containing species richness, abundance-based diversity metrics, and rarity-based indices.
}

\usage{
  biodiversity_metrics(
    ausplots,
    plots = NULL,
    species_name = c("SN", "HD")
  )
}

\arguments{
  
  \item{ausplots}{
    List object returned by \code{\link{get_ausplots}} containing AusPlots data, including at least \code{veg.vouchers} and \code{veg.PI}.
  }
  
  \item{plots}{
    Optional character vector of AusPlots plot identifiers (\code{site_unique}) for which biodiversity metrics are to be calculated. If \code{NULL}, metrics are calculated for all available plots.
  }
  
  \item{species_name}{
    Character string specifying which plant name field should be used. Options are \code{"SN"} for standardised names and \code{"HD"} for herbarium determinations.
  }
  
}

\details{
  
  The function calculates a set of biodiversity metrics at plot level using two complementary AusPlots vegetation data sources.
  
  Presence/absence-based metrics are calculated from \code{veg.vouchers}, using a species-by-plot matrix generated internally with \code{\link{species_table}}. These metrics include:
    
    \itemize{
      \item Species richness
      \item Range Rarity Richness (RRR)
      \item Corrected Weighted Endemism (CWE)
    }
  
  Abundance-based metrics are calculated from \code{veg.PI}, using the number of point-intercept hits per species within each plot as an abundance proxy. These metrics include:
    
    \itemize{
      \item Total abundance (i.e. total point-intercept hits)
      \item Shannon diversity
      \item Simpson diversity
      \item Pielou evenness
    }
  
  Because abundance-based metrics are derived from point-intercept hits, they reflect community structure rather than full plot richness. In contrast, richness, RRR and CWE are based on the fuller species inventory represented by \code{veg.vouchers}.
  
  Range Rarity Richness (RRR) is calculated as the sum, for each plot, of the inverse frequency of each species across all plots. Corrected Weighted Endemism (CWE) is calculated as RRR divided by species richness.
  
  If abundance data are unavailable or no matching point-intercept records exist for a plot, abundance-based metrics are returned as \code{NA} for that plot.
  
}

\value{
  Returns a data frame with one row per \code{site_unique} and the following columns:
    
    \itemize{
      \item \code{site_unique}: AusPlots plot identifier
      \item \code{species_richness}: Number of species present in the plot
      \item \code{total_abundance}: Total number of point-intercept hits
      \item \code{shannon}: Shannon diversity index
      \item \code{simpson}: Simpson diversity index (\eqn{1-D})
      \item \code{pielou_evenness}: Pielou evenness index
      \item \code{RRR}: Range Rarity Richness
      \item \code{CWE}: Corrected Weighted Endemism
    }
}

\seealso{
  \code{\link{get_ausplots}}
  \cr
  \cr
  \code{\link{species_table}}
}

\author{
  Irene Martín-Forés.
}

\examples{
  
  \dontrun{
    
    library(ausplotsR)
    
    # Download AusPlots data including vouchers and point intercepts
    ausplots_data <- get_ausplots(
      veg.vouchers = TRUE,
      veg.PI = TRUE
    )
    
    # Calculate biodiversity metrics for all available plots
    metrics_all <- biodiversity_metrics(ausplots_data)
    
    # Calculate biodiversity metrics for a subset of plots
    metrics_subset <- biodiversity_metrics(
      ausplots = ausplots_data,
      plots = c("SAAKAN0009-58901", "NSABBS0001-58580"),
      species_name = "SN"
    )
    
    head(metrics_subset)
    
  }
}