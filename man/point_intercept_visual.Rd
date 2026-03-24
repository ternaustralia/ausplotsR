\name{point_intercept_visual}
\alias{point_intercept_visual}

\title{Visualisation of standard 100 × 100 m AusPlots point-intercept surveys}

\description{
  Creates spatial visualisations of vegetation point-intercept observations from AusPlots 100 × 100 m monitoring plots. 
  The function plots the position of each point-intercept observation within the standard plot grid and allows visualisation by 
  substrate, growth form, or dominant species. Optional height-based scaling of symbols can be applied to growth form or dominant 
  species observations. Output can be displayed interactively or exported to a PDF.
}

\usage{
  point_intercept_visual(
    ausplots,
    plots,
    visual = c("substrate"),
    height_growth_form = FALSE,
    height_dominant_species = FALSE,
    top_n_species = 5,
    file = NULL,
    width = 12,
    height = 10
  )
}

\arguments{
  
  \item{ausplots}{
    List object returned by \code{\link{get_ausplots}} containing AusPlots monitoring data, including the components 
    \code{veg.PI} and \code{site.info}.
  }
  
  \item{plots}{
    Character vector of AusPlots plot identifiers to visualise (e.g. \code{"SAAKAN0009-58901"}). Only plots corresponding to 
    standard 100 × 100 m AusPlots monitoring plots will be used.
  }
  
  \item{visual}{
    Character vector specifying which visualisations to produce. Options include \code{"substrate"}, \code{"growth_form"}, and 
    \code{"dominant_species"}. Multiple options can be provided.
  }
  
  \item{height_growth_form}{
    Logical indicating whether point symbols should be scaled according to vegetation height classes when visualising growth form 
    observations. Default is \code{FALSE}.
  }
  
  \item{height_dominant_species}{
    Logical indicating whether point symbols should be scaled according to vegetation height classes when visualising dominant 
    species observations. Default is \code{FALSE}.
  }
  
  \item{top_n_species}{
    Integer specifying the number of most frequent species to include when visualising dominant species. If the requested number 
    exceeds the number of species present in the plot, all species will be displayed.
  }
  
  \item{file}{
    Optional file path for exporting the visualisation to a PDF file. If \code{NULL} (default), plots are displayed in the active 
    graphics device.
  }
  
  \item{width}{
    Width of the output figure in inches when exporting to PDF. Default is \code{12}.
  }
  
  \item{height}{
    Height of the output figure in inches when exporting to PDF. Default is \code{10}.
  }
  
}

\details{
  
  AusPlots vegetation surveys use a standardised point-intercept method across five transects within a 100 × 100 m monitoring plot. 
  Each transect records vegetation structure, species identity, and substrate observations at regularly spaced points.
  
  This function converts the point-intercept observations into spatial coordinates within the plot grid and produces visual representations 
  of these observations. The resulting figures allow users to explore spatial patterns of vegetation structure, substrate cover, and dominant 
  species composition within monitoring plots.
  
  When height scaling is enabled, point sizes are adjusted according to height classes (<3 m, 3–6 m, 6–10 m, >10 m), allowing visualisation 
  of vegetation structural variation within plots.
  
  Dominant species visualisations display the most frequently recorded species within the selected plot, based on the value specified in \code{top_n_species}.
  
  The function automatically filters the dataset to include only standard 100 × 100 m plots based on the \code{plot_is_100m_by_100m} field within \code{site.info}.
  
}

\value{
  Returns the processed point-intercept dataset invisibly. The main output is a series of spatial visualisations of point-intercept observations 
  for the selected plots.
}

\seealso{
  \code{\link{get_ausplots}}
  
  \code{\link{species_table}}
}

\author{
  Irene Martin-Fores and Greg R. Guerin.
}

\examples{
  
  \dontrun{
    
    library(ausplotsR)
    
    # Download AusPlots data including point-intercept observations
    ausplots_data <- get_ausplots(veg.PI = TRUE)
    
    # Visualise growth form and dominant species with height for dominant species 
    for a selected plot point_intercept_visual(
      ausplots = ausplots_data,
      plots = "SAAKAN0009-58901",
      visual = c("growth_form", "dominant_species"),
      height_dominant_species = TRUE
    )
    
  }
}