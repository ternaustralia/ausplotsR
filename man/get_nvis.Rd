\name{get_nvis}
\alias{get_nvis}
\title{Retrieve NVIS attributes for AusPlots sites}

\description{
  Returns NVIS vegetation attributes associated with AusPlots sites by
  matching on \code{site_unique}. The function uses the internal
  \code{NVIS} dataset included in the package.
  
  The function can return NVIS data for all sites or for a subset of sites.
  It accepts either a vector of site identifiers or a data frame containing
  a \code{site_unique} column, allowing it to integrate easily with outputs
  from other \code{ausplotsR} functions and pipe workflows.
}

\usage{
  get_nvis(site_unique = NULL)
}

\arguments{
  
  \item{site_unique}{
    Optional input specifying which sites to retrieve NVIS data for. Can be:
      
      \itemize{
        \item \code{NULL} (default): return NVIS data for all AusPlots sites
        \item a character vector of \code{site_unique} identifiers
        \item a data frame containing a \code{site_unique} column
      }
    
    If a data frame is supplied, unique \code{site_unique} values will be
    extracted and used to retrieve the corresponding NVIS records.
  }
  
}

\value{
  A data frame containing \code{site_unique} and all NVIS attributes
  stored in the internal \code{NVIS} dataset.
}

\examples{
  # NVIS information for all sites
  nvis <- get_nvis()
  
  # NVIS information for a single site
  nvis <- get_nvis("NSABHC0007-53602")
  
  # NVIS information for several sites
  nvis <- get_nvis(c("NSABHC0007-53602", "NSABBS0002-58555"))
}

\author{
  Irene Martin-Fores, Donna Lewis, Carly Steen
}