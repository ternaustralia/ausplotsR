\name{plot.opt}
\alias{plot_optim}

\title{Plot species accumulation curves derived from the optim_species function}
\description{This function allows plotting different species accumulation curves obtained from selected sites through different optimisers in the \code{\link{optim_species}} function.}

\usage{
  plot.opt(optim_result, choices)
}
    
\arguments{
      \item{optim_result}{Object returned from the \code{\link{optim_species}} function, which is a list of species accumulation objects obtained with the different optimisers.}
      \item{choices}{select the optimisers from which the species accumulations curves are going to be plotted. 
      The choices can be "Richness", "RRR", "CWE", "Shannon", "Simpson", "SimpsonBeta", "Frequent", "SimpsonBeta_randSeed", "Random"}
}
    
\value{
    This function will return a plot including the species cumulative curves obtained by the different optimisers in the optim_species function.
}
    
    
\author{Greg R. Guerin}
  
\examples{
  \example with dune database from vegan
  library(vegan)
  data(dune)
  example1 <- optim_species(dune, n.plt=15, iterations=20)
  plot.opt(example1)
  plot.opt(example1, choices=c("Richness", "SimpsonBeta"))
      
  \example with auplots database from ausplotsR
  \dontrun{
  library(ausplotsR)
  ausplotsdata <- get_ausplots(site_info=TRUE, veg.PI=TRUE, veg.vouchers=FALSE)
  ausplotsPAdata <- species_table(my.data$veg.PI, m_kind="PA")
  example2 <- optim_species(ausplotsPAdata, n.plt=5, iterations= 5)
  plot.opt(example2)
  plot.opt(example2, choices=c("RRR", "SimpsonBeta", "Frequent", "SimpsonBeta_randSeed"))
  }
}  

            
      
