\name{plot_opt}
\alias{plot_opt}

\title{Plot a set of species accumulation curves together}
\description{This function plots different species accumulation curves obtained through different optimisers in the \code{\link{optim_species}} function.}

\usage{
  plot_opt(optim_result, choices=c("Richness", "RRR", "CWE", "Shannon", 
  "Simpson", "SimpsonBeta", "Frequent", "SimpsonBeta_randSeed", "Random"))
}
    
\arguments{
      \item{optim_result}{Object returned from the \code{\link{optim_species}} function, which is a list of species accumulation objects (see \code{\link[vegan]{specaccum}}) obtained with the different optimisers.}
      \item{choices}{select the optimisers from which the species accumulations curves are going to be plotted. The choices can be "Richness", "RRR", "CWE", "Shannon", "Simpson", "SimpsonBeta", "Frequent", "SimpsonBeta_randSeed", "Random"}
}
    
\details{Line colours are assigned randomly on each run.
}

\value{
    This function will return a plot of the species cumulative curves obtained by the different optimisers in the \code{\link{optim_species}} function.
}
    
\seealso{
\code{\link{optim_species}}
}
    
\author{Greg R. Guerin}
  
\examples{
  #example with dune database from vegan
  library(vegan)
  data(dune)
  
  example1 <- optim_species(dune, n.plt=15, frequent=FALSE, plot=FALSE)
  
  plot_opt(example1)
  plot_opt(example1, choices=c("Richness", "SimpsonBeta"))
      
  #example with auplots database from ausplotsR
  \dontrun{
  library(ausplotsR)
  
  ausplotsdata <- get_ausplots(site_info=TRUE, veg.PI=TRUE)
  ausplotsPAdata <- species_table(ausplotsdata$veg.PI, m_kind="PA", 
  species_name="SN")
  
  example2 <- optim_species(ausplotsPAdata, n.plt=5, iterations=5, plot=FALSE)
  plot_opt(example2)
  plot_opt(example2, 
  choices=c("RRR", "SimpsonBeta", "Frequent", "SimpsonBeta_randSeed"))
  }
}  

            
      
