\name{species_list}
\alias{species_list}
\title{Species lists for AusPlots
}
\description{Nice species lists based on the vegetation voucher module of Ausplots generated using the \code{\link{get_ausplots}} function.
}
\usage{
species_list(veg.vouch, grouping=c("by_site", "by_visit", "collapse"), 
species_name=c("SN","HD","GS"), strip_bryophytes=FALSE, 
append_family=FALSE, writefile=FALSE, outfile="my_species_lists.txt")
}
\arguments{
  \item{veg.vouch}{The input raw plant voucher data generated by \code{\link{get_ausplots}}
  }
  \item{grouping}{One of \code{c("by_site", "by_visit", "collapse")}. Determines whether species names are  pooled over all visits to each plot (\code{site_location_name}, the default), or gathered by individual site visits (\code{site_unique}). The final option 'collapse' returns a complete list of species over the plots provided.
  }
  \item{species_name}{Defaults to "SN" (standardised_name); set to  "HD" to use "herbarium_determination", and "GS" to use standardised "genus_species" format. See details for an explanation of each setting.
  }
  \item{strip_bryophytes}{Logical. Switch to \code{TRUE} to exclude bryophyte records and only return vascular plants in Angiosperm (flowering plants), Gymnosperm (conifers) and Pteridophyte (ferns and allies) groups. Although there are a number of bryophyte records in Ausplots, they are not surveyed systematically.
  }
  \item{append_family}{Logical. If set to \code{TRUE}, names in the the returned species lists are appended with (and sorted by) family standardised name.
  }
  \item{writefile}{Logical. Whether or not to write the result to a text file.
  }
  \item{outfile}{Character denoting path/file/extension to attempt to write to if \code{writefile=TRUE}.
  }
  }

\details{The aim of the function is to provide simple, cleaned lists of species that have been recorded at various Ausplots sites, simplifying from the more detailed voucher and identification information in the \code{$veg.vouch} vegetation voucher module. The output is not designed for downstream analysis (which is perhaps better performed by manipulating the raw data tables) but as a rapid  means of viewing recorded species.

\code{species_name} provides multiple options for how names are presented. If \code{species_name="HD"}, the function will use the "herbarium_determination" provided by state herbaria to identify unique species. Herbarium determinations are made to the lowest possible taxonomic rank, which may be the family, genus, species, subspecies or variety. If \code{species_name="SN"}, the default, the function will use the the "standardised_name" to identify unique species. Values are based on the \code{veg.PI$herbarium_determination} but have been matched to the accepted scientific name according to the Australian Plant Census (APC: https://www.anbg.gov.au/cpbr/program/hc/hc-APC.html) and the Australian Plant Name Index (APNI: https://www.anbg.gov.au/cpbr/program/hc/hc-APNI.html). The standardised name field therefore helps to ensure taxonomic consistency. It excludes any records that don't match the APC. If \code{species_name="GS"}, only the standardised genus species rank ("genus_species" in the veg.vouch input table) is used to identify species. \code{species_name="GS"} maximises consistency in species comparisons between plots, but reduces the taxonomic detail.
}


\value{Returns a list containing a character vector of species names for each plot/visit grouping. If \code{grouping="collapse"}, a single character vector is returned. If \code{writefile=TRUE}, a text file is written to path \code{outfile} in the working directory.
}

\author{Greg Guerin, Samantha Munroe
}

\seealso{
  \code{\link{get_ausplots}}
  \cr
  \cr
  \code{\link{species_table}}
}
\examples{
  \dontrun{
  #get veg.vouch data for selected plots:
  my.data <- get_ausplots(site_info=FALSE, veg.vouchers=TRUE, 
  my.Plot_IDs=c("SAAFLB0005", "SAAFLB0006"))
  
  #generate 'genus_species' lists by site:
  species_list(my.data$veg.vouch, grouping="by_site", 
  species_name="GS", writefile=FALSE)
  
  #generate a complete species list ordered by family:
  species_list(my.data$veg.vouch, grouping="collapse", 
  species_name="SN", append_family=TRUE, writefile=FALSE)
  }
}

