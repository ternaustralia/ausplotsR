\name{get_ausplots}
\alias{get_ausplots}
\title{Extract AusPlots data for sites, soils and vegetation
}
\description{This function is the starting point for accessing data through the ausplotsR package.
}
\usage{
get_ausplots(my.Plot_IDs="none", site_info=TRUE, structural_summaries=FALSE,
	veg.vouchers=TRUE, veg.PI=TRUE, basal.wedge=FALSE, soil_subsites=FALSE,
	soil_bulk_density=FALSE, soil_character=FALSE, bounding_box="none",
	herbarium_determination_search=NULL, family_search=NULL, 
	standardised_name_search=NULL)
}
\arguments{
  \item{my.Plot_IDs}{Optional character vector of AusPlots plot IDs to request data for specific set of plots.
}
  \item{site_info}{Whether site summary data are required (includes plot and visit details, landform data, geographic coordinates, notes etc). Default is to include.
}
  \item{structural_summaries}{Whether site vegetation structural summaries are required.
}
  \item{veg.vouchers}{Whether vegetation vouchers data are requested - contains a complete set of species records for the plot determined by a herbarium plus ID numbers for silica-dried tissue samples. Default is to include.
}
  \item{veg.PI}{Whether point-intercept data are requested; includes data on substrate, plant species, growth form and height etc at each of (typically) 1010 points per plot. Default is to include. 
}
  \item{basal.wedge}{Whether basal wedge data raw hits are required to enable calculation of basal area by species by plot.
}
  \item{soil_subsites}{Whether soil subsite information is required. This includes information on what soil and soil metagenomics samples were taken at nine locations across the plot and their identification barcode numbers.
}
  \item{soil_bulk_density}{Whether soil bulk density data are required.
}
  \item{soil_character}{Whether soil characterisation and sample ID data at 10 cm increments to a depth of 1 m are required.
}
  \item{bounding_box}{An additional optional spatial filter for selecting AusPlots based on a rectangular box, in the format of e.g. c(xmin, xmax, ymin, ymax). AusPlots location data are are in longlat, therefore x is the longitude and y is the latitude of the box/extent object. e.g., c(120, 140, -30, -10).
}

  \item{herbarium_determination_search}{Optional character string filter for selecting AusPlots based on the presence of a genus or species as recorded in the "herbarium_determination" column. See details for search term settings.

}
\item{family_search}{Optional character string filter for selecting AusPlots based on the presence of a plant family as recorded in the 'family' column.  Will be NA if no family indentification was possible. See details for search term settings.
}
\item{standardised_name_search}{Optional character string filter for selecting AusPlots based on the presence of a genus and/or species as recorded in the 'standardised_name' column. Will be NA if no genus rank (at minimum) was possible. See details for search term settings.
}
}

\details{By default, the function will extract and compile data from the vegetation vouchers and point intercept modules over all available plots and provide a basic site information table. Arguments allow the user to select sites by ID numbers, geographic bounding box, or species, and select which modules data are drawn from. Further spatial filtering of sites is possible by using the 'bioregion_name', 'state', and spatial coordinate fields in the site table.

The datasets returned by the function are used directly by further formatting and analysis functions in ausplotsR (such as \code{\link{species_table}} to generate a species by sites matrix for community analysis), making it simple to go from raw data extract to ecological analysis.
	
AusPlots surveys are identified by a plot ID and unique visit number, that are combined to identify a unique measurement for sites with repeat visits, identified in the extracted data tables as 'site_unique'.

"herbarium_determination_search", "family search", and "standardised_name_search" search terms are not case sensitive and do not require an exact taxonomic match (e.g. "Eucalyptus moderata", "Eucalyptus", and "euca" are all acceptable search terms). If \code{veg.vouch=TRUE}, veg.vouch will return a data frame that only includes voucher records that match the species or family search. If \code{veg.PI=TRUE}, veg.PI will return point intercept data for all plots where the species or family occurs. If \code{basal.wedge=TRUE}, veg.basal will return a dataframe that only includes the raw basal wedge data of records that match the species or family. For all other data.frames, get_ausplots will return data for all the plots where the species or family occurs. 

\code{herbarium determinations} are provided by state herbaria and are the most commonly used scientific names in the given state. However, scientific names may differ between states and with international databases due to disagreements on taxonomy/nomenclature. Herbarium determinations are made to the lowest possible taxonomic rank, which may be the family, genus, species, subspecies or variety. It also includes incomplete identifications, dead or generic vegetation identifications (e.g. "Dead Tree/Shrub", "Annual Grass"), and "No ID" labels.
    
\code{standardised_name_search} values are based on the \code{veg.PI$herbarium_determination} but have been edited to match the most commonly used or widely accepted scientific name according to "World Flora Online" (http://www.worldfloraonline.org/). This helps ensure scientific names will not differ between states and will be more consistent with global databases. \code{standardised_name_search} determinations are made to the lowest possible taxonomic rank, which may be the genus, species, subspecies or variety, but not family. It excludes dead or generic vegetation identifications (e.g. "Dead Tree/Shrub", "Annual Grass") and "No ID" labels. The genus, specific_epithet, infraspecific_rank, infraspecific_epithet and genus_species based on the standardised_name are listed as separate columns in veg.PI, veg.vouch, and veg.basal data tables.  
}

\value{Returns a list with following elements, depending on the AusPlots modules requested in the function call.
  \item{$site.info }{Data frame with basic site information including location.}
  \item{$struct.summ }{Data frame with vegetation structural summaries for each plot.}
  \item{$veg.vouch }{Data frame with rows for each voucher and information on species determinations and silica-dried tissue samples.}
  \item{$veg.PI }{Data frame with individual point-intercept data.}
  \item{$veg.basal }{Data frame with compiled raw basal wedge hit data.}
  \item{$soil.sub }{Data frame with details of soil subsites within each plot including sample IDs.}
  \item{$soil.bulk }{Data frame with raw bulk density data from each plot.}
  \item{$soil.char }{Data frame with soil characterisation data from the 1 m pit at the SW corner of each plot.}
  \item{$citation }{Auto-generated citation for the data extracted. Please cite ausplotsR and the TERN AusPlots data you use.}
}

\author{Greg Guerin, Andrew Tokmakoff, Tom Saleeba, Samantha Munroe
}

\seealso{
\code{\link{species_table}}
}
\examples{
#get data for selected plots:
my.data <- get_ausplots(my.Plot_IDs=c("SATFLB0004", "QDAMGD0022", "NTASTU0002"), 
veg.PI=FALSE, basal.wedge=TRUE, structural_summaries=TRUE)

#View module:
my.data$struct.summ

#Get data citation and extraction date info:
my.data$citation

#Get site info, veg vouchers and veg point intercept for all available plots:
\dontrun{get_ausplots()} 

#Get voucher records for "Eucalyptus moderata" using "herbarium_determination":
Eucalyptus_moderata <- get_ausplots(veg.PI=FALSE, 
herbarium_determination_search="Eucalyptus moderata")
Eucalyptus_moderata$veg.vouch

#Get veg voucher records for all species in the family "Myrtaceae"
Myrtaceae <- get_ausplots(veg.PI=FALSE, family_search="Myrtaceae")
Myrtaceae$veg.vouch

}
