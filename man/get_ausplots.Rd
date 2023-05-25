\name{get_ausplots}
\alias{get_ausplots}
\title{Extract AusPlots data for sites, soils and vegetation
}
\description{This function is the starting point for accessing data through the ausplotsR package.
}
\usage{
get_ausplots(my.Plot_IDs="none", site_info=TRUE, structural_summaries=FALSE,
	veg.vouchers=FALSE, veg.PI=FALSE, basal.wedge=FALSE, soil_subsites=FALSE,
	soil_bulk_density=FALSE, soil_character=FALSE, plot_search=NULL, 
	bounding_box="none", herbarium_determination_search=NULL, family_search=NULL, 
	standardised_name_search=NULL, dictionary=FALSE)
}
\arguments{
  \item{my.Plot_IDs}{Optional character vector of AusPlots plot IDs to request data for specific set of plots.
}
  \item{site_info}{Whether site summary data are required (includes plot and visit details, landform data, geographic coordinates, notes etc). Default is to include.
}
  \item{structural_summaries}{Whether site vegetation structural summaries are required.
}
  \item{veg.vouchers}{Whether vegetation vouchers data are requested - contains a complete set of species records for the plot determined by a herbarium plus ID numbers for silica-dried tissue samples.
}
  \item{veg.PI}{Whether point-intercept data are requested; includes data on substrate, plant species, growth form and height etc at each of (typically) 1010 points per plot.
}
  \item{basal.wedge}{Whether basal wedge data raw hits are required to enable calculation of basal area by species by plot.
}
  \item{soil_subsites}{Whether soil subsite information is required. This includes information on what soil and soil metagenomics samples were taken at nine locations across the plot and their identification barcode numbers.
}
  \item{soil_bulk_density}{Whether soil bulk density data are required.
}
  \item{soil_character}{Whether soil characterisation and sample ID data at 10 cm increments to a depth of 1 m are required.
}
  \item{plot_search}{Optional character string filter for selecting AusPlots based on wildcard/partial match to the \code{site_location_name}.
}
  \item{bounding_box}{An additional optional spatial filter for selecting AusPlots based on a rectangular box, in the format of e.g. c(xmin, xmax, ymin, ymax). AusPlots location data are are in longlat, therefore x is the longitude and y is the latitude of the box/extent object. e.g., c(120, 140, -30, -10).
}
  \item{herbarium_determination_search}{Optional character string filter for selecting AusPlots based on the presence of a genus or species as recorded in the "herbarium_determination" column. See details for search term settings.

}
\item{family_search}{Optional character string filter for selecting AusPlots based on the presence of a plant family as recorded in the 'family' column.  Will be NA if no family identification was possible. See details for search term settings.
}
\item{standardised_name_search}{Optional character string filter for selecting AusPlots based on the presence of a genus and/or species as recorded in the 'standardised_name' column. Will be NA if no genus rank (at minimum) was possible. See details for search term settings.
}
\item{dictionary}{Whether the metadata variable dictionary is required. Provides a table that describes many of the variables and values in each data frame.
}
}

\details{By default, the function will extract and compile a site information table including details of all available plot visits. Arguments allow the user to select sites by plot ID and visit codes, geographic bounding box, or species, and select which modules data are drawn from. Partial/wildcard site code matches are enabled in the \code{plot_search} argument, and further spatial filtering of sites from a master list is possible by using the 'bioregion_name', 'state', and spatial coordinate fields in the site table. Additional tables such as vegetation vouchers, point intercept modules and soil attributes can be requested by setting the desired modules to 'TRUE'.

The datasets returned by the function are used directly by further formatting and analysis functions in ausplotsR (such as \code{\link{species_table}} to generate a species by sites matrix for community analysis), making it simple to go from raw data extract to ecological analysis.
	
AusPlots surveys are identified by a plot ID called \code{site_location_name} and unique visit number, that are combined to identify a unique measurement for sites with repeat visits, identified in the extracted data tables as 'site_unique'.

"herbarium_determination_search", "family search", and "standardised_name_search" search terms are not case sensitive and do not require an exact taxonomic match (e.g. "Eucalyptus moderata", "Eucalyptus", and "euca" are all acceptable search terms). If \code{veg.vouch=TRUE}, veg.vouch will return a data frame that only includes voucher records that match the species or family search. If \code{veg.PI=TRUE}, veg.PI will return point intercept data for all plots where the species or family occurs. If \code{basal.wedge=TRUE}, veg.basal will return a dataframe that only includes the raw basal wedge data of records that match the species or family. For all other data.frames, get_ausplots will return data for all the plots where the species or family occurs. 

\code{herbarium determinations} are provided by state herbaria and are the most commonly used scientific names in the given state. However, scientific names may differ between states and with international databases due to disagreements on taxonomy/nomenclature. Herbarium determinations are made to the lowest possible taxonomic rank, which may be the family, genus, species, subspecies or variety.
    
\code{standardised_name_search} values are based on the \code{veg.PI$herbarium_determination} but have been matched to the accepted scientific name according to the Australian Plant Census (APC: https://www.anbg.gov.au/cpbr/program/hc/hc-APC.html) and the Australian Plant Name Index (APNI: https://www.anbg.gov.au/cpbr/program/hc/hc-APNI.html). The standardised name field therefore helps to ensure taxonomic consistency. \code{standardised_name_search} determinations are made to the lowest possible taxonomic rank, which may be the family, genus, species, subspecies or variety. It excludes any records that don't match the APC. Additional taxonomic fields based on the standardised_name are listed as separate columns in veg.PI, veg.vouch, and veg.basal data tables.  

The metadata variable dictionary returned by \code{dictionary=T} may not include all of the the variables and values in each data frame, but will be expanded over time.

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
   \item{$metadata.dictionary }{Data frame that lists and describes each variable and corresponding values in each data frame.}
  \item{$citation }{Auto-generated citation for the data extracted. Please cite ausplotsR and the TERN AusPlots data you use.}
}

\author{Greg Guerin, Andrew Tokmakoff, Tom Saleeba, Samantha Munroe
}

\seealso{
\code{\link{species_table}}
}
\examples{
\dontrun{
#get data for selected plots:
my.data <- get_ausplots(my.Plot_IDs=c("SATFLB0004", "QDAMGD0022", "NTASTU0002"), 
basal.wedge=TRUE, structural_summaries=TRUE)

#View module:
my.data$struct.summ

#Get data citation and extraction date info:
my.data$citation

#Get site info for all available plots:
my.data <- get_ausplots()

##Subset site records to plots with revisits only
#vector of plots with revisits
revisited_plots <- unique(my.data$site.info$site_location_name[
which(my.data$site.info$visit_number > 1)])

#subset site table to revisited plots
revisited_site.info <- my.data$site.info[
which(my.data$site.info$site_location_name %in% revisited_plots),]

#Get voucher records for "Eucalyptus moderata" using "herbarium_determination":
Eucalyptus_moderata <- get_ausplots(site_info=FALSE, veg.vouchers=TRUE, 
herbarium_determination_search="Eucalyptus moderata")

head(Eucalyptus_moderata$veg.vouch, 10)

#Get veg voucher records for all species in the family "Myrtaceae"
Myrtaceae <- get_ausplots(site_info=FALSE, veg.vouchers=TRUE, 
family_search="Myrtaceae")

head(Myrtaceae$veg.vouch,10)
}
}
