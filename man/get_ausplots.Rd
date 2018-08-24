\name{get_ausplots}
\alias{get_ausplots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract AusPlots data for sites, soils and vegetation
%%  ~~function to do ... ~~
}
\description{This function is the starting point for accessing data through the ausplotsR package.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_ausplots(my.Plot_IDs="none", site_info=TRUE, structural_summaries=FALSE, veg.vouchers=TRUE, veg.PI=TRUE, basal.wedge=FALSE, soil_subsites=FALSE, soil_bulk_density=FALSE, soil_character=FALSE, bounding_box="none", species.level.taxonomy=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my.Plot_IDs}{Optional character vector of AusPlots plot IDs to request data for specific set of plots.
%%     ~~Describe \code{my.Plot_IDs} here~~
}
  \item{site_info}{Whether site summary data are required (includes plot and visit details, landform data, coordinates, notes etc). Default is to include.
%%     ~~Describe \code{site_info} here~~
}
  \item{structural_summaries}{Whether site vegetation structural summaries are required.
%%     ~~Describe \code{structural_summaries} here~~
}
  \item{veg.vouchers}{Whether vegetation vouchers data are requested - contains a complete set of species records for the plot determined by a herbarium. Default is to include.
%%     ~~Describe \code{veg.vouchers} here~~
}
  \item{veg.PI}{Whether point intercept data are requested; includes data on substrate, plant species, growth form and height etc at each of (typically) 1010 points per plot. Default is to include.
%%     ~~Describe \code{veg.PI} here~~
}
  \item{basal.wedge}{Whether basal wedge data raw hits are required to enable calculation of basal area by species by plot.
%%     ~~Describe \code{basal.wedge} here~~
}
  \item{soil_subsites}{Whether soil subsite information is required.
%%     ~~Describe \code{soil_subsites} here~~
}
  \item{soil_bulk_density}{Whether soil bulk density data are required.
%%     ~~Describe \code{soil_bulk_density} here~~
}
  \item{soil_character}{Whether soil characterisation data are required.
%%     ~~Describe \code{soil_character} here~~
}
  \item{bounding_box}{An additional optional spatial filter for selecting AusPlots based on a rectangular box, in the format of e.g. c(xmin, xmax, ymin, ymax). AusPlots location data are are in longlat, therefore x is the longitude and y is the latitude of the box/extent object. e.g., c(120, 140, -30, -10). Possible extensions to spatial filtering of sites are the ability to extract sites via IBRA regions, political boundaries or user-provided shapefile.
%%     ~~Describe \code{bounding_box} here~~
}
  \item{species.level.taxonomy}{Placeholder argument for future addition of option to retrieve/re-format the species data to species level, i.e., excluding subspecies, varieties etc.
%%     ~~Describe \code{species.level.taxonomy} here~~
}
}
\details{By default, the function will extract and compile data from the vegetation vouchers and point intercept modules over all available plots and provide a basic site information table. Arguments allow the user to select sites by ID numbers or geographic bounding box and select which modules data are drawn from. The datasets returned by the function are used directly by further formatting and analysis functions in ausplotsR (such as \code{\link{species_table}} to generate a species by sites matrix for community analysis), making it simple to go from raw data extract to ecological analysis.
	
	AusPlots surveys are identified by a plot ID and unique visit number, that are combined to identify a unique measurement for sites with repeat visits.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a list with following elements, depending on the AusPlots modules requested in the function call.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
  \item{$site.info }{Data frame with basic site information including location.}
  \item{$struct.summ }{Data frame with vegetation structural summaries for each plot.}
  \item{$veg.vouch }{Data frame with rows for each voucher.}
  \item{$veg.PI }{Data frame with individual point intercept data.}
  \item{$veg.basal }{Data frame with compiled raw basal wedge hit data.}
  \item{$soil.sub }{Data frame with details of soil subsites within each plot.}
  \item{$soil.bulk }{Data frame with raw bulk density data from each plot.}
  \item{$soil.char }{Data frame with soil characterisation data from each plot.}
}
\references{n/a
%% ~put references to the literature/web site here ~
}
\author{Greg R. Guerin, Andrew Tokmakoff, Tom Saleeba
%%  ~~who you are~~
}
\note{n/a
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{species_table}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#get data for selected plots:
my.data <- get_ausplots(my.Plot_IDs=c("SATFLB0004", "QDAMGD0022", "NTASTU0002"), veg.PI=FALSE, basal.wedge=TRUE, structural_summaries=TRUE)

#view module:
my.data$struct.summ

#get site info, veg vouchers and veg point intercept for all available plots:
\dontrun{get_ausplots()} 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
