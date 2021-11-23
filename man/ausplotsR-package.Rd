\name{ausplotsR-package}
\alias{ausplotsR-package}
\alias{ausplotsR}
\docType{package}
\title{
Live extraction, preparation, visualisation and analysis of TERN AusPlots ecosystem monitoring data.
}
\description{
Through ausplotsR, users can directly access plot-based data on vegetation and soils across Australia, including physical sample barcode numbers. Simple function calls extract the data and merge them into species occurrence matrices for downstream analysis or calculate things like basal area and fractional cover.

Warning: ausplotsR was built in R version 3.

}
\details{
\tabular{ll}{
Package: \tab ausplotsR\cr
Type: \tab Package\cr
Version: \tab 1.2.7\cr
Date: \tab 2021-11-23\cr
License: \tab GPL-3\cr
}
TERN AusPlots is a national field plot-based ecosystem surveillance monitoring method and dataset for Australia. A standardised national network of plots enables consistent ecological assessment and monitoring. The data have been collected across a national network of plots and transects by the Terrestrial Ecosystem Research Network (TERN - \href{https://www.tern.org.au}{https://www.tern.org.au}), an Australian Government NCRIS-enabled project, and its Ecosystem Surveillance platform (\href{https://www.tern.org.au/tern-observatory/tern-ecosystem-surveillance/}{https://www.tern.org.au/tern-observatory/tern-ecosystem-surveillance/}).

The AusPlots survey method is made up of a series of individual modules, which are described in full in the protocols manual (White et al. 2012, Sparrow et al. 2020). Data from the following modules can currently be accessed through ausplotsR: plot selection and layout; vascular plant vouchering; point intercept; basal area; structural summary; and soils. Field data are recorded directly onto mobile (tablet) devices before being stored in cloud-based server infrastructure. \cr

\emph{Method summaries:}

\emph{Plot layout}: 1 ha (100 x 100 m) plots are permanently marked over a homogenous patch of terrestrial vegetation.

\emph{Soil sampling}: soils are characterised in a pit at the SW corner of the plot to 1 m depth and 500 g samples are collected at 10 cm depth intervals; soil bulk density is measured; nine soil subsites are located across the plots and 500 g samples are taken at 10 cm depths to 30 cm. Additionally, 200 g surface soil (~3 cm depth) samples are taken at the nine subsites and silica-dried for metagenomic analysis.

\emph{Structural summary}: the dominant species in three vegetation strata (lower, middle and upper) are identified visually by the observer. The vegetation is then categorised into MVGs according to the Australian National Vegetation Information System (NVIS 2017).

\emph{Vascular plant vouchering}: all vascular plants within the plot are detected visually, with a herbarium voucher taken for each unique taxon, which is assigned a barcode and identified then permanently stored in a herbarium. Additionally, silica-dried material is sampled from each species for genetic and isotopic analysis, and five replicate samples are taken for dominant species. Bryophytes are sometimes recorded opportunistically (e.g. when prominent) but full diversity is not systematically collected.

\emph{Point-intercept} : 10 x 100 m long transects are laid out within the plot in a grid pattern. A staff with laser pointer and densiometer is used to record species, growth form and height plus substrate type every 1 m along the transects, resulting in a total of 1010 (10 x 101) point-intercept hits for the plot.

\emph{Basal wedge} : a basal wedge is used to determine the basal area of trees and shrubs at 1.3 m, with measures of hits per species taken at nine locations across the plot.

Additional modules in the AusPlots method not available through this package include: full DGPS coordinate data (only the coordinates of the SW corner are provided here), photo panoramas, Leaf Area Index. \cr

\emph{Acknowledging TERN AusPlots data in publications:}

It is important that users of data extracted via ausplotsR acknowledge TERN, the AusPlots dataset and ausplotsR by including an appropriate citation in the Acknowledgments or References section of any published papers/reports/theses that make use of the data. To make this easy, a data citation is automatically generated when you extract data (see \code{\link{get_ausplots}}). It has the following format:


TERN ("year") AusPlots ecosystem surveillance monitoring dataset 
(URL: http://aekos.org.au/). Obtained via the ausplotsR R package (URL: https://github.com/ternaustralia/ausplotsR), accessed "day month year".

To cite the ausplotsR package itself and the functions it contains, enter: \code{citation('ausplotsR')}, or see links to citable archived versions with DOI at \href{https://github.com/ternaustralia/ausplotsR}{https://github.com/ternaustralia/ausplotsR}. \cr
 
\emph{Key functions in the ausplotsR package:}

\code{\link{get_ausplots}} :  This function is the starting point for accessing data through the ausplotsR package for the following survey modules: Site information; Vegetation point intercept; Vegetation vouchers; Basal wedge; Vegetation structural summaries; Soils and soil samples information.

 \code{\link{ausplots_visual}} :  Quick maps and demonstration graphics for AusPlots.
 
 \code{\link{species_table}} :  Species occurrence matrices for AusPlots.

 \code{\link{growth_form_table}} :  Plant growth form occurrence matrices for TERN AusPlots.

 \code{\link{fractional_cover}} :  Fractional cover based on TERN AusPlots data.

 \code{\link{basal_area}} :  Basal area of tree trunks in TERN AusPlots.
 
 \code{\link{single_cover_value}} :  Vegetation cover in TERN AusPlots by height and growth form subsets.

 \code{\link{species_list}} :  Simplified plant species lists for TERN AusPlots based on verified vouchers.
 
 \code{\link{optim_species}} :  Species accumulation optimisation for community data including TERN AusPlots.
}

\author{
Greg Guerin, Tom Saleeba, Samantha Munroe, Bernardo Blanco-Martin, Irene Martin-Fores, Andrew Tokmakoff

Maintainer: Greg Guerin <ggueri01@gmail.com>
}
\references{Guerin, G. R., Sparrow, B., Tokmakoff, A., Smyth, A., Leitch, E., Baruch, Z., & Lowe, A. J. (2017). Opportunities for integrated ecological analysis across inland Australia with standardised data from Ausplots Rangelands. \emph{PLoS ONE}, \strong{12}, e0170137.

Munroe, S., Guerin, G.R., Saleeba, T., Martin-Fores, M., Blanco-Martin, B., Sparrow, B. & Tokmakoff, A. (2021). ausplotsR: An R package for rapid extraction and analysis of vegetation and soil data collected by Australia’s Terrestrial Ecosystem Research Network. \emph{Journal of Vegetation Science}, \strong{32}, e13046.

NVIS Technical Working Group (2017). Australian Vegetation Attribute Manual: National Vegetation Information System, Version 7.0. Department of the Environment and Energy, Canberra. Prep by Bolton, M.P., deLacey, C. and Bossard, K.B. (Eds)
 
Sparrow, B., Foulkes, J., Wardle, G., Leitch, E., Caddy-Retalic, S., van Leeuwen, S., Tokmakoff, A., Thurgate, N., Guerin, G.R. and Lowe, A.J. (2020). A vegetation and soil survey method for surveillance monitoring of rangeland environments. \emph{Frontiers in Ecology and Evolution}, \strong{8}, 157.

Tokmakoff, A., Sparrow, B., Turner, D., & Lowe, A. (2016). AusPlots Rangelands field data collection and publication: Infrastructure for ecological monitoring. \emph{Future Generation Computer Systems}, \strong{56}, 537-549.

White, A., Sparrow, B., Leitch, E., Foulkes, J., Flitton, R., Lowe, A. J., & Caddy-Retalic, S. (2012). AUSPLOTS rangelands survey protocols manual. University of Adelaide Press.
}

\seealso{

\code{\link{get_ausplots}}
}
\examples{
\dontrun{
#load package:
library(ausplotsR)

#get data for selected plots:
my.data <- get_ausplots(my.Plot_IDs=c("SATFLB0004", "QDAMGD0022", "NTASTU0002"),
	veg.PI=FALSE, basal.wedge=TRUE, structural_summaries=TRUE)

#view module:
my.data$struct.summ

#get site info, veg vouchers and veg point intercept for ALL available plots:
get_ausplots() 

#get a species by sites table:
my.data <- get_ausplots(my.Plot_IDs=c("SATFLB0004", "QDAMGD0022", "NTASTU0002"))

species_table(my.data$veg.PI, m_kind="percent_cover", cover_type="PFC", species_name="SN")
}
}
