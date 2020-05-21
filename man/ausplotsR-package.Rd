\name{ausplotsR-package}
\alias{ausplotsR-package}
\alias{ausplotsR}
\docType{package}
\title{
Live extraction, preparation, visualisation and analysis of TERN AusPlots ecosystem monitoring data.
}
\description{
Through ausplotsR, users can directly access plot-based data on vegetation and soils across Australia, including physical sample barcode numbers, with simple function calls to extract the data and merge them into species occurrence matrices for analysis or to calculate things like basal area and fractional cover. Additional functionality will be added over time.
}
\details{
\tabular{ll}{
Package: \tab ausplotsR\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2020-05-11\cr
License: \tab GPL-3\cr
}
TERN AusPlots is a national field plot-based ecosystem surveillance monitoring method and dataset for Australia. A standardised national network of plots enables consistent ecological assessment and monitoring.

The AusPlots survey method is made up of a series of individual modules, which are described in full in the protocols manual (White et al. 2012). Data from the following modules can currently be accessed through ausplotsR: plot selection and layout; vascular plant vouchering; point intercept; basal area; structural summary; and soils. Field data are recorded directly onto mobile (tablet) devices before being stored in cloud-based server infrastructure.

Method summaries:

Plot layout: 1 ha (100 x 100 m) plots are permanently marked over a homogenous patch of terrestrial vegetation.

Soil sampling: soils are characterised in a pit at the SW corner of the plot to 1 m depth and 500 g samples are collected at 10 cm depth intervals; soil bulk density is measured; nine soil subsites are located across the plots and 500 g samples are taken at 10 cm depths to 30 cm. Additionallly, 200 g surface soil (~3 cm depth) samples are taken at the nine subsites and silica-dried for metagenomic analysis.

Structural summary: the dominant species in three vegetation strata (lower, middle and upper) are identified visually by the observer. The vegetation is then categorised into MVGs according to the Australian National Vegetation Information System.

Vascular plant vouchering: vascular plants within the plot are detected visually, with a herbarium voucher taken for each unique taxon, which is assigned a barcode and identified then permanently stored in a herbarium. Additionally, silica-dried material is sampled from each species for genetic and isotopic analysis, and five replicate samples are taken for dominant species.

Point intercept: 10 x 100 m long transects are laid out within the plot in a grid pattern. A staff with laser pointer and densitometer is used to record species, growth form and height plus substrate type every 1 m along the transects, resulting in a total of 1010 (10 x 101) point intercept hits for the plot.

Basal wedge: a basal wedge is used to determine the basal area of trees and shrubs at 1.3 m, with measures of hits per species taken at nine locations across the plot.

Additional modules in the AusPlots method not avaialable through this package include: full DGPS coordinate data (only the coordinates of the SW corner are provided), photo panoramas, Leaf Area Index. 


Acknowledging TERN AusPlots data in publications:

It is important that users of data extracted via ausplotsR acknowledge TERN, the AusPlots dataset and ausplotsR by including an appropriate citation in the Acknowledgments or References section of any published papers/reports/theses that make use of the data. To make this easy, a data citation is automatically generated when you extract data (see \code{get_ausplots}). It has the following format:


TERN ("year") AusPlots ecosystem surveillance monitoring dataset (URL: http://aekos.org.au/collection/adelaide.edu.au/ausplotsrangelands). Obtained via the ausplotsR R package (URL: https://github.com/ternaustralia/ausplotsR), accessed "day month year".

To cite the ausplotsR package itself and the functions it contains, enter: \code{citation('ausplotsR')}


Key functions in the ausplotsR package:

\code{get_ausplots} :  This function is the starting point for accessing data through the ausplotsR package for the following survey modules: Site information; Vegetation point intercept; Vegetation vouchers; Basal wedge; Vegetation structural summaries; Soils and soil samples information.

 \code{ausplots_visual} :  Quick demonstration graphics for AusPlots.
 
 \code{species_table} :  Species occurrence matrices for AusPlots.

 \code{growth_form_table} :  Plant growth form occurrence matrices for TERN AusPlots.

 \code{fractional_cover} :  Fractional cover based on TERN AusPlots data.

 \code{basal_area} :  Basal area of tree trunks in TERN AusPlots.
 
 \code{single_cover_value} :  Vegetation cover in TERN AusPlots by height and growth form subsets.

}
\author{
Greg Guerin, Tom Saleeba, Samantha Munroe, Bernardo Blanco-Martin, Irene Martin-Fores, Andrew Tokmakoff

Maintainer: Greg Guerin <ggueri01@gmail.com>
}
\references{White, A., Sparrow, B., Leitch, E., Foulkes, J., Flitton, R., Lowe, A. J., & Caddy-Retalic, S. (2012). AUSPLOTS rangelands survey protocols manual. University of Adelaide Press.

Tokmakoff, A., Sparrow, B., Turner, D., & Lowe, A. (2016). AusPlots Rangelands field data collection and publication: Infrastructure for ecological monitoring. Future Generation Computer Systems, 56, 537-549.

Guerin, G. R., Sparrow, B., Tokmakoff, A., Smyth, A., Leitch, E., Baruch, Z., & Lowe, A. J. (2017). Opportunities for integrated ecological analysis across inland Australia with standardised data from Ausplots Rangelands. PloS ONE, 12(1), e0170137.
 
}

\keyword{ package }
\seealso{

\code{\link{get_ausplots}}
}
\examples{
#load package:
library(ausplotsR)

#get data for selected plots:
my.data <- get_ausplots(my.Plot_IDs=c("SATFLB0004", "QDAMGD0022", "NTASTU0002"),
	veg.PI=FALSE, basal.wedge=TRUE, structural_summaries=TRUE)

#view module:
my.data$struct.summ

#get site info, veg vouchers and veg point intercept for all available plots:
\dontrun{get_ausplots()} 

#get a species by sites table:
my.data <- get_ausplots(my.Plot_IDs=c("SATFLB0004", "QDAMGD0022", "NTASTU0002"))

species_table(my.data$veg.PI, m_kind="percent_cover", cover_type="PFC")

}
