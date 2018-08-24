\name{ausplotsR}
\alias{ausplotsR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{R package for live extraction, preparation, visualisation and analysis of TERN AusPlots ecosystem monitoring data.
%%  ~~function to do ... ~~
}
\description{Through ausplotsR, users can directly access plot-based data on vegetation and soils across Australia, with simple function calls to extract the data and merge them into species occurrence matrices for analysis or to calculate things like basal area and fractional cover. Additional functionality will be added over time.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{See individual functions.

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{get_ausplots}{This function is the starting point for accessing data through the ausplotsR package for the following survey modules: Site information; Vegetation point intercept; Vegetation vouchers; Basal wedge; Vegetation structural summaries; Soils and soil samples information.
%%     ~~Describe \code{my.Plot_IDs} here~~
}
  \item{species_table}{Species occurrence matrices for AusPlots.
%%     ~~Describe \code{site_info} here~~
}
  \item{growth_form_table}{Plant growth form occurrence matrices for TERN AusPlots.
%%     ~~Describe \code{structural_summaries} here~~
}
  \item{fractional_cover}{Fractional cover based on TERN AusPlots data.
%%     ~~Describe \code{veg.vouchers} here~~
}
  \item{basal_area}{Basal area of tree trunks in TERN AusPlots.
%%     ~~Describe \code{veg.PI} here~~
}
}
\details{Add details
%%  ~~ If necessary, more details than the description above ~~
}
\value{Add details of outputs.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
