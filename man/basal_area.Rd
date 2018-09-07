\name{basal_area}
\alias{basal_area}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Basal area of tree trunks in TERN AusPlots
%%  ~~function to do ... ~~
}
\description{Calculates basal area (or number of basal wedge hits) for each plot, using the raw basal wedge data returned from \code{\link{get_ausplots}}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
basal_area(veg.basal, by.spp=FALSE, by.hits=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{veg.basal}{Raw basal wedge data generated from \code{\link{get_ausplots}}.
%%     ~~Describe \code{my.Plot_IDs} here~~
}
  \item{by.spp}{Set to TRUE to get values per individual species in plots rather than combined values per plot.
%%     ~~Describe \code{site_info} here~~
}
  \item{by.hits}{Set to TRUE to return the number of individual basal wedge hits rather than basal area per se.
%%     ~~Describe \code{structural_summaries} here~~
}
}
\details{n/a
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a data frame with rows for plots (or species by plots) and basal area (m2/ha) or hit scores.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{n/a
%% ~put references to the literature/web site here ~
}
\author{Greg R. Guerin
%%  ~~who you are~~
}
\note{n/a
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{get_ausplots}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#get basal wedge data for selected plots:
my.data <- get_ausplots(my.Plot_IDs=c("SATFLB0004", "QDAMGD0022", "NTASTU0002"),
	veg.vouchers=FALSE, site_info=FALSE, veg.PI=FALSE, basal.wedge=TRUE)

#calculate basal area:
basal_area(my.data$veg.basal)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
