# ausplotsR
[![](https://cranlogs.r-pkg.org/badges/ausplotsR)](https://cran.r-project.org/package=ausplotsR)

R package for live extraction, preparation, visualisation and analysis of TERN Ecosystem Surveillance monitoring data (AusPlots data).

Through ausplotsR, users can now directly access plot-based data on vegetation and soils across Australia, with simple function calls to extract the data and merge them into species occurrence matrices for analysis or to calculate things like basal area and fractional cover.

The data have been collected by TERN’s Ecosystem Surveillance platform via field surveys and sampling across a national network of plots and transects. Follow the links for more information on the research infrastructure provided by the Terrestrial Ecosystem Research Network ([TERN](https://www.tern.org.au)), an Australian Government NCRIS-enabled project, and its [Ecosystem Surveillance platform](https://www.tern.org.au/tern-observatory/tern-ecosystem-surveillance/).

## New features in ausplotsR version 2.0

Update to the latest package version to make use of new and improved features:
 1. Visualise changes in parameters between revisits and easier coding of repeat visits.
 1. Standardised NVIS vegetation description fields for each survey visit.
 1. Updated implementation of tree Basal Area (BA) calculation as measure of species dominance where tree species are patchily distributed.
 1. Filter site downloads based on unique site/visit codes as well as partial code match/wildcard feature.
 1. Bug fixes and corrected plot order in ausplots_visual
 1. ETC TRAITS, INVASION, SAMPLE REQUESTS

# Using ausplotsR

ausplotsR is now available on [CRAN](https://cran.r-project.org/web/packages/ausplotsR/index.html), meaning it can be installed using the 'install packages' command or menu in an R or RStudio session.

ausplotsR requires the following packages: 'Depends': vegan, maps, mapdata; 'Imports': plyr, R.utils, simba, httr, jsonlite, sp, maptools, ggplot2, gtools, jose, betapart, curl; 'Suggests' (needed to build the package vignette if 'build_vignettes' is set to TRUE below): knitr, rmarkdown.

The most current ausplotsR can be installed directly from github to get the latest developments and patches using the devtools package, which must be installed first.
 
To install the package, use:

```
# if you have problems, see the troubleshooting section at the bottom of this document
library(devtools)
install_github("ternaustralia/ausplotsR", build_vignettes = TRUE, dependencies = TRUE)
```


To get started:

```
library(ausplotsR)
help(ausplotsR)
browseVignettes(package="ausplotsR")
```

To download AusPlots data, start with:

```
?get_ausplots
```

Or, to simply grab all vegetation point intercept and voucher data plus basic site info for all available plots, use:

```
library(ausplotsR)
my.ausplots.data <- get_ausplots()
names(my.ausplots.data)
```

## Citation

A suggested citation is automatically generated in the following format when you extract TERN AusPlots data via ausplotsR:

```
TERN ("year") AusPlots ecosystem surveillance monitoring dataset (URL: http://aekos.org.au/collection/adelaide.edu.au/ausplotsrangelands). Obtained via the ausplotsR R package (URL: https://github.com/ternaustralia/ausplotsR), accessed "day month year".
```

To print the citation of our package:
```R
citation('ausplotsR')
```

Please include appropriate citation in published papers/reports/theses that use the data and R functions.

Authors: Greg Guerin, Tom Saleeba, Samantha Munroe, Irene Martín-Forés, Bernardo Blanco-Martin, Andrew Tokmakoff