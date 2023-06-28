# ausplotsR
[![](https://cranlogs.r-pkg.org/badges/ausplotsR)](https://cran.r-project.org/package=ausplotsR)

[![](https://cranlogs.r-pkg.org/badges/grand-total/ausplotsR)]

R package for live extraction, preparation, visualisation and analysis of TERN Ecosystem Surveillance monitoring data (AusPlots data).

Through ausplotsR, users can now directly access plot-based data on vegetation and soils across Australia, with simple function calls to extract the data and merge them into species occurrence matrices for analysis or to calculate things like basal area and fractional cover.

The data  and open access under Creative Commons – Attribution 4.0 International (CC BY 4.0), and have been collected by TERN’s Ecosystem Surveillance platform via field surveys and sampling across a national network of plots and transects. Follow the links for more information on the research infrastructure provided by the Terrestrial Ecosystem Research Network ([TERN](https://www.tern.org.au)), an Australian Government NCRIS-enabled project, and its [Ecosystem Surveillance platform](https://www.tern.org.au/tern-observatory/tern-ecosystem-surveillance/).

## Update to Version 2 to work with new features and settings

The latest package version features a range of bug fixes as well as updated or additional functionality and insome cases altered data extracts and settings, including:
 1. New and improved standardised plant taxonomy matching and fields as well as original herbarium determinations.
 1. Reduced size of default data extraction due to the growing size of the database.
 1. Easier coding, ordering and visualisation of revisits.
 1. Enhanced search options including particular site visits and partial site name matches.
 
## Future updates to Version 2

Pending minor versions of the package will enable extraction of two additional data tables presenting species-level traits (e.g., photosynthetic pathway, invasion status) and survey-level community indices (e.g., species abundance distribution, proportional abundance by photosynthetic pathway, community temperature index).

# Using ausplotsR

ausplotsR is now available on [CRAN](https://cran.r-project.org/web/packages/ausplotsR/index.html), meaning it can be installed using the 'install packages' command or menu in an R or RStudio session.

ausplotsR requires the following packages as 'Imports': mapdata, vegan, plyr, R.utils, httr, jsonlite, ggplot2, gtools, jose, betapart, curl, r2r, stringr; 'Suggests' (needed to build the package vignette if 'build_vignettes' is set to TRUE below): knitr, markdown, rmarkdown.

The most current development version of ausplotsR can be installed directly from github using the devtools package, which must be installed first.
 
To install the package, use:

```
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

Or, to simply grab basic site and visit info for all available plots, use:

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
