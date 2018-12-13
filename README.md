# ausplotsR
R package for live extraction, preparation, visualisation and analysis of TERN AusPlots ecosystem monitoring data.

Through ausplotsR, users can directly access plot-based data on vegetation and soils across Australia, with simple function calls to extract the data and merge them into species occurrence matrices for analysis or to calculate things like basal area and fractional cover. Additional functionality will be added over time.

Click here for more information on [TERN](http://www.tern.org.au) and [AusPlots](http://www.ausplots.org).

ausplotsR requires the following packages: plyr, R.utils, simba, httr, jsonlite; ('Suggests' needed to build vignette if 'build_vignettes' is set to TRUE below: vegan, knitr, rmarkdown, goeveg).

ausplotsR can be installed directly from github using the devtools package, which must be installed first.
 
To install the package, use:

```
library(devtools)
install_github("ternaustralia/ausplotsR", build_vignettes = TRUE)
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

Please cite the ausplotsR package as:
```
Greg Guerin, Tom Saleeba and Andrew Tokmakoff (2018). ausplotsR: TERN AusPlots analysis package. R package version 1.0.
```

Authors: Greg Guerin, Tom Saleeba, Andrew Tokmakoff