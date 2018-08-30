# ausplotsR
R package for live extraction, preparation, visualisation and analysis of TERN AusPlots ecosystem monitoring data.

Through ausplotsR, users can directly access plot-based data on vegetation and soils across Australia, with simple function calls to extract the data and merge them into species occurrence matrices for analysis or to calculate things like basal area and fractional cover. Additional functionality will be added over time.

Click here for more information on [TERN](http://www.tern.org.au) and [AusPlots](http://www.ausplots.org).

ausplotsR requires the following packages: plyr, R.utils, simba, httr, jsonlite; ('Suggests' needed to build vignette: vegan, knitr, rmarkdown, goeveg).
 
To install the package, use:

```
library(devtools)
install_github("GregGuerin/ausplotsR", build_vignettes = TRUE)
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

Authors: Greg Guerin, Tom Saleeba, Andrew Tokmakoff