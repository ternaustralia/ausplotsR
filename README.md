# ausplotsR
R package for live extraction, preparation, visualisation and analysis of TERN AusPlots ecosystem monitoring data.

Through ausplotsR, users can now directly access plot-based data on vegetation and soils across Australia, with simple function calls to extract the data and merge them into species occurrence matrices for analysis or to calculate things like basal area and fractional cover. Additional functionality will be added over time.

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

## Citation
To print the citation of our package:
```R
citation('ausplotsR')
```

## Repeatability with older versions of the package
If you need to install an older version of the package for repeatability, you can do so by supplying the specific
version to the `install_github` call. The version to install can be obtained from the citation string you obtained when
you first used the package (see above).

As an example, the output from the citation function call might look like:
```
... R package version 1.0 commit SHA=559e0eb77ca3d42a7276351695db42331ef170b4.
```

The piece of information we need is the commit ID/SHA, which in this example is `559e0eb77ca3d42a7276351695db42331ef170b4`.
We would then use this to install this specific version of the package with:
```R
install_github("ternaustralia/ausplotsR", build_vignettes = TRUE, ref = '559e0eb77ca3d42a7276351695db42331ef170b4')
```

Authors: Greg Guerin, Tom Saleeba, Andrew Tokmakoff
