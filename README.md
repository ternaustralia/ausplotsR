# ausplotsR
[![](https://cranlogs.r-pkg.org/badges/ausplotsR)](https://cran.r-project.org/package=ausplotsR)

R package for live extraction, preparation, visualisation and analysis of TERN Ecosystem Surveillance monitoring data (AusPlots data).

*(See bottom of page for troubleshooting help)*

Through ausplotsR, users can now directly access plot-based data on vegetation and soils across Australia, with simple function calls to extract the data and merge them into species occurrence matrices for analysis or to calculate things like basal area and fractional cover.

The data have been collected by TERN’s Ecosystem Surveillance platform via field surveys and sampling across a national network of plots and transects. Follow the links for more information on the research infrastructure provided by the Terrestrial Ecosystem Research Network ([TERN](https://www.tern.org.au)), an Australian Government NCRIS-enabled project, and its [Ecosystem Surveillance platform](https://www.tern.org.au/tern-observatory/tern-ecosystem-surveillance/).

## Standardised names - an update

To provide plant taxonomy that is standardised nationally (due to some state-level differences in accepted taxa), herbarium determinations are currently mapped to a standard using "World Flora Online" (http://www.worldfloraonline.org/). However, this mapping has become out-dated and is no longer the best solution available.

We are therefore in the process of providing updated name standardisation to the Australian Plant Census (APC: https://www.anbg.gov.au/cpbr/program/hc/hc-APC.html) to improve waning coverage and fix matching errors. In the interim, by selecting herbarium determination to build tables, you will maximise coverage of species records in the database. Package documentation will be updated to explain the changes within a pending minor version update.

## New features coming soon in ausplotsR version 2

A new package version is under development that will feature a range of bug fixes as well as updated or additional functionality, including:
 1. New and improved standardised plant taxonomy as well as herbarium determinations.
 1. Reduced size of default data extraction due to the growing size of the database.
 1. Easier coding and visualisation of revisits.
 1. Enhanced search options including particular site visits and partial site name matches.

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

Authors: Greg Guerin, Tom Saleeba, Samantha Munroe, Irene Martín-Forés, Bernardo Blanco-Martin, Andrew Tokmakoff

# Troubleshooting

## These packages have more recent versions available
When you try to install AusplotsR, you might see a notice like the following.
```
These packages have more recent versions available.
Which would you like to update?

 1:   All
 2:   CRAN packages only
 3:   None
 4:   backports   (1.1.4  -> 1.1.5 ) [CRAN]
 5:   callr       (3.2.0  -> 3.4.2 ) [CRAN]
 ...
```
AusplotsR has a list of other packages, and their versions, that it needs to
work. R is being helpful and telling you that it can install the *newest* versions
of those packages rather than the versions that AusplotsR has asked for.

The safest choice is to select `3: None`, which means the exact versions
AusplotsR asks for will be installed. You're free to install newer versions of
packages if you would like but beware that AusplotsR *may* not work with these
newer packages.

## Rcmd.exe not found
We've seen this error when trying to install AusplotsR on Windows in RStudio.
```
Error in rethrow_call(c_processx_exec, command, c(command, args), stdin, :
Command 'C:/some/path/to/R/R-3.6.3/bin/x64/Rcmd.exe' not found @win/processx.c:98
```

It seems to be an issue with your R installation. There an official
[FAQ](https://cran.r-project.org/bin/windows/base/rw-FAQ.html#Rcmd-is-not-found-in-my-PATH_0021)
that seems related but the instructions on how to fix it aren't related to
RStudio. So we'll add our own here.

 1. make sure you have R itself installed. You can get the latest version from https://cran.rstudio.com/
 1. Open RStudio
 1. Open the Tools -> Global Options menu item
 1. Click the *Change* button for the `R version`
 1. Make sure it's set to `Use your machine's default version of R64 (64-bit)`
    ![screenshot showing how to configure RStudio to use the default R on your
    machine](./rstudio-config.png)
 1. Click OK
