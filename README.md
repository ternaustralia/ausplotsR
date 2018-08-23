# ausplotsR
R package in development for specific data extraction, compilation and arrangement tasks for AusPlots data.

A set of self-contained functions will increasingly replace the original scripts.

When complete, users will be able to access data directly through the R package, with simple function calls to extract the data and merge them into species occurrence matrices for analysis or to calculate things like basal area and fractional cover.

To source functions directly, for example:

```
library(devtools)
source_url("https://github.com/GregGuerin/ausplotsR/blob/master/R/species_table.R")
``` 