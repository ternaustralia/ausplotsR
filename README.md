# ausplotsR
R package for live extraction, preparation, visualisation and analysis of TERN AusPlots ecosystem monitoring data.

Through ausplotsR, users can directly access plot-based data on vegetation and soils across Australia, with simple function calls to extract the data and merge them into species occurrence matrices for analysis or to calculate things like basal area and fractional cover. Additional functionality will be added over time.

To install the package, use:

```
library(devtools)
install_github("gregguerin/ausplotsR")
``` 

To download AusPlots data, start with:

```
?get_ausplots
```