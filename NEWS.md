# ausplotsR News

# Changes in version 2.0.0

## Breaking changes

* Default data download in `get_ausplots` now only includes `$site.info` table (and not vegetation point intercept - `$veg.PI` - nor vegetation vouchers - `$veg.vouch` as per previous default) due to increasing size of the dataset. Users can still select to extract vegetation point-intercept or vouchers as well as all the other modules available in ausplotsR. All downloaded data objects include a data `$citation`.

* Consistent implementation of logical arguments in functions, meaning some existing calls using character strings as selections will no longer work. 

## New features and data

* New and improved name standardisation fields according to the Australian Plant Census and Australian Plant Name Index. Original herbarium determinations remain available.

* Filter site downloads in `get_ausplots` based on unique site/visit codes as well as partial code match/wildcard feature. Previously, only the `site_location_name` code could be used to filter by text match.

* Improved handling and interpretation of revisit data:
  * Survey visit dates are now presented as R-readable dates and the sites table is appended with revisit numbers for each plot (i.e., 1, 2, 3 and so on) to simplify identification of revisits and their chronological order.
  * Visualisation of vegetation change (species composition, diversity or input community indices) along a sequence of visits to a plot with new function `ausplots_trajectories`.

* Added a `NEWS.md` file to track changes to the package.

## Minor improvements

* Updated basal area calculations where per species values are requested in 'basal_area' to better reflect whole site species means (sum of species means = site mean).

## Bug fixes

* Corrected plot order in ausplots_visual. Some plots and sets of visits had missing calculations that upset order and formatting of plots. This has been corrected.

* Various minor corrections to avoid nuisance messages.

## Installation

* Package tested for compatibility with `R` v4 as it was originally built in v3.

* Adjustments to required package imports.