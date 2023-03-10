# ausplotsR News

# Changes in version 2.0.0

## Breaking changes

* Default data download in `get_ausplots` now only includes `site_info` table (and not vegetation point intercept - veg.PI - nor vegetation vouchers - veg.vouch) due to increasing size of the dataset. Users can still select to extract vegetation point-intercept or vouchers as well as all the other modules available in ausplotsR.

## New features and data

* New and improved name standardisation to the Australian Plant Census and Australian Plant Name Index.

* Filter site downloads in `get_ausplots` based on unique site/visit codes as well as partial code match/wildcard feature. Previously, only the `site_location_name` code could be used to filter by text match.

* Improved handling of revisit data:
  * Survey visit dates are now presented as R-readable dates and the sites table is appended with revisit numbers for each plot (i.e., 1, 2, 3 and so on) to simplify identification of revisits and their chronological order.
  * Visualisation of changes along a sequence of visits to a plot.

* Added a `NEWS.md` file to track changes to the package.

## Minor improvements

* 

## Bug fixes

* Corrected plot order in ausplots_visual. Some plots and sets of visits had missing calculations that upset order and formatting of plots. This have been corrected.

## Installation

Package is now tested for compatibility with `R` v4. 