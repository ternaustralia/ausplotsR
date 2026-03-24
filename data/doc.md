# Internal Datasets

This package bundles two static `.rda` datasets under `data/`.

---

## `NVIS.rda`

**Object name:** `NVIS`  
**Used in:** `R/get_nvis.R` → `get_nvis()`

### Description
A static lookup table linking AusPlots site visit identifiers (`site_unique`) to
vegetation classifications derived from the National Vegetation Information System (NVIS).
One row per site visit.

### Columns
| Column | Description |
|---|---|
| `site_unique` | Plot visit identifier (e.g. `SATFLB0004-53705`) |
| `class` | NVIS Level 1 — dominant growth form class (e.g. Forest) |
| `structural_formation` | NVIS Level 2 — cover + height + growth form |
| `nvis_code` | Structural formation code |
| `broad_floristic_formation` | NVIS Level 3 — dominant genus + structure |
| `sub_formation` | NVIS Level 4 — multi-stratum genera + structure |
| `association` | NVIS Level 5 — up to 3 dominant species across strata |
| `sub_association` | NVIS Level 6 — up to 5 dominant species across strata |
| `vegetation_community_l5` | Plain-language Level 5 community description |
| `vegetation_community_l6` | Plain-language Level 6 community description |
| `upper_average_height_metre` | Average canopy height from point intercept hits (>2 m) |

### How it is used
`get_nvis(site_unique)` joins this table to the user-requested site visits and returns
the matching rows. If no filter is provided, the full table is returned.

### Source
NVIS classifications were assigned by TERN ecologists applying the NVIS Technical
Working Group rules to AusPlots field survey data. This dataset cannot be regenerated
from the ausplots API — it must be sourced from TERN directly.

---

## `trait_data_pp.rda`

**Object name:** `trait_data_pp`  
**Used in:**
- `R/species_traits.R` → `species_traits()`
- `R/community_traits.R` → `community_traits()`

### Description
A static species-level functional trait table. One row per species.
All numeric trait values are log-transformed. Species names are aligned to the
Australian Plant Census (APC) using the APCalign package (applied offline during
dataset preparation, not at runtime).

### Columns
| Column | Type | Description |
|---|---|---|
| `species_name` | character | APC-aligned species name (join key) |
| `family_name` | character | Plant family |
| `leaf_length` | numeric (log) | Leaf length (mm) |
| `leaf_width` | numeric (log) | Leaf width (mm) |
| `leaf_area` | numeric (log) | Leaf area (mm²) |
| `leaf_mass_per_area` | numeric (log) | Leaf mass per area / LMA (g/m²) |
| `max_height` | numeric (log) | Maximum plant height (m) |
| `seed_length` | numeric (log) | Seed length (mm) |
| `seed_dry_mass` | numeric (log) | Seed dry mass (mg) |
| `photosynthetic_pathway` | character | C3, C4, CAM, C3-CAM, C4-CAM |
| `d13C` | numeric (log) | Carbon isotope discrimination |

### How it is used

**`species_traits()`** — species-level trait lookup  
Takes a species list (from `ausplots$veg.vouch`, a vector, or a data frame),
removes genus-only names, then left-joins to `trait_data_pp` on `species_name`.
Returns a data frame of species with their functional trait values.

**`community_traits()`** — plot-level community metrics  
1. Builds an abundance matrix (plots × species) from `veg.PI` point-intercept hits
2. Joins species to `trait_data_pp` to get trait values
3. Computes **Community Weighted Means (CWMs)** for numeric traits
4. Computes **photosynthetic pathway proportions** (C3 vs C4) per plot
5. Uses `FD` package to compute functional diversity (FRic, FEve, FDis, RaoQ)
6. Uses `adiv` package to compute functional redundancy and uniqueness

### Source
Trait values sourced from an external functional trait database (e.g. AusTraits).
This dataset cannot be generated from the ausplots API — it requires a separate
trait data source.