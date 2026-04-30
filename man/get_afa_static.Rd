\name{get_afa_static}
\alias{get_afa_static}
\title{Fetch the Alien Flora of Australia (AFA) dataset from the AusPlots API}
\description{
  Retrieves the current harmonised Alien Flora of Australia (AFA) invasion
  status table from the AusPlots REST API (\code{/afa_static}) and returns it
  as a data frame with the internal column schema expected by
  \code{\link{invasion_info}}.
}
\usage{
  get_afa_static()
}
\value{
  A data frame with one row per taxon. Columns include harmonised invasion
  status fields for all Australian states and territories
  (\code{status_ACT_harmonised}, \code{status_NSW_harmonised}, etc.),
  external territory status fields, national AFA status
  (\code{unified_AFA_status_continental},
  \code{unified_AFA_status_all_territories}), GRIIS invasive status
  (\code{griis_is_invasive}), Darwin Core-style establishment fields
  (\code{establishment_means}, \code{degree_of_establishment}), and
  additional detail columns added as \code{NA} where not supplied by the API
  (\code{declared_state_details}, \code{declared_national_details},
  \code{threatened_state_details}, \code{threatened_epbc_details},
  \code{family}).
}
\examples{
\dontrun{
afa <- get_afa_static()
head(afa)

# Use with invasion_info()
vouch <- get_ausplots(veg.vouchers = TRUE)$veg.vouch
result <- invasion_info(ausplots = vouch, afa_static = afa)
}
}
\seealso{
  \code{\link{invasion_info}}, \code{\link{get_ausplots}}
}
\author{
  Irene Martin-Fores
}
