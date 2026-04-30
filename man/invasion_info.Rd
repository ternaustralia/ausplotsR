\name{invasion_info}
\alias{invasion_info}
\title{Add invasion information to AusPlots records using a static AFA table}
\usage{
invasion_info(
  ausplots = NULL,
  afa_static,
  taxon_col = "herbarium_determination",
  keep_apcalign_cols = TRUE,
  keep_all = TRUE,
  remove_helper_cols = TRUE,
  clean_output = TRUE,
  ...
)
}
\arguments{
  \item{ausplots}{A data frame of plant records, or \code{NULL}. If \code{NULL},
  the function calls \code{get_ausplots(veg.vouchers = TRUE)} and uses the
  \code{veg.vouch} component.}

  \item{afa_static}{A data frame containing the harmonised Alien Flora of
  Australia dataset.}

  \item{taxon_col}{Character string giving the column in \code{ausplots} to use
  for taxonomic alignment. Defaults to \code{"herbarium_determination"}.}

  \item{keep_apcalign_cols}{Logical. If \code{TRUE}, retain columns returned by
  \pkg{APCalign}. If \code{FALSE}, keep only the original input columns plus the
  core aligned-name fields.}

  \item{keep_all}{Logical. If \code{TRUE}, return all joined columns. If
  \code{FALSE}, return a reduced set focused on invasion, declared, and
  threatened fields.}

  \item{remove_helper_cols}{Logical. If \code{TRUE}, remove internal helper
  columns used for matching.}

  \item{clean_output}{Logical. If \code{TRUE}, filter uncertain or indeterminate
  determinations where relevant and keep a simplified output table.}

  \item{...}{Additional arguments passed to \code{get_ausplots()} when
  \code{ausplots} is \code{NULL}.}
}
\description{
This function aligns plant names from AusPlots records with \pkg{APCalign}
and appends invasion-related information from a static Alien Flora of
Australia (AFA) dataset.
}
\details{
Matching is performed primarily using APC-aligned accepted names and, where
needed, a conservative binomial fallback. By default, the function retrieves
AusPlots voucher records and appends harmonised invasion status fields for
Australian states, mainland territories, and selected external territories,
together with national AFA status, GRIIS invasive status, Darwin Core-style
establishment fields, and declared and threatened status summaries.

Taxon names are first cleaned and aligned with \pkg{APCalign}. The function
then matches aligned names to the AFA dataset using a normalised text key
based on \code{accepted_name_usage}, with a binomial fallback where no exact
match is found.
}
\value{
A data frame containing the input records plus appended invasion, declared,
threatened, and establishment fields from the AFA dataset.
}
\seealso{
\code{\link{get_ausplots}}, \code{\link{species_list}}, \code{\link{species_table}}
}
\examples{
\dontrun{
afa <- get_afa_static()
x <- invasion_info(afa_static = afa)

vouch <- get_ausplots(veg.vouchers = TRUE)$veg.vouch
y <- invasion_info(
  ausplots = head(vouch, 100),
  afa_static = afa,
  keep_all = FALSE
)
}
}
\author{
  Irene Martin-Fores
}
