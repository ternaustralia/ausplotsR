citation_ausplotsr <- function () {
  packageName <- getPackageName()
  commitSha <- packageDescription(packageName, fields = 'RemoteSha', drop = TRUE)
  if (is.na(commitSha)) {
    # with older versions of devtools (pre 2.0 maybe?) we won't see the commitSha during install, but it works after
    commitSha <- '(not installed from git)'
  }
  version <- packageDescription(packageName, fields = 'Version', drop = TRUE)
  cat(
    'AusplotsR Citation:\n',
    '  Greg Guerin, Tom Saleeba and Andrew Tokmakoff (2018). ausplotsR: TERN AusPlots analysis package. R package version ',
    version,
    ' commit=',
    commitSha,
    ".\n",
    sep=""
  )
}

citation_ausplotsr() # run on package install/build
