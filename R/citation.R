citation <- function () {
  packageName <- getPackageName()
  commitSha <- packageDescription(packageName, fields = 'RemoteSha', drop = TRUE)
  if (is.na(commitSha)) {
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

citation() # run on package load
