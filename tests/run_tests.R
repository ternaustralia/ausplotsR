# Auto-installs missing packages and runs all tests

pkgs <- c("devtools", "testthat", "mockery", "FD", "dplyr")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) {
  install.packages(to_install, type = "binary", ask = FALSE)
  # fallback to source if binary unavailable
  still_missing <- to_install[!sapply(to_install, requireNamespace, quietly = TRUE)]
  if (length(still_missing)) install.packages(still_missing, ask = FALSE)
}

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", ask = FALSE)
}
if (!requireNamespace("adiv", quietly = TRUE)) {
  BiocManager::install("adiv", ask = FALSE, update = FALSE, type = "binary")
  if (!requireNamespace("adiv", quietly = TRUE)) {
    install.packages("RcppEigen")
    BiocManager::install("adiv", ask = FALSE, update = FALSE, force = TRUE)
  }
}

setwd(dirname(sys.frame(1)$ofile))  # set wd to ausplotsR/tests/

devtools::load_all("..")
devtools::test("..")