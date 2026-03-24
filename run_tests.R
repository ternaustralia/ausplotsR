# Auto-installs missing packages and runs all tests

pkgs <- c("devtools", "testthat", "mockery")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install)

setwd(dirname(sys.frame(1)$ofile))  # set wd to this script's directory

suppressWarnings(devtools::load_all("."))
devtools::test(".")