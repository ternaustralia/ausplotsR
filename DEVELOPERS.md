## Loading development code in R
When you're hacking on this package locally, you can test it by installing from source:

  1. change into the repo's root directory
      ```bash
      cd /path/to/git/ausplotsR/
      ```
  1. start R
      ```bash
      R
      ```
  1. make sure `devtools` and `roxygen2` are installed, you only need to do this once
      ```R
      install.packages("devtools")
      install.packages("roxygen2")
      ```
  1. use the `devtools::load_all` ([doco](https://rdrr.io/cran/devtools/man/load_all.html)) function to load the package from source:
      ```R
      devtools:load_all()
      ```
  1. if you make code changes, re-run `devtools::load_all()` to reload.


## Override the API URL

The `load_all()` function defaults to exporting everything so you can access all package private functions to test them.

You might also want to change the URL of the API that is used (to your local machine?). To do so, we need to set an R option:

```R
devtools::load_all()
options("ausplotsR_api_url" = "http://localhost:30000")
#...continue to call functions
```

You can check the current setting of the API URL with:

```R
getOption("ausplotsR_api_url")
```
