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

## Accessing unpublished data
By default the public (unauthorised users) can only access site visits that are marked as *published* in the database.

If you authorise yourself, you can access these unpublished records. To do so:

  1. load the ausplotsR library like normal
  1. set your credentials by running
      ```R
      set_auth('somerole', 'somesecret')
      ```
  1. now all queries you perform will include unpublished visit data
  1. to return to only querying published data, run:
      ```R
      unset_auth()
      ```

The authorisation will expire. If you leave your R session open for a really long time, you might see an error like:
```
Error in .ausplots_api(path, query) : Unauthorized (HTTP 401).
```

If this is the case, re-run the `set_auth()` function and things should start working again.

