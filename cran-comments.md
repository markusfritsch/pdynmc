## Test environments
local Windows 10 x64 install: R 4.0.5, R 4.1.0, and R-devel (2021-08-12 r80748)
travis-ci: ubuntu 16.04.6 LTS, R 3.6.3 R 4.0.0 and R-devel (2020-05-06 r78370)
winbuilder: R 4.0.5, R 4.1.0, and R-devel (2021-08-09 r80724)


## R CMD check results
There were no ERRORs, WARNINGs.

There was 1 NOTE during the checks with winbuilder:

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1912775
    From: man/jtest.fct.Rd
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1913103
    From: man/pdynmc.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1913103
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

The DOIs in the DESCRIPTION are specified as given in the checklist. The DOIs given in the documentation are correct and correctly displayed on a local Windows 10 x64 install.

Please note that when trying to check R-devel (2021-08-12 r80748), an error on the availability of function 'Rcpp_precious_remove' by package 'Rcpp' occurred.




## Downstream dependencies
There are currently no downstream dependencies for this package.
