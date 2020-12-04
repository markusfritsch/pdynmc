## Test environments
local Windows 10 x64 install: R 3.6.3, R 4.0.2, R 4.0.3, and R-devel (2020-12-03 r79553)
travis-ci: ubuntu 16.04.6 LTS, R 3.6.3 R 4.0.0 and R-devel (2020-05-06 r78370)
winbuilder: R 3.6.3, R 4.0.3, and R-devel (2020-12-03 r79553)


## R CMD check results
There were no ERRORs, WARNINGs.

There was 1 NOTE during the local checks:
'unable to verify current time'
This seems to be due to the following:
Resource http://worldclockapi.com/ currently not available for R CMD check
(see https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time)



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

The URLs work fine in my browser (see https://cran.r-project.org/web/packages/URL_checks.html).
Please let me know if I should change anything.




## Downstream dependencies
There are currently no downstream dependencies for this package.
