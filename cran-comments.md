## Test environments
local Windows 10 x64 install: R 4.0.3, R 4.1.1, and R-devel (2021-10-13 r81052)
github actions: ubuntu 20.04.3 LTS, R 4.0.5 R 4.1.1 and R-devel (2021-10-12 r81044)
  Microsoft Windows Server 2019 10.0.17763, R 4.1.1
  Mac OS X 10.15.7 19H1417, R 4.1.1
winbuilder: R 4.0.5, R 4.1.0, and R-devel (2021-10-07 r81018)


## R CMD check results
There were no ERRORs, WARNINGs.

There was 1 NOTE during the checks with winbuilder:

Possibly mis-spelled words in DESCRIPTION:
  Hwang (35:6)
  Kang (35:13)

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

The author names are correct. The DOIs in the DESCRIPTION are specified as given in the checklist. The DOIs given in the documentation are correct and correctly displayed on a local Windows 10 x64 install.





## Downstream dependencies
There are currently no downstream dependencies for this package.
