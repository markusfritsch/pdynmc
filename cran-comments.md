## Test environments
local Windows 10 x64 install: R 4.0.3, R 4.1.3, and R-devel (2022-03-23 r81968 ucrt)
github actions: ubuntu 20.04.4 LTS, R 4.0.5 R 4.1.3 and R-devel (2022-03-24 r81969)
  Microsoft Windows Server 2022 10.0.20348, R 4.1.3
  Mac OS X 11.6.5 20G527, R 4.1.3
winbuilder: R 4.0.5, R 4.1.3, and R-devel (2022-03-23 r81968 ucrt)


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
