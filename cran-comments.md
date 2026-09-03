## Test environments
local Windows 11 x64 install: R 4.1.3, R 4.4.2 R. 4.6.1, and R-devel (2026-09-01 r90464)
github actions: ubuntu 24.04.4 LTS, R 4.5.3, 4.6.1, and R-devel (4.7.0)
  Microsoft Windows Server 2025 10.0.26100, R 4.6.1
  macOS 26.5.2 25F84, R 4.6.1
winbuilder: R 4.5.3, R 4.6.1, and R-devel (2026-08-31 r90457 ucrt)


## R CMD check results
There were no ERRORs or WARNINGs.
There was one NOTE in the winbuilder checks on CRAN (R 4.6.1 and R.devel):

Found the following (possibly) invalid URLs:

The listed URLs are included in an article published in the R Journal that is used as package vignette.


## Downstream dependencies
There are currently no downstream dependencies for this package.
