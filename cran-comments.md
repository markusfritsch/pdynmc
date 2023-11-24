## Test environments
local Windows 11 x64 install: R 4.1.3, R 4.2.2, 4.3.0, 4.3.2, and R-devel (2023-11-21 r85583 ucrt)
github actions: ubuntu 22.04.3 LTS, R 4.2.3, 4.3.2 and R-devel (2023-11-22 r85609)
  Microsoft Windows Server 2022 10.0.20348, R 4.3.2
  Mac OS 12.6.9 21G726, R 4.3.2
winbuilder: R 4.2.3, R 4.3.2, and R-devel (2023-11-23 r85618 ucrt)


## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

There was 1 NOTE during the checks with winbuilder for R 4.3.2:

Found the following (possibly) invalid URLs:
  URL: https://econpapers.repec.org/software/bocbocode/s435901.htm
    From: man/pdynmc.Rd
    Status: Error
    Message: Failed to connect to econpapers.repec.org port 443 after 21979 ms: Couldn't connect to server

The URL is correct and accessible and the note did not appear for the checks
based on R 4.2.3 and R-devel (2023-11-23 r85618 ucrt)





## Downstream dependencies
There are currently no downstream dependencies for this package.
