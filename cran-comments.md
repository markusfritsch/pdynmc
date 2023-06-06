## Test environments
local Windows 10 x64 install: R 4.1.3, R 4.2.2, 4.3.0, and R-devel (2023-06-03 r84490 ucrt)
github actions: ubuntu 22.04.2 LTS, R 4.2.3, 4.3.0 and R-devel
  Microsoft Windows Server 2022 10.0.20348, R 4.3.0
  Mac OS 12.6.5 20G730, R 4.3.0
winbuilder: R 4.2.3, R 4.3.0, and R-devel (to be R-4.4.0)


## R CMD check results
There were no ERRORs, WARNINGs.

There was 1 NOTE during the checks with winbuilder:

Possibly mis-spelled words in DESCRIPTION:
  Ahn (24:5)
  Arellano (25:9)
  Bover (25:22)
  Eakin (23:11)
  Fritsch (44:9)
  GMM (27:24, 38:10, 40:34)
  Holtz (23:5)
  Hwang (34:6)
  Kang (34:13)
  Newey (23:18)
  Pua (44:18)
  Rosen (23:29)
  Schnurbus (44:23)
  Windmeijer (32:14)
  overidentification (36:5)


Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1913103
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

The author names are correct. The DOI in the DESCRIPTION is specified as given in the checklist. The DOI given in the documentation is correct and correctly displayed on a local Windows 10 x64 install.





## Downstream dependencies
There are currently no downstream dependencies for this package.
