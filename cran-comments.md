## Test environments
local Windows 10 x64 install: R 3.6.3, R 4.0.0, R 4.0.2, and R-devel (2020-09-15 r79215)
travis-ci: ubuntu 16.04.6 LTS, R 3.6.3 R 4.0.0 and R-devel (2020-05-06 r78370)
winbuilder: R 3.6.3, R 4.0.2, and R-devel (2020-09-09 r79174)


## R CMD check results
There were no ERRORs, WARNINGs.

There was one NOTE during the local checks:
'unable to verify current time'
This seems to be due to the following:
Resource http://worldclockapi.com/ currently not available for R CMD check
(see https://stackoverflow.com/questions/63613301/r-cmd-check-note-unable-to-verify-current-time)



## Downstream dependencies
There are currently no downstream dependencies for this package.
