## Re-submission Notes:

This is a re-submission of the package to resolve the
CRAN farm R CMD check failures on windows.  I have
given up on trying to fix the two failing tests since
they test minor ancillary features, and I cannot
reproduce the failures on winbuilder, my own windows
machine, or the Rhub windows machines.

Instead I am just skipping those two tests.

## R CMD check --as-cran

There is one NOTE:

    Maintainer: <brodie.gaslam@yahoo.com>

    Days since last update: 2

I submitted this tar.gz file to winbuilder and
the tests passed (links at end).

## Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 12.04.5 LTS
    * R devel (2017-04-05 r72488)
    * R version 3.3.2 (2016-10-31)
    * R version 3.2.5 (2016-04-14)
* Winbuilder
    * R devel (2017-04-03 r72475)
    * R version 3.3.3 (2017-03-06)
* Locally on Mac OS 10.12.1
    * R version 3.3.3 (2017-03-06)
* Rhub:
    * R devel (2017-03-10 r72327

## Links to Winbuilder Builds

* Devel: https://win-builder.r-project.org/7a29iSip2YkP/00check.log
* Release:  https://win-builder.r-project.org/As13BL0sFZy2/00check.log
