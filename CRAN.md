## Re-submission Notes:

This is a re-submission of the package to resolve the
CRAN farm R CMD check failures on windows.

https://cran.r-project.org/web/checks/check_results_unitizer.html

Originally Uwe Ligges suggested the error might go
away on its own since the tests passed on winbuilder.
However, the error persists on the CRAN check farm.

I believe I have fixed the one error I could see
in the "last 13 lines of output", and hope I have
fixed the other error.  Because I cannot see the
other error, I have no way knowing for sure that
it is fixed.  I have also changed the reporter
output so that if errors persist, I should have
a much better chance to see all/more of them in
the "last 13 lines of output".

If this submission does not fix the two test
failures (out of ~750 tests), I will simply comment
them out as they are only testing ancillary
features.

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
