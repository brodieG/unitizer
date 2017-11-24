## Submission Notes:

A test started failing on R-devel after
Martin Maechler's improvements to S4
object deparsing.  This submission adapts
the test to work under the old and
new regime.

## R CMD check --as-cran

Status: OK

### Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 14.04.5 LTS
    * R-devel (unstable) (2017-09-01 r73181)
    * R version 3.4.1 (2017-01-27)
    * R version 3.3.3 (2017-01-27)
* Winbuilder
    * R devel (2017-09-01 r73181)
    * Link to build: https://win-builder.r-project.org/MeuvSWwBt4R6
* Locally on Mac OS 10.12.5
    * R version 3.4.0 (2017-04-21)


