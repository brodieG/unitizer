# CRAN Submission Notes

## R CMD check --as-cran

There is one NOTE:

    Maintainer: <brodie.gaslam@yahoo.com>

    New submission

and a 404 error for the as-of-yet-non-existent CRAN URL
for this package that we use in the README since the README
is also on Github:

    URL: https://cran.r-project.org/package=unitizer
    From: README.md
    Status: 404
    Message: Not Found

## Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 12.04.5 LTS
    * R devel (2017-03-28 r72430)
    * R version 3.3.2 (2016-10-31)
    * R version 3.2.5 (2016-04-14)
* Windows 7:
    * R version 3.3.3 (2017-03-06)
* Winbuilder
    * R devel (2016-08-30 r71176)
    * R version 3.3.1 (2016-06-21)
* Locally on Mac OS 10.12.1
    * R version 3.3.3 (2017-03-06)

## Additional Comments

This package is an interactive unit testing environment
that saves test output to RDS files (similar to
`.Rout.save`, but with actual R objects).  Users are
always prompted to agree to file writes.

The package also provides optional advanced features
that are DISABLED by default that are noncompliant
with CRAN policies.  For example, in order to run tests
in an environment unaffected by `.GlobalEnv`, we, at
user's request, _lightly_ trace `library`, `attach`, etc. 
to update the test environment parent environment to always
be `search(2)`.  This behavior MUST BE EXPLICITLY ENABLED
BY THE USER.  Absent explicit user action to enable
this behavior, this package complies with all CRAN
policies.

Even when advanced features are enabled by the user,
they are only active for the duration of the `unitizer`
function evaluation, with everything returned to normal
state via `on.exit` calls.

