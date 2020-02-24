## Submission Notes:

Dear CRAN Maintainers,

I just submitted an update to the unitizer package that
should resolve the current CRAN errors.  The errors
happened because for **tests and demos only**, we
used install.packages to install dummy packages
to test and demonstrate the use of unitizer as a
unit testing framework for package development.

In retrospect, it seems obvious that even for that
use, and even with the precautions I took (

All instances of `install.packages` that were used
in *tests and demo* now install to a temporary
folder created with:

tmp.lib <- file.path(tempfile(), 'utz-tmp-lib')
dir.create(tmp.lib, recursive=TRUE)
install.packages(dummy_package, lib=tmp.lib)

The package itself does not use `install.packages`.
It is only used in integration tests to confirm that
this package correctly tracks package versions and
other meta data of other packages that use unitizer
for their unit tests.  To test this we need to
be able to run tests on dummy packages with
changing versions.

Previously `lib=tmp.lib` was not specified and tests
started failing when the library became read-only.
In retrospect it seems obvious that I should never
have been writing these dummy packages to the
standard library location, although I did take care
to ensure the dummy packages
I was installing did not already
exist, and removed them on.exit.




## R CMD check --as-cran

Status: OK

### Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 14.04.5 LTS
    * R-devel (unstable) (2017-11-24 r73779)
    * R version 3.4.2 (2017-01-27)
    * R version 3.3.3 (2017-01-27)
* Winbuilder
    * R devel (2017-09-12 r73242)
    * Link to build: https://win-builder.r-project.org/1RaLvhYmD8Ab/
* Locally on Mac OS 10.12.5
    * R version 3.4.1 (2017-06-30)


