# unitizeR - Interactive R Unit Tests

[![](https://travis-ci.org/brodieG/unitizer.svg?branch=master)](https://travis-ci.org/brodieG/unitizer)
[![](https://codecov.io/github/brodieG/unitizer/coverage.svg?branch=master)](https://codecov.io/github/brodieG/unitizer?branch=master)
[![](http://www.r-pkg.org/badges/version/unitizer)](https://cran.r-project.org/package=unitizer)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

## TL;DR

`unitizer` simplifies creating, reviewing, and debugging unit tests in R.  To install:

```
install.packages('unitizer')
```
Please keep in mind this is an experimental framework that has been thoroughly
tested by *one* person.

`unitizer` bakes in a lot of contextual help so you can get started without reading all the documentation.  Try the demo to get an idea:
```
library(unitizer)
demo(unitizer)
```
Or check out the [screencast](http://htmlpreview.github.io/?https://github.com/brodieG/unitizer/blob/rc/extra/gifshow.html) to see `unitizer` in action.

## Why Another Testing Framework?

### Automated Test Formalization

Are you tired of the `deparse`/`dput` then copy-paste R objects into test file
dance, or do you use `testthat::expect_equal_to_reference` a lot?

With `unitizer` you review function output at an interactive prompt as you
would with informal tests.  You then store the value, conditions (e.g.
warnings, etc.), and environment for use as the reference values in formal
tests, all with a single keystroke.

### Streamlined Debugging

Do you wish the nature of a test failure was more immediately obvious?

When tests fail, you are shown a proper
[diff](https://github.com/brodieG/diffobj) so you can clearly identify _how_ the
test failed:

![diff
example](https://github.com/brodieG/unitizer/raw/rc/extra/gif/review1.png)

Do you wish that you could start debugging your failed tests without
additional set-up work?

`unitizer` drops you in the test environment so you can debug _why_ the test
failed without further ado:

![review
example](https://github.com/brodieG/unitizer/raw/rc/extra/gif/review2.png)

### Fast Test Updates

Do you avoid improvements to your functions because that would require
painstakingly updating many tests?

The diffs for the failed tests let you immediately confirm only what you
intended changed.  Then you can update each test with a single keystroke.

## Usage

`unitizer` stores R expressions and the result of evaluating them so that it can
detect code regressions.  This is akin to saving test output to a
`.Rout.save` file as documented in [Writing R
Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-subdirectories),
except that we're storing the actual R objects and it is much easier to review
them.

To use `unitizer`:

* Write test expressions as you would when informally testing code on the
  command line, and save them to a file (e.g. "my_file_name.R")
* Run `unitize("my_file_name.R")` and follow the prompts
* Continue developing your package
* Re-run `unitize("my_file_name.R")`; if any tests fail you will be able to
  review and debug them in an interactive prompt

`unitizer` can run in a non-interactive mode for use with `R CMD check`.

## Documentation

* `help(package="unitizer")`, in particular `?unitize`
* `demo(package="unitizer")`
* [`vignette("unitizer")`](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/unitizer_index.html) for a list of vignettes, or skip straight to the [Introduction vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/unitizer.html)

## Related Packages

* [testthat](https://cran.r-project.org/package=testthat)
* [RUnit](https://cran.r-project.org/package=RUnit)

## Acknowledgments

Thank you to:

* R Core for developing such a wonderfully flexible language
* Hadley Wickham for [testthat](https://cran.r-project.org/package=testthat)
  from which we borrow many concepts and use for internal tests, for
  [devtools](https://cran.r-project.org/package=devtools), and for many other
  packages.
* [Jim Hester](https://github.com/jimhester) for developing
  [covr](https://cran.r-project.org/package=covr) and nudging me to use it.
  Seeing line by line test coverage is an eye opening experience.
* [Gábor Csárdi](https://github.com/gaborcsardi) for
  [crayon](https://cran.r-project.org/package=crayon) through which we can
  add a new dimension to the R experience.
* [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
  Boettiger](https://github.com/cboettig) for the
  [rocker](https://github.com/rocker-org/rocker) project, without which testing
  bugs on R-devel would be a nightmare.
* @kohler for [gifsicle](https://github.com/kohler/gifsicle) and the [ffmpeg
  team](http://ffmpeg.org/about.html) for ffmpeg.
* [Yihui Xie](https://github.com/yihui) for
  [knitr](https://cran.r-project.org/package=knitr) and  [J.J.
  Allaire](https://github.com/jjallaire) etal for
  [rmarkdown](https://cran.r-project.org/package=rmarkdown).
* All open source developers out there that make their work freely available
  for others to use.
* [Github](https://github.com/), [Travis-CI](https://travis-ci.org/),
  [Codecov](https://codecov.io/), [Vagrant](https://www.vagrantup.com/),
  [Docker](https://www.docker.com/), [Ubuntu](https://www.ubuntu.com/),
  [Brew](https://brew.sh/) for providing infrastructure that greatly simplifies
  open source development.
* [Free Software Foundation](http://fsf.org/) for developing the GPL license and
  promotion of the free software movement

## About the Author

Brodie Gaslam is a hobbyist programmer based in the US East Coast.
