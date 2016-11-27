# unitizeR - Easy R Unit Tests

[![](https://travis-ci.org/brodieG/unitizer.svg?branch=master)](https://travis-ci.org/brodieG/unitizer)
[![](https://codecov.io/github/brodieG/unitizer/coverage.svg?branch=master)](https://codecov.io/github/brodieG/unitizer?branch=master)
[![](http://www.r-pkg.org/badges/version/unitizer)](https://cran.r-project.org/package=unitizer)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

**DISCLAIMER**: This package does a lot of things that CRAN would likely not
approve of.  For example, we trace some `base` functions and manipulate the
search path, though we only do so while the `unitizer` functions are evaluating
and restore everything to its original state `on.exit`.  For more details see
the [things you should know about `unitizer`](https://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/vgn01introduction.html#things-you-should-know-about-unitizer) introduction vignette section.  We will likely turn off these features by default for the final release.

## TL;DR

`unitizer` simplifies creating, reviewing, and debugging unit tests in R.  To install:
```
devtools::install_github("brodieg/unitizer")
```
`unitizer` bakes in a lot of contextual help so you can get started without reading all the documentation.  Try the demo to get an idea:
```
library(unitizer)
demo(unitizer)
```
Or just save some R expressions to a file and run:
```
library(unitizer)
unitize("my_file_name.R")
```

## Description and Usage

`unitizer` stores R expressions and the result of evaluating them so that it can detect code regressions.  This is similar to saving test output to a `.Rout.save` file as documented in [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-subdirectories), except that:

* Result objects and signaled conditions are automatically saved
* You can review each test separately before it is stored
* When a test fails, you can immediately review it interactively with all variables set to the same values they had when the test was run
* You can easily add, remove, and modify tests

To use `unitizer`:

* Write test expressions as you would when informally testing code on the command line, and save them to a file (e.g. "my_file_name.R")
* Run `unitize("my_file_name.R")` and follow the prompts
* Continue developing your package
* Re-run `unitize("my_file_name.R")`; if any tests fail you will be able to review and debug them in an interactive prompt

`unitizer` can run in a non-interactive mode for use with `R CMD check`.

## Documentation

* `help(package="unitizer")`, in particular `?unitize`
* `demo(package="unitizer")`
* [`vignette("unitizer")`](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/unitizer_index.html) for a list of vignettes, or skip straight to the [Introduction vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/unitizer.html)

## Acknowledgments

Thank you to:

* Hadley Wickham for devoting so much of his life to making the R universe a better place to live in.  `unitizer` borrows several concepts from `testthat`, and uses `testthat` for internal unit tests.
* Jim Hester for developing `covr` and nudging me to use it.  Seeing line by line test coverage is an eye opening experience.
* Gábor Csárdi for `crayon` and for adding a new dimension to the R experience
* R Core for developing such a wonderfully flexible language

## About the Author

Brodie Gaslam is a hobbyist programmer based in the US East Coast.
