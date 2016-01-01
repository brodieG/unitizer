# unitizeR - Easy R Unit Tests
<table style="border: none; background-color: transparent; vertical-align: middle;">
  <tr style="border: none; background-color: transparent; padding: 2px;">
    <td style="border: none; background-color: transparent; padding: 2px; padding-right: 50px;">
      <a href='https://travis-ci.org/brodieG/unitizer'><img src='https://travis-ci.org/brodieG/unitizer.png?branch=master'></a>
    <td style="border: none; background-color: transparent; padding: 2px;">RC:
    <td style="border: none; background-color: transparent; padding: 2px;">
      <a href='https://travis-ci.org/brodieG/unitizer'><img src='https://travis-ci.org/brodieG/unitizer.png?branch=rc'></a>
    <td style="border: none; background-color: transparent; padding: 2px;">Dev:
    <td style="border: none; background-color: transparent; padding: 2px;">
      <a href='https://travis-ci.org/brodieG/unitizer'><img src='https://travis-ci.org/brodieG/unitizer.png?branch=development'></a>
  <tr style="border: none; background-color: transparent; padding: 2px;">
    <td style="border: none; background-color: transparent; padding: 2px; padding-right: 50px;">
      <a href='https://codecov.io/github/brodieG/unitizer?branch=master'><img src='https://codecov.io/github/brodieG/unitizer/coverage.svg?branch=master'></a>
    <td style="border: none; background-color: transparent; padding: 2px;">RC:
    <td style="border: none; background-color: transparent; padding: 2px;">
      <a href='https://codecov.io/github/brodieG/unitizer?branch=rc'><img src='https://codecov.io/github/brodieG/unitizer/coverage.svg?branch=rc'></a>
    <td style="border: none; background-color: transparent; padding: 2px;">Dev:
    <td style="border: none; background-color: transparent; padding: 2px;">
      <a href='https://codecov.io/github/brodieG/unitizer?branch=dev'><img src='https://codecov.io/github/brodieG/unitizer/coverage.svg?branch=development'></a>
</table>

## TL;DR

`unitizer` simplifies creating, reviewing, and debugging unit tests in R.  To install:
```
library(devtools)
install_github("brodieg/unitizer")
```
We will release to CRAN once version 1.0.x is more thoroughly tested.

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
* [`vignette("unitizer_index")` for a list of vignettes](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/unitizer_index.html), or skip straight to the [Introduction vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/unitizer.html)

## Acknowledgments

Thank you to:

* Hadley Wickham for devoting so much of his life to making the R universe a better place to live in.  `unitizer` borrows several concepts from `testthat`, and uses `testthat` for internal unit tests.
* Jim Hester for developing `covr` and nudging me to use it.  Seeing line by line test coverage is an eye opening experience.
* Gábor Csárdi for `crayon` and for adding a new dimension to the R experience
* R Core for developing such a wonderfully flexible language

## About the Author

Brodie Gaslam is a hobbyist programmer based in the US East Coast.
