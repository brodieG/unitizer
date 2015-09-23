# unitizeR - Easy R Unit Tests

[![Travis-CI Build Status](https://travis-ci.org/brodieG/unitizer.png?branch=master)](https://travis-ci.org/brodieG/unitizer)
[![codecov.io](https://codecov.io/github/brodieG/unitizer/coverage.svg?branch=master)](https://codecov.io/github/brodieG/unitizer?branch=master)

## Quick Start

### Installation

Currently `unitizer` is only available on github, but we plan on submitting to CRAN soon.

```
library(devtools)
install_github("brodieg/unitizer")
```

### Usage

To use `unitizer`:

* Write up test expressions for your functions as you would when informally testing them on the command line, and save them to a file (e.g. "my_file_name.R")
* Run `unitize("my_file_name.R")`

`unitize` will step through the tests in an interactive environment for you to review and accept as unit tests.  Subsequently, any time you make changes to your project you can re-run `unitize`.  `unitizer` will detect any changes to the results of your expressions so you will be alerted to any regressions you introduce.  `unitizer` is compatible with `R CMD check` and travis.

## Details

We strongly recommend you review the **[introduction vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/vgn01introduction.html)** for more details, including comparsions to other unit testing frameworks.  There is also a demo (`demo("unitizer")`) so you can explore the interactive interface, and many more vignettes with additional details (**[vignette index](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/unitizer.html)**).

## Acknowledgments

A big thank you to Hadley Wickham for devoting so much of his life to making the R universe a better place to live in.  `unitizer` borrows several concepts from `testthat`, and uses `testthat` for internal unit tests.  Additionally, we used `devtools` extensively during package development.

## About the Author

Brodie Gaslam is a hobbyist programmer based in the US East Coast.
