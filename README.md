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
Or check out the screencast to see `unitizer` in action:
[![2 Minute Animated Gif
Example](https://github.com/brodieG/unitizer/blob/development/extra/gif/frames/fin.gif)](https://github.com/brodieG/unitizer/blob/development/extra/gif/frames/fin.gif)

## Description and Usage

`unitizer` stores R expressions and the result of evaluating them so that it can
detect code regressions.  This is akin to saving test output to a
`.Rout.save` file as documented in [Writing R
Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-subdirectories),
except that:

* Result objects and signaled conditions are automatically saved so they can be
  examined in detail
* You can review each test separately before it is stored
* When a test fails, you can immediately review it interactively in the
  environment in which it was evaluated
* You can easily add, remove, and modify tests with a couple of keystrokes

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

## Acknowledgments

Thank you to:

* R Core for developing such a wonderfully flexible language
* Hadley Wickham for devoting so much of his life to making the R universe a
  better place to live in.  `unitizer` borrows several concepts from `testthat`,
  and uses `testthat` for internal unit tests.
* [Jim Hester](https://github.com/jimhester) for developing `covr` and nudging
  me to use it.  Seeing line by line test coverage is an eye opening experience.
* [Gábor Csárdi](https://github.com/gaborcsardi) for `crayon` and for adding a
  new dimension to the R experience
* @kohler for [gifsicle](https://github.com/kohler/gifsicle) and the [ffmpeg
  team](http://ffmpeg.org/about.html) for ffmpeg
* [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
  Boettiger](https://github.com/cboettig) for the
  [rocker](https://github.com/rocker-org/rocker) project.
* All open source developers out there that make their work freely available
  for others to use
* [Github](https://github.com/), [Travis-CI](https://travis-ci.org/),
  [Codecov](https://codecov.io/), [Vagrant](https://www.vagrantup.com/),
  [Docker](https://www.docker.com/), [Ubuntu](https://www.ubuntu.com/),
  [Brew](https://brew.sh/) for providing infrastructure that greatly simplifies
  open source development
* [Free Software Foundation](http://fsf.org/) for developing the GPL license and
  promotion of the free software movement

## About the Author

Brodie Gaslam is a hobbyist programmer based in the US East Coast.
