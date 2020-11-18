<!-- README.md is generated from README.Rmd. Please edit that file 

# render('README.Rmd', output_format=html_vignette(css='vignettes/styles.css'))
rmarkdown::render('README.Rmd', output_format=rmarkdown::md_document())

-->
unitizeR - Interactive R Unit Tests
===================================

[![](https://travis-ci.org/brodieG/unitizer.svg?branch=master)](https://travis-ci.org/brodieG/unitizer)
[![](https://codecov.io/github/brodieG/unitizer/coverage.svg?branch=master)](https://codecov.io/github/brodieG/unitizer?branch=master)
[![](http://www.r-pkg.org/badges/version/unitizer)](https://cran.r-project.org/package=unitizer)
[![Dependencies
direct/recursive](https://tinyverse.netlify.app/badge/unitizer)](https://tinyverse.netlify.app/)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

TL;DR
-----

`unitizer` simplifies creation, review, and debugging of tests in R. It
automatically stores R expressions and the values they produce, so
explicit expectations are unnecessary. Every test is easy to write with
`unitizer` because testing and using a function are the same. This
encourages non-trivial tests that better represent actual usage.

Tests fail when the value associated with an expression changes. In
interactive mode you are dropped directly into the failing test
environment so you may debug it.

`unitizer` is on CRAN:

    install.packages('unitizer')

It bakes in a lot of contextual help so you can get started without
reading all the documentation. Try the demo to get an idea:

    library(unitizer)
    demo(unitizer)

Or check out the
[screencast](http://htmlpreview.github.io/?https://github.com/brodieG/unitizer/blob/rc/extra/gifshow.html)
to see `unitizer` in action.

Why Another Testing Framework?
------------------------------

### Automated Test Formalization

Are you tired of the `deparse`/`dput` then copy-paste R objects into
test file dance, or do you use `testthat::expect_equal_to_reference` or
other snapshot testing a lot?

With `unitizer` you interactively review your code, as you would when
informally testing it by typing it at the R prompt. Then, with a single
keystroke, `unitizer` stores the code, the value and any conditions
(warnings, errors, etc.) it produced, and turns the lot into a formal
regression test.

### Streamlined Debugging

Do you wish the nature of a test failure was more immediately obvious?

When tests fail, you are shown a proper
[diff](https://github.com/brodieG/diffobj) so you can clearly identify
*how* the test failed:

![diff
example](https://github.com/brodieG/unitizer/raw/rc/extra/gif/review1.png)

Do you wish that you could start debugging your failed tests without
additional set-up work?

`unitizer` drops you in the test environment so you can debug *why* the
test failed without further ado:

![review
example](https://github.com/brodieG/unitizer/raw/rc/extra/gif/review2.png)

### Fast Test Updates

Do you avoid improvements to your functions because that would require
painstakingly updating many tests?

The diffs for the failed tests let you immediately confirm only what you
intended changed. Then you can update each test with a single keystroke.

Usage
-----

`unitizer` stores R expressions and the result of evaluating them so
that it can detect code regressions. This is akin to saving test output
to a `.Rout.save` file as documented in [Writing R
Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-subdirectories),
except that we’re storing the actual R objects and it is much easier to
review them.

To use `unitizer`:

-   Write test expressions as you would when informally testing code on
    the command line, and save them to a file (e.g. “my\_file\_name.R”).
-   Run `unitize("my_file_name.R")` and follow the prompts.
-   Continue developing your package.
-   Re-run `unitize("my_file_name.R")`; if any tests fail you will be
    able to review and debug them in an interactive prompt.

`unitizer` can run in a non-interactive mode for use with `R CMD check`.

Documentation
-------------

-   `help(package="unitizer")`, in particular `?unitize`
-   `demo(package="unitizer")`
-   [`browseVignettes("unitizer")`](https://cran.r-project.org/package=unitizer/vignettes/u0_unitizer_index.html)
    for a list of vignettes, or skip straight to the [Introduction
    vignette](https://cran.r-project.org/package=unitizer/vignettes/u1_intro.html)

Related Packages
----------------

-   [`testthat`](https://cran.r-project.org/package=testthat).
-   [`tinytest`](https://cran.r-project.org/package=tinytest), which is
    extended by [`ttdo`](https://cran.r-project.org/package=ttdo) for
    [`diffobj`](https://cran.r-project.org/package=diffobj) diffs.
-   [`RUnit`](https://cran.r-project.org/package=RUnit).

Acknowledgments
---------------

Thank you to:

-   R Core for developing and maintaining such a wonderfully language.
-   CRAN maintainers, for patiently shepherding packages onto CRAN and
    maintaining the repository, and Uwe Ligges in particular for
    maintaining [Winbuilder](http://win-builder.r-project.org/).
-   [Hadley Wickham](https://github.com/hadley) for
    [testthat](https://cran.r-project.org/package=testthat) from which
    we borrow many concepts and use for internal tests, and for his many
    other packages.
-   [Gábor Csárdi](https://github.com/gaborcsardi) for
    [crayon](https://cran.r-project.org/package=crayon) through which we
    can add a new dimension to the R experience.
-   [Jim Hester](https://github.com/jimhester) because
    [covr](https://cran.r-project.org/package=covr) rocks.
-   [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
    Boettiger](https://github.com/cboettig) for the
    [rocker](https://github.com/rocker-org/rocker) project, and [Gábor
    Csárdi](https://github.com/gaborcsardi) and the R-consortium for
    [Rhub](https://github.com/r-hub/rhub), without which testing bugs on
    R-devel and other platforms would be a nightmare.
-   [Yihui Xie](https://github.com/yihui) for
    [knitr](https://cran.r-project.org/package=knitr) and [J.J.
    Allaire](https://github.com/jjallaire) et al. for
    [rmarkdown](https://cran.r-project.org/package=rmarkdown), and by
    extension John MacFarlane for [pandoc](https://pandoc.org/).
-   @kohler for [gifsicle](https://github.com/kohler/gifsicle) and the
    [ffmpeg team](http://ffmpeg.org/about.html) for ffmpeg.
-   All open source developers out there that make their work freely
    available for others to use.
-   [Github](https://github.com/), [Travis-CI](https://travis-ci.org/),
    [Codecov](https://codecov.io/),
    [Vagrant](https://www.vagrantup.com/),
    [Docker](https://www.docker.com/), [Ubuntu](https://ubuntu.com/),
    [Brew](https://brew.sh/) for providing infrastructure that greatly
    simplifies open source development.
-   [Free Software Foundation](https://www.fsf.org/) for developing the
    GPL license and promotion of the free software movement.

About the Author
----------------

Brodie Gaslam is a hobbyist programmer based in the US East Coast.
