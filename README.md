[![Travis-CI Build Status](https://travis-ci.org/brodieg/unitizer.png?branch=rc0.8.0)](https://travis-ci.org/brodieg/unitizer)

# unitizeR - Easy R Unit Tests

## Motivation

`unitizer` makes unit tests easy by turning the "informal" tests you write during code development into unit tests.  It does so by storing the test expressions **and** their results.  While `unitizer` is particularly useful when testing expressions that produce complex objects, it generally simplifies unit testing.

To use `unitizer`:

* Save your "informal" tests to an R file
* Run `unitize("my_file_name.R")`

`unitize` will step through the tests in an interactive environment for you to review and accept as unit tests.  Subsequently, any time you make changes to your project you can re-run `unitize`.  You will be alerted if any tests produce results different than when they were first added to the `unitizer`.  In other words, you will be alerted to any regressions you introduce.

Informal tests are R expressions you would normally type in the command line to test that your code is working as expected.  There is no need for special syntax, functions, or anything other than the R expression itself.

## Details, Installation, Etc.

We strongly recommend you review the **[introduction vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/vgn01introduction.html)** for more details.  This page is an excerpt of that vignette.  There is also a **[vignette index](http://htmlpreview.github.io/?https://raw.githubusercontent.com/brodieG/unitizer/master/inst/doc/unitizer.html)** if you really want to get into the nitty gritty.

## Acknowledgments

A big thank you to Hadley Wickham for devoting so much of his life to making the R universe a better place to live in.  `unitizer` borrows several concepts from `testthat`, and uses `testthat` for internal unit tests.  Additionally, we used `devtools` extensively during package development.

## About the Author

Brodie Gaslam is a hobbyist programmer based in the US East Coast.
