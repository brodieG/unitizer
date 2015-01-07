# unitizeR - Easy R Unit Tests

Note: this is a copy of the vignette you would get with:
```
vignette("01introduction", package="unitizer")
```
we recommend you install this package and use that vignette as the links embedded here will only work properly from the vignette.

## Introduction

### Motivation

`unitizer` makes unit tests easy by turning the "informal" tests you write during code development into unit tests.  It does so by storing the test expressions **and** their results.

To use `unitizer`:

* Save your "informal" tests to an R file
* Run `unitize("my_file_name.R")`

`unitize` will step through the tests in an interactive environment for you to review and accept as unit tests.  Subsequently, any time you make changes to your project you can re-run `unitize`.  You will be alerted if any tests produce results different than when they were first added to the `unitizer`.  In other words, you will be alerted to any regressions you introduce.

Informal tests are R expressions you would normally type in the command line to test that your code is working as expected.  There is no need for special syntax, functions, or anything other than the R expression itself.

### Demo

A big part of `unitizer` is the [interactive environment](04interactiveenvironment.html).  It allows you to quickly review, add, and remove tests from your unit test store.  The best way to get a feel for the `unitizer` process is to run the provided demo:
```
library(unitizer)
demo(unitizer)
```
There is a [companion vignette](02democompanion.html) for the demo as well.

### How is `unitizer` Different to `testthat`?

`unitizer` trades off some formality for ease of use, and as with any trade-off, there are pros and cons.  Let us illustrate with some unit tests for the `base::log10` function.  First, as implemented in `testthat`:

```
expect_equal(
  log10(c(100, -100, 0, 1/10, Inf, -Inf, NA, 500)),
  c(2, NaN, -Inf, -1, Inf, NaN, NA, 2.69897000433602)
)
expect_warning(
  log10(c(100, -100, 0, 1/10, Inf, -Inf, NA, 500)),
  "NaNs produced"
)
expect_error(
  log10(letters),
  "Error in log10\\(letters\\) : non-numeric argument to mathematical function\n"
)
```
And as implemented in `unitizer`:
```
log10(c(100, -100, 0, 1/10, Inf, -Inf, NA, 500))  # succeed with warnings
log10(letters)                                    # input error
```
These two unit test implementations are functionally equivalent, though clearly different in terms of what the user needs to do.  There are benefits to both approaches.  In favor of `unitizer`:

* Tests are easy to write
    * Notice how we do not need to provide the result vector for the first test
* Conditions are captured automatically
    * Values and warnings are both captured at the same time
    * Errors do not require any special handling

In favor of `testthat`:

* The tests are self documenting
    * Expected results are obvious from the source, whereas it is not so with `unitizer`
* Once you write the test you are done
    * With `unitizer` you still need to `unitize` and review the tests

The main selling point for `unitizer` is that the tests are easy to write.  This is particularly true when the return values from the tests are complex (e.g., conditions with messages, as shown above, or simply complex objects, as returned by `lm` for example).  You review the test result, confirm it is what it should be, and tell `unitizer` to store it.  There is no need to copy and paste awkward deparsed expressions into tests.

On the other hand, `unitizer` is not conducive to either self documentation or to test driven / [extreme](http://www.extremeprogramming.org/rules/testfirst.html) programming.  If these features are important to you then you are likely better off using more formal unit testing frameworks.

**UPDATE**: recently `testthat` added `expect_equal_to_reference`, which takes a similar approach by storing test results in an RDS file and subsequently comparing to the pre-calculated value.  The difference is that more user work is required since each test requires a separate file, and both the initial result verification and checking differences requires manual review and/or manually loading the RDS file for comparison.  Additionally, it appears conditions still need to be handled independently.

### `unitizer` and Packages

The simplest way to use `unitizer` as part of your package development process is to create a `tests/unitizer` folder for all your `unitizer` test scripts.  Then, in `tests`, add an additional file with calls to `unitize`.  Here is a sample test structure for our `unitizer.fastlm` demo package:
```
unitizer.fastlm/         # top level package directory
    R/
    tests/
        run.R            # <- calls `unitize`
        unitizer/
            fastlm.R
            cornerCases.R
```
And this is what the `run.R` file would look like
```
library(unitzer)
unitize("unitizer/fastlm.R")
unitize("unitizer/cornerCases.R")
```
The path specification for test files should be relative to the `tests` directory as that is what `R CMD check` effectively sets the working directory to before running the files in `tests/`.  This means you cannot just source your `run.R` file without also first setting the working directory to `tests/`.

When `unitize` is run by `R CMD check` it will run in a non-interactive mode that will succeed only if all tests pass.

There is also a `unitize_dir` function that will run `unitize` against all files in a directory.

Remember to include `unitizer` as a "suggests" package in your DESCRIPTION file.

## Quick Start

### Installation Instructions

Currently `unitizer` is only available on github:
```
library(devtools)
install_github("brodieg/unitizer")
```
### Running `unitizer`

1. Write some R expressions and save to a file (e.g. "my_test_file.R")
2. Type `unitize("my_test_file.R")` at the prompt
3. Follow interactive environment instructions

## Things You Should Know About `unitizer`

### `unitizer` Writes To Your Filesystem

The `unitize`d tests need to be saved someplace, and the default action is to save to the same directory as the test file.  You will always be prompted by `unitizer` before it writes to your file system.  See [storing `unitized` tests](06nittygritty.html#storing-unitized-tests) for implications and alternatives.

### Tests are Evaluated in Clean Environments

Objects in `.GlobalEnv` will not be visible, and any non basic packages will be detached prior to test evaluation (you can re-attach those you need in your tests with `library`).  For more details see the [reproducible tests vignette](05reproducibletests.html).

### Tests Pass If They `all.equal` Stored Reference Values

Once you have created your first `unitizer` with `unitize`, subsequent calls to `unitize` will compare the old stored value to the new one using `all.equal`.  You can change the comparison function by using `unitizer_sect` (see [tests vignette](03tests.html)).

### `unitizer` Is Complex

In order to create an intuitive (hopefully) interactive unit testing framework we had to resort to a fair bit of trickery.  For the most part this trickery should be transparent to the user, but you should be aware it exists in the event something unexpected happens that exposes it.  Here is a non-exhaustive list of some of the tricky things we do:

* `library`, `require`, `attach`, and `detach` are `trace`d during `unitize` evaluation (see [reproducible tests vignette](05reproducibletests.html))
* Each tests is evaluated in its own environment, a child of the previous test's environment; because `R` looks up objects in parent environments it appears that all tests are evaluated in one environment (see [interactive environment vignette](04interactiveenvironment.html))
* We provide modified versions of `quit`/`q` and `ls` (see [esoteric topics vignette](06nittygritty.html)) at the `unitizer` prompt
* `traceback` should work when reviewing tests that produce errors, but only because we capture the trace with `sys.calls` and write it to `base::.Traceback` during review
* We sink `stdout` and `stderr` during test evaluation to capture those streams; be careful if you are also sinking those streams (see [details on tests vignette](03tests.html))
* We parse the test file and extract comments so that they can be attached to the correct test for review
* The history file is temporary replaced so that your `unitizer` interactions do not pollute it

### Avoid Tests That Require User Input

In particular, you should avoid evaluating tests that invoke `debug`ged functions, or introducing interactivity by using something like `options(error=recover)`, or `readline`, or some such.  Tests will work, but the interaction will be challenging because you will have to do it with `stderr` and `stdout` captured...

### Avoid running `unitize` within `try` / `tryCatch` Blocks

Doing so will cause `unitize` to quit if any test expressions throw conditions.  See discussion in [error handling](06nittygritty.html#error-handling).

## Acknowledgments

A big thank you to Hadley Wickham for devoting so much of his life to making the R universe a better place to live in.  `unitizer` borrows several concepts from `testthat`, and uses `testthat` for internal unit tests.  Additionally, we used `devtools` extensively during package development.

## About the Author

Brodie Gaslam is a hobbyist programmer based in the US East Coast.
