# for translate unitizer tests

set.seed(1)
context("testthat to unitizer")

# random non-sectioned

expect_equal(rev(10:1), 1:10)        # blah blah

test_that("simple tests", {
  expect_equal(fun0(a), 1:10)        # first internal
  expect_equal(
    fun1(a, b, c, d, e, f), # internal comment
    runif(20)
  )   # "external" comment
  expect_true(fun1(a))
})
# a test for errors

test_that("errors", {
  # Making up sections

  expect_error(stop("hello"))
  expect_warning(warning("yoyo"))
})
