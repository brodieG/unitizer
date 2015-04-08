# for translate unitizer tests

test_that("simple tests", {

  expect_equal(fun0(a), 1:10)        #blah blah
  expect_equal(
    fun1(a, b, c, d, e, f), # internal comment
    runif(20)
  )   #blah blah
  expect_true(fun1(a))
})
# a test for errors

test_that("errors", {
  # Making up sections

  expect_error(stop("hello"))
  expect_warning(warning("yoyo"))
}
