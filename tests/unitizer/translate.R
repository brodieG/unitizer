# Test testthat translation; seems okay to use `unitizer` for this limited
# testing since none of the translation tools are actually used by `unitizer`
# for anything

unitizer_sect("check testthat attachment", {
  unitizer:::is_testthat_attached()
  library(testthat)
  unitizer:::is_testthat_attached()
})

unitizer_sect("extract calls", {
  unitizer:::testthat_match_call(
    quote(expect_equal(fun(a), TRUE)), expect_equal, "object"
  )
  unitizer:::testthat_match_call(
    quote(expect_equal(expected=fun(a), TRUE)), expect_equal, "object"
  )
  unitizer:::testthat_match_call(
    quote(expect_equal(a=fun(a), b=TRUE)), expect_equal, "object"
  )
  unitizer:::testthat_match_call(
    quote(expect_equal(fun(a), TRUE)), "iamnotafunction", "object"
  )
  unitizer:::testthat_match_call(
    quote(test_that("a section", TRUE)), test_that, c("desc", "code")
  )
  unitizer:::testthat_match_call(
    quote(test_that("a section", {fun(a); TRUE; 1+1})),
    test_that, c("desc", "code")
  )
})

unitizer_sect("translate simple", {
  # NOTE: paths relative to R CMD check set-up

  testthat_to_unitizer("blahbalh")  # not real file, should fail
  testthat_to_unitizer("unitizer/helper/translate1.R", NULL)
  testthat_to_unitizer("unitizer/helper/translate2.R", NULL)
})
