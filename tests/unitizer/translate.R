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

  testthat_translate_file("blahbalh")  # not real file, should fail
  testthat_translate_file("unitizer/helper/translate1.R", NULL)
  testthat_translate_file("unitizer/helper/translate2.R", NULL)
  testthat_translate_file(
    "unitizer/helper/translate2.R", NULL, keep.testthat.call=TRUE
  ) # keep testthat call
})

unitizer_sect("Convert File Names", {
  f1 <- "tests/testthat/test-one.R"
  unitizer:::testthat_translate_name(f1)
  unitizer:::testthat_translate_name(f1, name.pattern="^.{6}(.*)")
  unitizer:::testthat_translate_name(f1, name.replace="hello")
  unitizer:::testthat_translate_name(f1, name.new="boom")
  unitizer:::testthat_translate_name(f1, list(), name.new="boom")
  unitizer:::testthat_translate_name(f1, name.new=file.path("hello", "there"))  # shouldn't be able to use subdirs
  unitizer:::testthat_translate_name(f1, name.replace=file.path("hello", "there")) # shouldn't be able to use subdirs
})
