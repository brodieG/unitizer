# Test testthat translation; seems okay to use `unitizer` for this limited
# testing since none of the translation tools are actually used by `unitizer`
# for anything

unitizer_sect("check testthat attachment", {
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
  testthat_translate_file("unitizer/translate_extra/translate1.R", NULL)
  testthat_translate_file("unitizer/translate_extra/translate2.R", NULL)
  testthat_translate_file(
    "unitizer/translate_extra/translate2.R", NULL, keep.testthat.call=FALSE
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

unitizer_sect("Translatable Funs", {
  unitizer:::testthat_translatable_fun(expect_true)
  unitizer:::testthat_translatable_fun(expect_equal)
  unitizer:::testthat_translatable_fun(expect_that)
  unitizer:::testthat_translatable_fun(expect_null)
  unitizer:::testthat_translatable_fun(expect_is)
})
