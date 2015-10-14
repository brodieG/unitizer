library("testthat")
library("unitizer")
context("Translate")

test.file <- "helper/translate/testthat/test-translate2.R"
target.dir.base <- tempfile()
target.dir <- file.path(target.dir.base, "helper", "translate", "unitizer")
res1 <- testthat_translate_file(test.file, target.dir, prompt="overwrite")  # has to be outside of `testthat`
dummy <- new("unitizerDummy")

test_that("translate a file", {
  expect_equal(
    readLines(res1),
    c("# for translate unitizer tests", "set.seed(1)", "# context(\"testthat to unitizer\")", "# random non-sectioned", "# blah blah", "# expect_equal(rev(10:1), 1:10)", "rev(10:1)", "# test_that(\"simple tests\", {", "#     expect_equal(fun0(a), 1:10)", "#     expect_equal(fun1(a, b, c, d, e, f), runif(20))", "#     expect_true(fun1(a))", "# })", "unitizer_sect(\"simple tests\", {", "    # first internal", "    # expect_equal(fun0(a), 1:10)", "    fun0(a)", "    # internal comment", "    # expect_equal(fun1(a, b, c, d, e, f), runif(20))", "    fun1(a, b, c, d, e, f)", "    # \"external\" comment", "    # expect_true(fun1(a))", "    fun1(a)", "})", "# a test for errors", "# test_that(\"errors\", {", "#     expect_error(stop(\"hello\"))", "#     expect_warning(warning(\"yoyo\"))", "# })", "unitizer_sect(\"errors\", {", "    # Making up sections", "    # expect_error(stop(\"hello\"))", "    stop(\"hello\")", "    # expect_warning(warning(\"yoyo\"))", "    warning(\"yoyo\")", "})")
  )
  # Can't do this twice in a row without prompting in non-interactive mode

  if(!interactive()) {
    expect_error(
      testthat_translate_file(test.file, target.dir, prompt="always"),
      "Unable to proceed"
    )
  }
  untz <- get_unitizer(file.path(target.dir, "translate2.unitizer"))
  expect_equal(
    untz@items.ref.calls.deparse,
    c("set.seed(1)", "rev(10:1)", "fun0(a)", "fun1(a, b, c, d, e, f)", "fun1(a)", "stop(\"hello\")", "warning(\"yoyo\")")
  )
  expect_equal(
    lapply(unitizer:::as.list(untz@items.ref), function(x) x@data@value),
    list(dummy, 1:10, NULL, NULL, NULL, NULL, "yoyo")
  )
})
unlink(target.dir, recursive=TRUE)

test.dir <- "helper/translate/testthat/"
target.dir <- file.path(target.dir.base, "helper/translate/unitizer")
res2 <- testthat_translate_dir(test.dir, target.dir)  # has to be outside of `testthat`

test_that("translate a dir", {
  expect_equal(
    lapply(res2, readLines),
    list(c("# for translate unitizer tests", "# blah blah", "# expect_equal(fun0(a), 1:10)", "fun0(a)", "# expect_true(fun1(a))", "fun1(a)", "# a test for errors", "# expect_error(stop(\"hello\"))", "stop(\"hello\")", "random_function()"), c("# for translate unitizer tests", "set.seed(1)", "# context(\"testthat to unitizer\")", "# random non-sectioned", "# blah blah", "# expect_equal(rev(10:1), 1:10)", "rev(10:1)", "# test_that(\"simple tests\", {", "#     expect_equal(fun0(a), 1:10)", "#     expect_equal(fun1(a, b, c, d, e, f), runif(20))", "#     expect_true(fun1(a))", "# })", "unitizer_sect(\"simple tests\", {", "    # first internal", "    # expect_equal(fun0(a), 1:10)", "    fun0(a)", "    # internal comment", "    # expect_equal(fun1(a, b, c, d, e, f), runif(20))", "    fun1(a, b, c, d, e, f)", "    # \"external\" comment", "    # expect_true(fun1(a))", "    fun1(a)", "})", "# a test for errors", "# test_that(\"errors\", {", "#     expect_error(stop(\"hello\"))", "#     expect_warning(warning(\"yoyo\"))", "# })", "unitizer_sect(\"errors\", {", "    # Making up sections", "    # expect_error(stop(\"hello\"))", "    stop(\"hello\")", "    # expect_warning(warning(\"yoyo\"))", "    warning(\"yoyo\")", "})"))
  )
  untz <- get_unitizer(file.path(target.dir, "translate2.unitizer"))

  expect_equal(
    untz@items.ref.calls.deparse,
    c("set.seed(1)", "rev(10:1)", "fun0(a)", "fun1(a, b, c, d, e, f)", "fun1(a)", "stop(\"hello\")", "warning(\"yoyo\")")
  )
  # Note not the same as when we did just the single file because the helper
  # file is loaded so `fun0` and `fun1` are actually defined

  expect_equal(
    lapply(unitizer:::as.list(untz@items.ref), function(x) x@data@value),
    list(dummy, 1:10, 42, 24, 24, NULL, "yoyo")
  )
  # Can't do it again since there are files there

  expect_error(
    testthat_translate_dir(test.dir, target.dir),
    "contains files so we cannot proceed"
  )
})

unlink(target.dir, recursive=TRUE)
