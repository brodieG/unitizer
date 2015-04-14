
library("unitizer")

test.file <- "helper/translate/testthat/test-translate2.R"
target.dir.base <- tempfile()
target.dir <- file.path(target.dir.base, "helper/translate/unitizer")
res1 <- testthat_translate_file(test.file, target.dir, prompt="overwrite")  # has to be outside of `testthat`

test_that("translate a file", {
  expect_equal(
    res1,
    c("# for translate unitizer tests", "set.seed(1)", "# context(\"testthat to unitizer\")", "# random non-sectioned", "# blah blah", "rev(10:1)", "unitizer_sect(\"simple tests\", {\n    # first internal\n    fun0(a)\n    # internal comment\n    fun1(a, b, c, d, e, f)\n    # \"external\" comment\n    fun1(a)\n})", "# a test for errors", "unitizer_sect(\"errors\", {\n    # Making up sections\n    stop(\"hello\")\n    warning(\"yoyo\")\n})")
  )
  # Can't do this twice in a row without prompting in non-interactive mode

  if(!interactive()) {
    expect_error(
      testthat_translate_file(test.file, target.dir),
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
    list(NULL, 1:10, NULL, NULL, NULL, NULL, "yoyo")
  )
})
unlink(target.dir.base, recursive=TRUE)

test.dir <- "helper/translate/testthat/"
target.dir.base <- tempfile()
target.dir <- file.path(target.dir.base, "helper/translate/unitizer")
res2 <- testthat_translate_dir(test.dir, target.dir)  # has to be outside of `testthat`

test_that("translate a dir", {
  expect_equal(
    res2,
    list(c("# for translate unitizer tests", "# blah blah", "fun0(a)", "fun1(a)", "# a test for errors", "stop(\"hello\")", "random_function()"), c("# for translate unitizer tests", "set.seed(1)", "# context(\"testthat to unitizer\")", "# random non-sectioned", "# blah blah", "rev(10:1)", "unitizer_sect(\"simple tests\", {\n    # first internal\n    fun0(a)\n    # internal comment\n    fun1(a, b, c, d, e, f)\n    # \"external\" comment\n    fun1(a)\n})", "# a test for errors", "unitizer_sect(\"errors\", {\n    # Making up sections\n    stop(\"hello\")\n    warning(\"yoyo\")\n})"))
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
    list(NULL, 1:10, 42, 24, 24, NULL, "yoyo")
  )
})

unlink(target.dir.base, recursive=TRUE)
