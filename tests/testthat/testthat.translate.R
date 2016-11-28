library("testthat")
library("unitizer")
context("Translate")

if(!file_test("-d", file.path("helper", "refobjs")))
  stop("Make sure wd is set to tests/testthat")

test.file <- "helper/translate/testthat/test-translate2.R"
target.dir.base <- tempfile()
target.dir <- file.path(target.dir.base, "helper", "translate", "unitizer")

# Must prompt to create directory when promp = "always"

local({
  on.exit({
    unlink(target.dir, recursive=TRUE)
    unitizer:::read_line_set_vals(NULL)
  })
  test_that("Prompt to create dir", {
    expect_error(
      testthat_translate_file(
        test.file, target.dir, prompt="always", interactive.mode=FALSE
      ),
      "Unable to proceed"
    )
  })
  # translations have to be outside of `testthat`; second translation should fail
  # except we allow manual input

  unitizer:::capture_output({
    unitizer:::read_line_set_vals(c("Y"))
    res1 <- testthat_translate_file(
      test.file, target.dir, prompt="always", interactive.mode=TRUE
    )
    res1.txt <- readLines(res1)
    unitizer:::read_line_set_vals(c("Y"))
    res2 <- testthat_translate_file(
      test.file, target.dir, prompt="overwrite", interactive.mode=TRUE
    )
    res2.txt <- readLines(res2)
    unitizer:::read_line_set_vals(NULL)
  })
  dummy <- new("unitizerDummy")

  test_that("translate a file", {
    expect_equal_to_reference(
      res1.txt, file.path("helper", "refobjs", "translate_res1.rds")
    )
    expect_equal(res1.txt, res2.txt)

    # Can't do this twice in a row without prompting in non-interactive mode
    # note test above does work because we use interactive mode to accept prompt

    expect_error(
      testthat_translate_file(
        test.file, target.dir, prompt="always", interactive.mode=FALSE
      ),
      "Unable to proceed"
    )
    untz <- get_unitizer(file.path(target.dir, "translate2.unitizer"))
    expect_equal_to_reference(
      untz@items.ref.calls.deparse,
      file.path("helper", "refobjs", "translate_res2.rds")
    )
    expect_equal(
      lapply(unitizer:::as.list(untz@items.ref), function(x) x@data@value[[1L]]),
      list(dummy, 1:10, NULL, NULL, NULL, NULL, "yoyo", NULL)
    )
  })
  unlink(target.dir, recursive=TRUE)

  test.dir <- "helper/translate/testthat/"
  target.dir <- file.path(target.dir.base, "helper/translate/unitizer")
  # has to be outside of `testthat`
  unitizer:::capture_output(
    res2 <- testthat_translate_dir(test.dir, target.dir)
  )
  test_that("translate a dir", {
    expect_equal_to_reference(
      lapply(res2, readLines),
      file.path("helper", "refobjs", "translate_res3.rds")
    )
    untz <- get_unitizer(file.path(target.dir, "translate2.unitizer"))

    expect_equal_to_reference(
      untz@items.ref.calls.deparse,
      file.path("helper", "refobjs", "translate_res4.rds")
    )
    # Note not the same as when we did just the single file because the helper
    # file is loaded so `fun0` and `fun1` are actually defined

    expect_equal(
      lapply(unitizer:::as.list(untz@items.ref), function(x) x@data@value[[1L]]),
      list(dummy, 1:10, 42, 24, 24, NULL, "yoyo", NULL)
    )
    # Can't do it again since there are files there

    expect_error(
      testthat_translate_dir(test.dir, target.dir),
      "contains files so we cannot proceed"
    )
  })

})
