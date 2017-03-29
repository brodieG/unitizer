library(unitizer)
context("Demo")

local({

  unlink(list.dirs(test.dir, recursive=FALSE), recursive=TRUE)

  test_that("copy fastlm dir works", {
    expect_identical(
      sort(list.files(.unitizer.fastlm)),
      sort(
        c(
          "DESCRIPTION", "man", "NAMESPACE", "R", "tests",
          "utzflm.Rcheck"
      ) )
    )
    expect_identical(
      readLines(file.path(.unitizer.fastlm, "DESCRIPTION"))[[5L]],
      "Version: 0.1.0"
    )
    update_fastlm(.unitizer.fastlm, version="0.1.1")
    expect_identical(
      readLines(file.path(.unitizer.fastlm, "DESCRIPTION"))[[5L]],
      "Version: 0.1.1"
    )
    update_fastlm(.unitizer.fastlm, version="0.1.2")
    expect_identical(
      readLines(file.path(.unitizer.fastlm, "DESCRIPTION"))[[5L]],
      "Version: 0.1.2"
    )
  })
  test_that("show_file", {
    f <- tempfile()
    cat("this is a\ntest code\nfile\n", file=f)
    file.show <- capture.output(show_file(f))
    expect_equal(file.show[[1L]], "+---------------+")
    start.file <- grep("+---+-----------+", file.show, fixed=TRUE)
    expect_equal(length(start.file), 2L)
    expect_equal(
      file.show[start.file[[1L]]:start.file[[2L]]],
      c(
        "+---+-----------+", "| 1 | this is a |", "| 2 | test code |",
        "| 3 | file      |", "+---+-----------+"
      )
    )
    unlink(f)
  })
  # Run actual demo bits; note we want to force `interactive.mode=TRUE` so that
  # `read_line_vals` values are used as user input; note that until we fix
  # / rationalize how sinking behaves within unitizer when the standard streams
  # come in sunk, we won't be able to fully test everything, since for example
  # the display of the captured stdout just won't happen.

  old.opt <- options(width=80)
  on.exit(options(old.opt), add=TRUE)

  # options(unitizer.disable.capt=c(output=TRUE, message=FALSE))

  # In tests, initial version of package should be 0.1.0; the test store
  # does not exist so it doesn't get overwritten with subsequent updates
  # Note the initial install happens in the test running script

  unitizer:::update_fastlm(.unitizer.fastlm, version="0.1.0")
  devtools::install(.unitizer.fastlm, quick=TRUE, quiet=TRUE, local=FALSE)

  unitizer:::read_line_set_vals(c("Y", "Y", "Y", "Y", "Y"))
  txt1 <- unitizer:::capture_output(
    untz <- unitize(.unitizer.test.file, interactive.mode=TRUE)
  )
  # Re-running doesn't change unitizer

  txt2 <- unitizer:::capture_output(
    untz2 <- unitize(.unitizer.test.file, interactive.mode=TRUE)
  )
  # Rejecting failed tests does not change unitizer

  update_fastlm(.unitizer.fastlm, version="0.1.1")
  devtools::install(.unitizer.fastlm, quick=TRUE, quiet=TRUE, local=FALSE)
  unitizer:::read_line_set_vals(c("N", "N", "Y"))
  txt3 <- unitizer:::capture_output(
    untz3 <- unitize(.unitizer.test.file, interactive.mode=TRUE)
  )
  untz.clean <- lapply(
    list(untz, untz2, untz3), function(x) {
      attr(x, "test.file") <- basename(attr(x, "test.file"))
      attr(x, "store.id") <- basename(attr(x, "store.id"))
      x
  } )
  test_that("demo create worked", {
    expect_is(untz, "unitizer_result")
    expect_equal_to_reference(
      untz.clean[[3]], file.path("helper", "refobjs", "demo_res1.rds")
    )
    expect_equal_to_reference(
      untz.clean[[1]], file.path("helper", "refobjs", "demo_res2.rds")
    )
    expect_equal_to_reference(
      untz.clean[[2]], file.path("helper", "refobjs", "demo_res3.rds")
    )
    expect_match(
      paste0(txt1$output, collapse=""),
      "\\+-+\\+| unitizer for: tests/unitizer/fastlm\\.R.*Pass Fail  New  1\\. <untitled>     -    -    4.*= Finalize Unitizer.*- Adding 4 out of 4 new tests"
    )
    expect_match(
      paste0(txt1$message, collapse=""),
      "Error in fastlm\\(1:100, 1:10\\).*You will IRREVERSIBLY modify.*unitizer updated"
    )
    expect_match(
      paste0(txt2$message, collapse=""),
      "| 4/4 tests passed; nothing to review.", fixed=TRUE
    )
  })
  # review is always in interactive mode

  unitizer:::read_line_set_vals(c("5", "Q"))
  txt4 <- unitizer:::capture_output(review(.unitizer.test.store))

  test_that("demo review", {
    expect_match(
      paste0(txt4$output, collapse=""),
      "5\\. get_slope\\(res\\).*> get_slope\\(res\\).*Q"
    )
    expect_match(
      paste0(txt4$message, collapse=""),
      "| No changes recorded.", fixed=TRUE
    )
  })
  unitizer:::read_line_set_vals(c("Y", "Y", "Y"))

  txt5 <- unitizer:::capture_output(
    untz5 <- unitize(.unitizer.test.file, interactive.mode=TRUE)
  )
  test_that("demo changes", {
    expect_match(
      paste0(txt5$output, collapse=""), "Pass Fail <untitled>     2    2"
    )
    expect_match(
      paste0(txt5$message, collapse=""),
      "| You will IRREVERSIBLY modify 'unitizer/fastlm1.unitizer' by:| - Replacing 2 out of 2 failed tests| unitizer updated."
    )
  })
  unitizer:::read_line_set_vals(NULL)
})
