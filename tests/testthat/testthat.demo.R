library(unitizer)
context("Demo")

# Mostly makes sure the demo steps work, but since it is a convenient way of
# generating a unitizer with actual errors and so forth, we use it to test a few
# other things as well in the context of those unitizers

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

  old.opt <- options(
    unitizer.color=FALSE, width=80L, crayon.enabled=TRUE,
    diffobj.term.colors=8
  )
  on.exit(options(old.opt))

  # options(unitizer.disable.capt=c(output=TRUE, message=FALSE))

  # In tests, initial version of package should be 0.1.0; the test store
  # does not exist so it doesn't get overwritten with subsequent updates
  # Note the initial install happens in the test running script

  unitizer:::update_fastlm(.unitizer.fastlm, version="0.1.0")
  install.packages(.unitizer.fastlm, repos=NULL, type='src', quiet=TRUE)

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
  install.packages(.unitizer.fastlm, repos=NULL, type='src', quiet=TRUE)
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
  # Use this opportunity to make sure `use.diff=FALSE` works as intended

  unitizer:::read_line_set_vals("Q")

  txt5a <- unitizer:::capture_output(
    unitize(.unitizer.test.file, interactive.mode=TRUE, use.diff=FALSE)
  )
  unitizer:::read_line_set_vals(c(".DIFF$state", "Q"))
  txt5b <- unitizer:::capture_output(
    unitize(.unitizer.test.file, interactive.mode=TRUE, use.diff=FALSE)
  )
  test_that("use.diff", {
    expect_equal_to_reference(
      txt5a, file.path("helper", "refobjs", "unitize_showdiff.rds")
    )
    expect_equal_to_reference(
      txt5b, file.path("helper", "refobjs", "unitize_usediff_no.rds")
    )
  })
  # See what happens if `diffobj` fails

  unitizer:::read_line_set_vals("Q")
  with_mock(
    `diffobj::diffObj`=function(...) stop("A failing diff."),
    txt5c <- unitizer:::capture_output(
      unitize(.unitizer.test.file, interactive.mode=TRUE)
    )
  )
  test_that("failing diff", {
    expect_equal_to_reference(
      txt5c, file.path("helper", "refobjs", "unitize_faildiff.rds")
    )
  })
  # Test what happens if we back out of a multi-accept

  unitizer:::read_line_set_vals(c("YY", "N", "Q"))
  txt6 <- unitizer:::capture_output(
    unitize(.unitizer.test.file, interactive.mode=TRUE)
  )
  test_that("multi-input", {
    expect_equal_to_reference(
      txt6, file.path("helper", "refobjs", "unitize_multinput.rds")
    )
  })
  # Or if we request to go to unreviewed when there are none

  unitizer:::read_line_set_vals(c("YY", "Y", "B", "U", "Q"))
  txt7 <- unitizer:::capture_output(
    unitize(.unitizer.test.file, interactive.mode=TRUE)
  )
  test_that("multi-input", {
    expect_equal_to_reference(
      txt7, file.path("helper", "refobjs", "unitize_invalid_unrev.rds")
    )
  })
  # Make sure parse warnings are issued

  unitizer:::read_line_set_vals(c("-2147483648L", "Q"))
  txt8 <- unitizer:::capture_output(
    unitize(.unitizer.test.file, interactive.mode=TRUE)
  )
  test_that("warn in parse", {
    expect_true(any(grepl("qualified with L", txt8$message)))
  })
  # Now actually accept the changes

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
