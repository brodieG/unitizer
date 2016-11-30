# Overflow tests from testthat.unitize.R

if(!file_test("-d", file.path("helper", "refobjs")))
  stop("Make sure wd is set to tests/testthat")

library(unitizer)
library(testthat)
local({
  old.opt.outer <- options(unitizer.color=FALSE, width=80L)
  on.exit({
    options(old.opt.outer)
    unitizer:::read_line_set_vals(NULL)
  })
  context("Unitize 2")

  update_fastlm(.unitizer.fastlm, "0.1.0")
  devtools::install(.unitizer.fastlm, quick=TRUE, quiet=TRUE, local=FALSE)
  unlink(list.dirs(test.dir, recursive=FALSE), recursive=TRUE)

  # Test unreviewed

  unitizer:::read_line_set_vals(
    c(
      "Y", "Q", "U",    # Accept one and go to unreviewed
      "Y", "B", "U",    # Accept one more and browse and go to unreviewed
      "Y", "Y", "U",    # Accept two remaining and confirm no unreviewed
      "B", "U", "Q"     # No unreviewed tests
    )
  )
  txt0 <- unitizer:::capture_output(
    unitize(.unitizer.test.file, interactive.mode=TRUE)
  )
  test_that("unreviewed variations", {
    expect_equal_to_reference(
      unitizer:::clean_eval_exp(txt0),
      file.path("helper", "refobjs", "unitize2_unreview.rds")
    )
  } )
  # Test re-eval

  unitizer:::read_line_set_vals(
    c(
      "1", "Y", "R", "Y",   # Re-eval and jump back to file 1
      "Q",                  # Quit from file 1 and back to main menu
      "2", "Y", "Y",        # Accept one test in file 2 and quit
      "3", "Y", "RR", "Y",  # Go to file 3, accept one, and Re-eval all
      "Q", "Q"
    )
  )
  txt1a <- unitizer:::capture_output(
    untz1 <- unitize_dir(test.dir, interactive.mode=TRUE)
  )
  txt1b <- unitizer:::capture_output(print(untz1))
  untz1.clean <- lapply(untz1, `attributes<-`, NULL) # temp file names

  test_that("Re-eval", {
    expect_equal_to_reference(
      txt1a, file.path("helper", "refobjs", "unitize2_rerun_a.rds")
    )
    expect_equal_to_reference(
      txt1b, file.path("helper", "refobjs", "unitize2_rerun_b.rds")
    )
    expect_equal_to_reference(
      untz1.clean, file.path("helper", "refobjs", "unitize2_rerun_res.rds")
    )
  } )
  # Make sure that deleted items from a section are still marked from that
  # section upgrade to version two to use the files that are set up for that
  # there; notice update_fastlm_*extra*

  # Re-set by dropping unitizers

  unlink(list.dirs(test.dir, recursive=FALSE), recursive=TRUE)
  unitizer:::update_fastlm_extra(.unitizer.fastlm)
  devtools::install(.unitizer.fastlm, quick=TRUE, quiet=TRUE, local=FALSE)
  test.file.1 <- file.path(test.dir, "unitizer.fastlm.R")
  test.file.2 <- file.path(test.dir, "unitizer.fastlm2.R")
  test.store <- file.path(test.dir, "store2.unitizer")

  # First auto accept all initial tests, and then re-run with second version to
  # make sure deleted tests are where we think they should be

  out.1 <- unitizer:::capture_output(
    unitize(test.file.1, test.store, auto.accept="new")
  )
  unitizer:::read_line_set_vals(c("B", "Q"))
  out.2 <- unitizer:::capture_output(
    untz.2 <- unitize(test.file.2, test.store, interactive.mode=TRUE)
  )
  test_that("Section Extra", {
    expect_equal_to_reference(
      `attributes<-`(untz.2, NULL),
      file.path("helper", "refobjs", "unitize2_del_sec.rds")
    )
  } )

  # Sections with comp funs that output to stdout/stderr

  temp.loc <- tempfile()
  dir.create(temp.loc)
  on.exit(unlink(temp.loc, recursive=TRUE))
  file.copy(file.path("helper", "sects.R"), temp.loc)
  unitizer:::capture_output(
    unitize(
      file.path(temp.loc, "sects.R"), auto.accept="new",
      interactive.mode=FALSE
  ) )
  test_that("warning when comp funs produce output", {
    expect_warning(
      unitizer:::capture_output(
        unitize(file.path(temp.loc, "sects.R"), interactive.mode=FALSE)
      ),
      "Test comparison functions appear to have produced output"
    )
  })
})
