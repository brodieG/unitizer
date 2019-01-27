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
  install.packages(.unitizer.fastlm, repos=NULL, type='src', quiet=TRUE)
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
  install.packages(.unitizer.fastlm, repos=NULL, type='src', quiet=TRUE)
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
  on.exit(unlink(temp.loc, recursive=TRUE), add=TRUE)
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
  # Corner case files

  local(
    {
      # empty

      temp.empty <- paste0(tempfile(), "-empty.R")
      on.exit(unlink(temp.empty))
      cat("\n", file=temp.empty)
      empty.capt <- unitizer:::capture_output(unitize(temp.empty, force=TRUE))

      # File, but does not end in .R

      temp.bad <- paste0(tempfile())
      on.exit(unlink(temp.bad), add=TRUE)
      cat("\n", file=temp.bad)

      badname.capt <-
        suppressWarnings(unitizer:::capture_output(unitize(temp.bad)))
    },
    envir=environment()
  )
  test_that("Corner Case Files", {
    expect_true(any(grepl("Empty unitizer", empty.capt$output)))
    expect_true(
      any(grepl("No valid unitizers available", badname.capt$message))
    )
  })
  # re-eval reeval with a modified file

  local(
    {
      temp.reeval.base <- paste0(tempfile(), "-reeval")
      temp.reeval <- paste0(temp.reeval.base, ".R")
      temp.reeval.utz <- paste0(temp.reeval.base, ".unitizer")
      on.exit(unlink(c(temp.reeval, temp.reeval.utz), recursive=TRUE))

      cat("1 + 1\n2 + 2\n", file=temp.reeval)

      unitizer:::read_line_set_vals(
        c(
          'Y',
          'P',  # force re-review
          'Y',
          'cat("1 + 1\n2 + 3\n", file=temp.reeval)', 'R', 'Y',
          'Q'
        )
      )
      # state = environment() so we can access variables from this local
      reeval.capt <- unitizer:::capture_output(
        unitize(temp.reeval, state=environment(), interactive.mode=TRUE)
      )
    },
    envir=environment()
  )
  test_that("Re-eval change", {
    expect_true(sum(grepl("Unable to find test", reeval.capt$message)) == 1L)
  })

  # Fail test with conditions

  local(
    {
      temp.cond.base <- paste0(tempfile(), "-cond")
      temp.cond <- paste0(tempfile(), ".R")
      temp.cond.utz <- paste0(tempfile(), ".unitizer")

      on.exit(unlink(c(temp.cond, temp.cond.utz), recursive=TRUE))

      cond.message <- "hello world"

      cat("warning(cond.message)", file=temp.cond)

      unitizer:::read_line_set_vals(c('Y', 'Y'))
      # state = environment() so we can access variables from this local
      unitizer:::capture_output(
        unitize(temp.cond, state=environment(), interactive.mode=TRUE)
      )
      cond.message <- "goodbye world"
      unitizer:::read_line_set_vals('Q')
      cond.capt <- unitizer:::capture_output(
        unitize(temp.cond, state=environment(), interactive.mode=TRUE)
      )
    },
    envir=environment()
  )
  test_that("Condition fail", {
    expect_true(sum(grepl('Conditions mismatch', cond.capt$output)) == 1L)
  })
  # Toggle force update, in order for this to work we need to create a situation
  # where in a `unitize_dir`, one file passes, the other doesn't, and we review
  # the file that passes.  Otherwise the only other way to do it is to set force
  # from the beginning, but that ruins the toggle effect.  One possible issue
  # here is that we don't have a great way to check the file actually changed.

  local(
    {
      temp.forceup.base.dir <- tempfile()
      dir.create(temp.forceup.base.dir)
      on.exit(unlink(temp.forceup.base.dir, recursive=TRUE))
      temp.forceup.1 <- file.path(temp.forceup.base.dir, "force1.R")
      temp.forceup.2 <- file.path(temp.forceup.base.dir, "force2.R")
      cat("force.var\n", file=temp.forceup.1)
      cat("4 + 1\n", file=temp.forceup.2)

      force.var <- 1
      unitizer:::capture_output({
        unitize(temp.forceup.1, auto.accept="new", state=environment())
        unitize(temp.forceup.2, auto.accept="new", state=environment())
      })
      force.var <- 2

      unitizer:::read_line_set_vals(c('2', '1', 'Y', 'O', 'Q', 'Q'))
      force.capt <- unitizer:::capture_output(
        unitize_dir(
          temp.forceup.base.dir, state=environment(), interactive.mode=TRUE
      ) )
    },
    envir=environment()
  )
  test_that("Force", {
    expect_true(
      sum(grepl('Toggling force update mode ON', force.capt$message)) == 1L
    )
    expect_true(
      sum(
        grepl('You are about to .* with re-evaluated', force.capt$message)
      ) == 1L
    )
  })
  # Bad comparison functions

  local(
    {
      temp.bad.comp <- paste0(tempfile(), ".R")
      on.exit(unlink(temp.bad.comp))
      cat("
        unitizer_sect(
          'bad comp', {1 + 1; 2 + 2},
          compare=function(x, y) list('failed', 'comparison')
        )\n", file=temp.bad.comp
      )
      unitizer:::capture_output(unitize(temp.bad.comp, auto.accept="new"))
      unitizer:::read_line_set_vals(c('Q'))
      bad.comp.capt <- unitizer:::capture_output(
        unitize(temp.bad.comp, interactive.mode=TRUE)
      )
    },
    envir=environment()
  )
  test_that("Force", {
    expect_true(
      sum(grepl('Unable to compare value', bad.comp.capt$message)) == 1L
    )
    expect_true(
      sum(grepl('Corrupted', bad.comp.capt$output)) >= 1L
    )
  })
  # Bad store mapping functions

  test_that("bad map", {
    expect_error(
      capture.output(
        unitize_dir(test.dir, store.ids=function(x) stop("Bad store map fun")),
        type="message"
      ),
      "attempting to use it to convert test file"
    )
  })
})
