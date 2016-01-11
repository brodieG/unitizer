# Overflow tests from testthat.unitize.R

# devtools::install(); library(testthat); library(unitizer); setwd("tests/testthat")

if(!file_test("-d", file.path("helper", "refobjs")))
  stop("Make sure wd is set to tests/testthat")

library(unitizer)
library(testthat)
old.opt.outer <- options(unitizer.color=FALSE, width=80L)
context("Unitize 2\n")

(.unitizer.fastlm <- copy_fastlm_to_tmpdir())    # package directory
devtools::install(.unitizer.fastlm, quiet=TRUE)  # install first version
.unitizer.test.file <- file.path(.unitizer.fastlm, "tests", "unitizer", "fastlm1.R")
.unitizer.test.store <- file.path(.unitizer.fastlm, "tests", "unitizer", "fastlm1.unitizer")
test.dir <- file.path(.unitizer.fastlm, "tests", "unitizer")

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
    txt0, file.path("helper", "refobjs", "unitize2_unreview.rds")
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
# Make sure that deleted items from a section are still marked from that section
# upgrade to version two to use the files that are set up for that there

unitizer_cleanup_demo()

(.unitizer.fastlm <- copy_fastlm_to_tmpdir())    # package directory
unitizer:::update_fastlm_extra(.unitizer.fastlm)
devtools::install(.unitizer.fastlm)

.unitizer.test.file <- file.path(
  .unitizer.fastlm, "tests", "unitizer", "unitizer.fastlm.R"
)
.unitizer.test.file.2 <- file.path(
  .unitizer.fastlm, "tests", "unitizer", "unitizer.fastlm2.R"
)
.unitizer.test.store <- file.path(
  .unitizer.fastlm, "tests", "unitizer", "fastlm1.unitizer"
)
# First auto accept all initial tests, and then re-run with second version to
# make sure deleted tests are where we think they should be

unitize(.unitizer.test.file, .unitizer.test.store, auto.accept="new")
unitizer:::read_line_set_vals(c("B", "Q"))
untz.2 <- unitize(.unitizer.test.file.2, .unitizer.test.store)

test_that("Section Extra", {
  expect_equal_to_reference(
    `attributes<-`(untz.2, NULL),
    file.path("helper", "refobjs", "unitize2_del_sec.rds")
  )
} )

unitizer_cleanup_demo()
unitizer:::read_line_set_vals(NULL)
options(old.opt.outer)
