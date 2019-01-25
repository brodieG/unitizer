library(testthat)
library(unitizer)
context("In Package")

if(!file_test("-d", file.path("helper", "refobjs")))
  stop("Make sure wd is set to tests/testthat")

rdsf <- function(x)
  file.path(getwd(), "helper", "refobjs", sprintf("%s.rds", x))

local({
  update_fastlm(.unitizer.fastlm, version="0.1.0")
  install.packages(.unitizer.fastlm, repos=NULL, type='src', quiet=TRUE)
  base.dir <- file.path(.unitizer.fastlm, "tests", "extra")
  in.pkg.file <- file.path(base.dir, "inpkg.R")
  old.opt <- options(width=80L)
  on.exit({
    options(old.opt)
    unitizer:::read_line_set_vals(NULL)
  })
  unitizer:::read_line_set_vals(c("Q"))
  txt1 <- unitizer:::capture_output(
    unitize(in.pkg.file, interactive.mode=TRUE)
  )
  # `sub` needed due to inconsistencies in R 3.4 and 3.3 for top level error
  # messages
  txt1$message <- sub("^Error.*:", "", txt1$message)
  unitizer:::read_line_set_vals(c("Q"))
  txt2 <- unitizer:::capture_output(
    unitize(in.pkg.file, state=in_pkg(), interactive.mode=TRUE)
  )
  unitizer:::read_line_set_vals(c("Q"))
  txt3 <- unitizer:::capture_output(
    try(
      unitize(
        in.pkg.file, state=in_pkg("ASDFASDFA"), interactive.mode=TRUE
  ) ) )
  test_that("in_pkg", {
    expect_equal_to_reference(txt1, rdsf("inpkg_txt1"))
    expect_equal_to_reference(txt2, rdsf("inpkg_txt2"))
    expect_equal_to_reference(txt3, rdsf("inpkg_txt3"))
  })
})
