library(unitizer)
context("Upgrade")
test_that("Upgrade works", {
  # this is also now tested as part of load
  unitizer <- get_unitizer("helper/trivial.unitizer.0.4.2")
  expect_error(validObject(unitizer, complete=TRUE))
  expect_equal(as.character(unitizer@version), "0.4.2")

  expect_warning(
    unitizer.up <- unitizer:::upgrade_internal(unitizer),
    "Slot .* does not exist"
  )
  expect_true(validObject(unitizer.up))
  expect_equal(unitizer.up@version, as.character(packageVersion("unitizer")))
})
test_that("Rename", {
  setClass("untzUpgrTest", slots=c(a="character"))
  x <- new("untzUpgrTest", a=letters)
  expect_true(validObject(x))
  setClass("untzUpgrTest", slots=c(b="character"))
  expect_error(validObject(x))
  expect_error(
    capture.output(
      unitizer:::renameSlot(x, "c", "b"), type="message"
    ), "Old slot `c` doesn't"
  )
  x.rename <- unitizer:::renameSlot(x, "a", "b")
  expect_true(validObject(x.rename))
})
test_that("Later but valid version", {
  tmpdir <- tempfile()
  on.exit(unlink(tmpdir, recursive=TRUE))
  dir.create(tmpdir)
  test.file <- file.path(tmpdir, "tests.R")
  cat("1 + 1", file=test.file)
  unitizer:::capture_output(unitize(test.file, auto.accept="new"))

  version <-
    unlist(strsplit(as.character(packageVersion('unitizer')), ".", fixed=TRUE))
  version[1] <- as.character(as.numeric(version[1]) + 1)
  version.new <- paste0(version, collapse=".")

  unitizer.rds <- readRDS(file.path(tmpdir, "tests.unitizer", "data.rds"))
  unitizer.rds@version <- version.new

  # this should work

  expect_true(!nchar(unitizer:::unitizer_valid(unitizer.rds)))

  # now lets cause an error

  unitizer.rds@eval.time <- runif(5)
  expect_match(unitizer:::unitizer_valid(unitizer.rds), "NB: ")
})
