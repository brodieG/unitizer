library(unitizer)
context("Upgrade")
test_that("Upgrade works", {
  # this is also now tested as part of load
  unitizer <- get_unitizer("helper/trivial.unitizer.0.4.2")
  expect_error(validObject(unitizer, complete=TRUE))
  expect_equal(as.character(unitizer@version), "0.4.2")

  unitizer.up <- unitizer:::upgrade_internal(unitizer)
  expect_true(validObject(unitizer.up))
  expect_equal(unitizer.up@version, as.character(packageVersion("unitizer")))
})
test_that("Rename", {
  setClass("untzUpgrTest", slots=c(a="character"))
  x <- new("untzUpgrTest", a=letters)
  expect_true(validObject(x))
  setClass("untzUpgrTest", slots=c(b="character"))
  expect_error(validObject(x))
  expect_error(unitizer:::renameSlot(x, "c", "b"), "Old slot `c` doesn't")
  x.rename <- unitizer:::renameSlot(x, "a", "b")
  expect_true(validObject(x.rename))
})
