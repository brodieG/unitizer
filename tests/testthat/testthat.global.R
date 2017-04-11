library(testthat)
context("Global")

# Most tests involving global are scattered all over the place, just putting a
# few extra ones that are very specifically about global here

test_that("Singleton Implementation Working", {
  expect_warning(
    unitizer:::unitizerGlobal$new(),
    "global object without global namespace"
  )
  glob.first <- unitizer:::unitizerGlobal$new(set.global=TRUE)
  expect_error(
    unitizer:::unitizerGlobal$new(set.global=TRUE),
    "global tracking object already exists"
  )
  expect_error(
    unitizer:::unitizerGlobal$new(),
    "global tracking object already exists"
  )
  glob.first$release()
})
test_that("Dummy Display", {
  expect_equal(
    capture.output(show(new('unitizerDummy'))), "<object not recorded>"
  )
})
test_that("Disable / Enable", {
  suppressWarnings(glob <- unitizer:::unitizerGlobal$new())
  glob$disable()
  expect_warning(glob$enable(c(search.path=2L)), "State setting")
})

