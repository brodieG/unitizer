library(unitizer)
test_that("Upgrade works", {
  # this is also now tested as part of load
  unitizer <- get_unitizer("helper/trivial.unitizer.0.4.2")
  expect_error(validObject(unitizer, complete=TRUE))
  expect_equal(as.character(unitizer@version), "0.4.2")

  unitizer.up <- unitizer:::upgrade_internal(unitizer)
  expect_true(validObject(unitizer.up))
  expect_equal(unitizer.up@version, as.character(packageVersion("unitizer")))
})

