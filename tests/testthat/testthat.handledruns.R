library(unitizer)

test_that("Ensure we get warning if we try to run in handlers", {
  expect_warning(try(unitize("helper/trivial.R")))
  expect_warning(tryCatch(unitize("helper/trivial.R")))
  expect_warning(withRestarts(unitize("helper/trivial.R")))
  expect_warning(withCallingHandlers(unitize("helper/trivial.R")))
} )
test_that("Ensure we get error if we try to do something stupid...", {
  expect_error(withRestarts(unitize("helper/trivial.R"), unitizerQuitExit=function() NULL))
} )
