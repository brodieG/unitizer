library(unitizer)
context("Handled Runs")

test_that("Ensure we get warning if we try to run in handlers", {
  expect_warning(try(unitize("helper/trivial.R")), "running unitizer inside an error handling function")
  expect_warning(tryCatch(unitize("helper/trivial.R")), "running unitizer inside an error handling function")
  expect_warning(withRestarts(unitize("helper/trivial.R")), "running unitizer inside an error handling function")
  expect_warning(withCallingHandlers(unitize("helper/trivial.R")), "running unitizer inside an error handling function")
} )
# need to figure out why running this without `try` in covr causes cover to
# fail with
# Error in aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...) :
#   no rows to aggregate

err <- withRestarts(
  try(unitize("helper/trivial.R")), unitizerQuitExit=function() NULL
)
test_that("Ensure we get error if we try to do something stupid...", {
  expect_is(err, "try-error")
} )
