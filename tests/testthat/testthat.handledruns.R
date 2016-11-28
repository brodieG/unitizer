library(unitizer)
context("Handled Runs")

if(!file_test("-d", file.path("helper", "refobjs")))
  stop("Make sure wd is set to tests/testthat")

test_that("Ensure we get warning if we try to run in handlers", {
  expect_warning(
    capture.output(
      capture.output(type="message", try(unitize("helper/trivial.R")))
    ),
    "running unitizer inside an error handling function"
  )
  expect_warning(
    capture.output(
      capture.output(type="message", tryCatch(unitize("helper/trivial.R")))
    ),
    "running unitizer inside an error handling function"
  )
  expect_warning(
    capture.output(
      capture.output(type="message",
        withRestarts(unitize("helper/trivial.R"))
    ) ),
    "running unitizer inside an error handling function"
  )
  expect_warning(
    capture.output(
      capture.output(type="message",
        withCallingHandlers(unitize("helper/trivial.R"))
    ) ),
    "running unitizer inside an error handling function"
  )
} )
# need to figure out why running this without `try` in covr causes cover to
# fail with
# Error in aggregate.data.frame(mf[1L], mf[-1L], FUN = FUN, ...) :
#   no rows to aggregate

test_that("Ensure we get error if we try to do something stupid...", {
  expect_error(
    expect_warning(
      capture.output(
        capture.output(type="message",
          withRestarts(
            unitize("helper/trivial.R"), unitizerInteractiveFail=function() NULL
      ) ) ),
      "appears you are running unitizer inside"
    ),
    "restart is already defined"
  )
} )
