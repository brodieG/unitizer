library(unitizer)

x <- readRDS("helper/trivial.unitizer/data.rds")

test_that("Rename Works", {
  x.edit <- editCalls(x, quote(x), quote(y), interactive.only=FALSE)

  expect_identical(
    x.edit@items.ref.calls.deparse,
    vapply(unitizer:::as.list(x.edit@items.ref), slot, character(1L), "call.dep")
  )
  expect_true(
    !identical(x@items.ref.calls.deparse, x.edit@items.ref.calls.deparse)
  )
  expect_true(
    identical(
      x.edit@items.ref.calls.deparse,
      gsub("\\bx\\b", "y", x@items.ref.calls.deparse)
    )
  )
})
