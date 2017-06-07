library(unitizer)
context("Rename")

test_that("Rename Works", {
  x <- readRDS("helper/trivial.unitizer/data.rds")
  expect_warning(
    x.edit <- editCalls(x, quote(x), quote(y), interactive.only=FALSE),
    "experimental"
  )
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
  unitizer:::read_line_set_vals("Y")
  expect_warning(
    capture.output(
      x.edit2 <- editCalls(x, quote(x), quote(y), interactive.only=TRUE)
    ),
    "experimental"
  )
  unitizer:::read_line_set_vals("N")
  expect_message(
    capture.output(
      x.edit3 <- editCalls(x, quote(x), quote(y), interactive.only=TRUE)
    ),
    "Exiting without edits"
  )
  expect_identical(x.edit3, x)

  unitizer:::read_line_set_vals(NULL)
  expect_identical(
    x.edit@items.ref.calls.deparse, x.edit2@items.ref.calls.deparse
  )
})
