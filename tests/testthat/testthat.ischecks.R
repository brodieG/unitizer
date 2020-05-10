library(unitizer)
context("is checks")

test_that("int.pos", {
  expect_false(unitizer:::is.int.pos.1L(c(1, 2, 3)))
  expect_true(unitizer:::is.int.pos.1L(1))
  expect_true(unitizer:::is.int.pos.1L(1.0000))
  expect_false(unitizer:::is.int.pos.1L(-1))
  expect_false(unitizer:::is.int.pos.1L(NA_integer_))

  expect_true(unitizer:::is.int.pos.2L(1:2))
  expect_true(unitizer:::is.int.pos.2L(c(1, 2)))
})

test_that("is.valid_two_arg", {
  f1 <- function(x, y) NULL
  f2 <- function(...) NULL
  f3 <- function(x, ...) NULL
  f4 <- function(x, y, z) NULL
  f5 <- function(x, y, z=3) NULL
  f6 <- function(x) NULL

  expect_true(unitizer:::is.two_arg_fun(f1))
  expect_true(unitizer:::is.two_arg_fun(f2))
  expect_true(unitizer:::is.two_arg_fun(f3))
  expect_match(unitizer:::is.two_arg_fun(f4), "any non-optional arguments")
  expect_true(unitizer:::is.two_arg_fun(f5))
  expect_match(unitizer:::is.two_arg_fun(f6), "does not have at least")
  expect_match(unitizer:::is.two_arg_fun(1), "is not a function")
})
test_that("is.valid_capt_setting", {
  expect_message(
    capt.test <- unitizer:::is.valid_capt_setting(c(T, T)), "value must be"
  )
  expect_false(capt.test)
})
