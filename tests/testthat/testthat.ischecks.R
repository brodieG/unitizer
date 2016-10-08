library(unitizer)
context("is checks")

test_that("int.pos.1L", {
  expect_false(unitizer:::is.int.pos.1L(c(1, 2, 3)))
  expect_true(unitizer:::is.int.pos.1L(1))
  expect_true(unitizer:::is.int.pos.1L(1.0000))
  expect_false(unitizer:::is.int.pos.1L(-1))
  expect_false(unitizer:::is.int.pos.1L(NA_integer_))
})

test_that("valid_con", {
  f <- tempfile()
  cat("hello there\n", file=f)
  con <- file(f, "r")

  expect_true(unitizer:::is.valid_con(con))
  expect_true(unitizer:::is.valid_con(con, f))
  expect_error(unitizer:::is.valid_con(con, 200), "must be NULL or")
  expect_match(unitizer:::is.valid_con(con, "hello"), "does not match connection")

  expect_true(unitizer:::is.open_con(con, f))
  expect_true(unitizer:::is.open_con(con, f, readable=TRUE))
  expect_match(
    unitizer:::is.open_con(con, f, readable=FALSE), "should not be readable"
  )
  expect_match(
    unitizer:::is.open_con(con, f, writeable=TRUE), "should be writeable"
  )
  expect_true(unitizer:::is.open_con(stdout(), writeable=TRUE))
  expect_true(unitizer:::is.open_con(stderr(), writeable=TRUE))
  expect_true(unitizer:::is.open_con(stdin(), readable=TRUE))

  close(con)
  unlink(f)
})
