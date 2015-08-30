library(unitizer)

test_that("read_line works", {
  unitizer:::`.global$prompt.vals` <- letters[1:3]
  expect_identical(unitizer:::read_line(), "a")
  expect_identical(.global$prompt.vals, letters[2:3])
  expect_identical(unitizer:::read_line(), "b")
  expect_identical(.global$prompt.vals, letters[3])
  expect_identical(unitizer:::read_line(), "c")
  expect_identical(.global$prompt.vals, character())
  if(!interactive()) expect_identical(unitizer:::read_line(), "")
})
