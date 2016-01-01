test_that("colors working", {
  old.color <- options(unitizer.color=TRUE)
  expect_identical( unitizer:::clr("hello", "red"), "\033[31mhello\033[39m")
  color <- options(old.color)
} )
