test_that("colors working", {
  old.color <- options(unitizer.color=TRUE)
  expect_identical(unitizer:::clr("hello", "red"), "\033[31mhello\033[39m")
  expect_identical(
    unitizer:::clr(letters[1:3], c("red", NA, "green")),
    c("\033[31ma\033[39m", "b", "\033[32mc\033[39m")
  )
  expect_error(unitizer:::clr(letters[1:3], c("red", "green")), "color")
  color <- options(old.color)
} )
