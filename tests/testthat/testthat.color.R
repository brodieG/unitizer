test_that("colors working", {
  old.color <- options(unitizer.color=TRUE)
  expect_identical(unitizer:::clr("hello", "red"), "\033[31mhello\033[39m")
  expect_identical(
    clrd <- unitizer:::clr(letters[1:3], c("red", NA, "green")),
    c("\033[31ma\033[39m", "b", "\033[32mc\033[39m")
  )
  expect_error(unitizer:::clr(letters[1:3], c("red", "green")), "color")
  expect_identical(unitizer:::has_style(clrd), c(TRUE, FALSE, TRUE))
  expect_identical(unitizer:::has_style(letters[1:3]), rep(FALSE, 3L))
  expect_identical(unitizer:::strip_style(clrd), letters[1:3])
  color <- options(old.color)
} )
