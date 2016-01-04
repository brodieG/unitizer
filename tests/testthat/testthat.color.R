old.color <- options(unitizer.color=TRUE)
test_that("colors working", {
  expect_identical(unitizer:::clr("hello", "red"), "\033[31mhello\033[39m")
  expect_identical(
    unitizer:::clr(letters[1:3], "red"),
    c("\033[31ma\033[39m", "\033[31mb\033[39m", "\033[31mc\033[39m")
  )
  expect_identical(
    clrd <- unitizer:::clr(letters[1:3], c("red", NA, "green")),
    c("\033[31ma\033[39m", "b", "\033[32mc\033[39m")
  )
  expect_error(unitizer:::clr(letters[1:3], c("red", "green")), "color")
  expect_identical(unitizer:::has_style(clrd), c(TRUE, FALSE, TRUE))
  expect_identical(unitizer:::has_style(letters[1:3]), rep(FALSE, 3L))
  expect_identical(unitizer:::strip_style(clrd), letters[1:3])
} )
test_that("word coloring", {
  tar <- c("abcd", "efgh", "jKLm", "")
  cur <- c("abCd", "", "JKL", "abc")
  expect_equal(
    res <- unitizer:::diff_word(tar, cur),
    structure(list(target = c("\033[31mabcd\033[39m", "\033[31mefgh\033[39m", "\033[31mjKLm\033[39m", ""), current = c("\033[32mabCd\033[39m", "", "\033[32mJKL\033[39m", "\033[32mabc\033[39m")), .Names = c("target", "current"))
  )
  # invisible(lapply(res, cat, sep="\n"))
})
color <- options(old.color)
