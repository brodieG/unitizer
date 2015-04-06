old.max <- getOption("unitizer.max.capture.chars")
options(unitizer.max.capture.chars=20L)

# # Messing around trying to understand seek...
# f <- tempfile()
# con <- file(f, "w+b")
# writeChar(paste(letters, LETTERS, collapse=" "), con)
# readChar(con, 20)
# pos <- seek(con, origin="current")
# seek(con, pos, rw="write")
# writeChar("xxxxxxxxx", con)
# readChar(con, 3)
# pos <- seek(con, origin="current")
# seek(con, pos, rw="write")
# writeChar("yyyyy", con)
# close(con)
# readLines(f)
# unlink(f)

f <- tempfile()
con <- file(f, "w+b")
test_that("get_capture", {
  writeChar(paste(letters, LETTERS, collapse=" "), con)
  expect_warning(
    cpt0 <- unitizer:::get_text_capture(con, f, "output", TRUE),
    "Reached maximum text capture"
  )
  expect_equal("a A b B c C d D e E ", cpt0)
  writeChar("xxxxxx", con)
  expect_equal("xxxxxx", unitizer:::get_text_capture(con, f, "output", TRUE))
  writeChar(paste0(rep("yyyyyyy", 10L), collapse=""), con)
  expect_warning(
    cpt1 <- unitizer:::get_text_capture(con, f, "output", TRUE),
    "Reached maximum text capture"
  )
  expect_equal("yyyyyyyyyyyyyyyyyyyy", cpt1)
  expect_equal(20L, unique(nchar(c(cpt0, cpt1))))
} )
close(con)
unlink(f)
options(unitizer.max.capture.chars=old.max)
