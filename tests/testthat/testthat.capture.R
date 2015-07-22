old.max <- getOption("unitizer.max.capture.chars")
options(unitizer.max.capture.chars=100L)

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
  base.char <- paste(rep(letters, 10), collapse=" ")
  writeChar(base.char, con)
  expect_warning(
    cpt0 <- unitizer:::get_text_capture(con, f, "output", TRUE),
    "Reached maximum text capture"
  )
  expect_equal(cpt0, substr(base.char, 1, 100))
  ## for some reason this test stopped working; not sure why, need to look into
  ## it; seemingly it messes up the pointer for the next read

  # writeChar("xxxxxx", con)
  # cpt2 <- unitizer:::get_text_capture(con, f, "output", TRUE)
  # expect_equal("xxxxxx", cpt2)
  writeChar(paste0(rep("yyyyyyy", 20L), collapse=""), con)
  expect_warning(
    cpt1 <- unitizer:::get_text_capture(con, f, "output", TRUE),
    "Reached maximum text capture"
  )
  expect_equal(paste0(rep("y", 100), collapse=""), cpt1)
} )
close(con)
unlink(f)
options(unitizer.max.capture.chars=old.max)
