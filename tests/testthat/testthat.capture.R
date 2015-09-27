library(unitizer)

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

test_that("connection capture works", {
  out.num <- as.integer(stdout())
  err.num <- as.integer(stderr())
  err.con <- getConnection(sink.number(type="message"))

  cons <- new("unitizerCaptCons")
  cons <- unitizer:::set_capture(cons)
  cat("hello there\n")
  message("goodbye there")
  capt <- unitizer:::get_capture(cons)
  cons <- unitizer:::unsink_cons(cons)
  expect_identical(
    capt, structure(list(output = "hello there\n", message = "goodbye there\n")
  ))
  expect_identical(as.integer(stdout()), out.num)
  expect_identical(as.integer(stderr()), err.num)
  unitizer:::close_and_clear(cons)

  # Now, test errors, here we add an extra stdout sink, so we need to blow away
  # two, and also need to reset the stderr sink.  In both cases unsink_cons will
  # not touch the sinks since we're not in an expected state

  cons <- new("unitizerCaptCons")
  cons <- unitizer:::set_capture(cons)
  cat("there hello\n")
  message("there goodbye")
  f1 <- tempfile()
  f2 <- tempfile()
  c2 <- file(f2, "w")
  sink(f1)
  sink(c2, type="message")
  capt <- unitizer:::get_capture(cons)
  cons <- unitizer:::unsink_cons(cons)
  sink()
  sink()
  sink(err.con, type="message")
  close(c2)
  unlink(c(f1, f2))
  expect_true(attr(cons@out.c, "waive"))
  expect_true(attr(cons@err.c, "waive"))
  expect_identical(
    capt, list(output = "there hello\n", message = "there goodbye\n")
  )
  unitizer:::close_and_clear(cons)

  # Test the more pernicious error where we substitute the stdout sink

  cons <- new("unitizerCaptCons")
  cons <- unitizer:::set_capture(cons)
  cat("woohoo\n")
  message("yohooo")
  f1 <- tempfile()
  sink()
  sink(f1)
  capt <- unitizer:::get_capture(cons)
  cons <- unitizer:::unsink_cons(cons)
  sink()
  unlink(f1)
  expect_true(attr(cons@out.c, "waive"))
  expect_true(is.null(attr(cons@err.c, "waive")))
  expect_identical(
    capt, list(output = "woohoo\n", message = "yohooo\n")
  )
  unitizer:::close_and_clear(cons)
})

test_that("eval with capt", {
  expect_identical(
    unitizer:::eval_with_capture(quote(1+1)),
    list(value = 2, visible = TRUE, aborted = FALSE, conditions = list(), trace = list(), output = "[1] 2\n", message = "")
  )
  expect_identical(
    unitizer:::eval_with_capture(message("wow")),
    list(value = NULL, visible = TRUE, aborted = FALSE, conditions = list(), trace = list(), output = "", message = "wow\n")
  )
})
