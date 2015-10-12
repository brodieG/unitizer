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

test_that("get_capture", {
  cons <- new("unitizerCaptCons")
  base.char <- paste(rep(letters, 10), collapse=" ")
  writeChar(base.char, cons@out.c)
  expect_warning(
    cpt0 <- unitizer:::get_text_capture(cons, "output", TRUE),
    "Reached maximum text capture"
  )
  expect_equal(cpt0, substr(base.char, 1, 100))
  ## for some reason this test stopped working; not sure why, need to look into
  ## it; seemingly it messes up the pointer for the next read

  # writeChar("xxxxxx", con)
  # cpt2 <- unitizer:::get_text_capture(con, f, "output", TRUE)
  # expect_equal("xxxxxx", cpt2)
  writeChar(paste0(rep("yyyyyyy", 20L), collapse=""), cons@out.c)
  expect_warning(
    cpt1 <- unitizer:::get_text_capture(cons, "output", TRUE),
    "Reached maximum text capture"
  )
  expect_equal(paste0(rep("y", 100), collapse=""), cpt1)
  unitizer:::close_and_clear(cons)
} )
options(unitizer.max.capture.chars=old.max)

test_that("connection capture works", {
  out.num <- as.integer(stdout())
  err.num <- as.integer(stderr())
  err.con <- getConnection(sink.number(type="message"))

  cons <- new("unitizerCaptCons")
  cons <- unitizer:::set_capture(cons)
  cat("hello there\n")
  cat("goodbye there\n", file=stderr())
  capt <- unitizer:::get_capture(cons)
  cons <- unitizer:::unsink_cons(cons)
  expect_identical(
    capt, structure(list(output = "hello there\n", message = "goodbye there\n")
  ))
  expect_identical(as.integer(stdout()), out.num)
  expect_identical(as.integer(stderr()), err.num)
  expect_identical(
    unitizer:::close_and_clear(cons),
    structure(c(TRUE, TRUE), .Names = c("output", "message"))
  )
  # Now, here we add an extra stdout sink. In both cases unsink_cons will not
  # touch the sinks since we're not in an expected state, leaving
  # close_and_clear to cleanup

  err.con <- getConnection(sink.number(type="message"))
  cons <- new("unitizerCaptCons")
  cons <- unitizer:::set_capture(cons)
  cat("there hello\n")
  cat("there goodbye\n", file=stderr())  # message does not work with testthat
  f1 <- tempfile()
  f2 <- tempfile()
  c2 <- file(f2, "w")
  sink(f1)
  sink(c2, type="message")
  cat("12 there hello\n")
  cat("12 there goodbye\n", file=stderr())  # message does not work with testthat
  capt <- unitizer:::get_capture(cons)
  cons <- unitizer:::unsink_cons(cons)
  expect_identical(
    unitizer:::close_and_clear(cons),
    structure(c(TRUE, TRUE), .Names = c("output", "message"))
  )
  expect_true(attr(cons@out.c, "waive"))
  expect_true(attr(cons@err.c, "waive"))
  expect_identical(
    capt, list(output = "there hello\n", message = "there goodbye\n")
  )
  expect_equal(readLines(f1), "12 there hello")
  expect_equal(readLines(f2), "12 there goodbye")
  close(c2)
  unlink(c(f1, f2))

  # Same, but this time close the sinks properly, so the connections should not
  # be waived

  err.con <- getConnection(sink.number(type="message"))
  cons <- new("unitizerCaptCons")
  cons <- unitizer:::set_capture(cons)
  cat("there hello\n")
  cat("there goodbye\n", file=stderr())  # message does not work with testthat
  f1 <- tempfile()
  f2 <- tempfile()
  c2 <- file(f2, "w")
  sink(f1)
  sink(c2, type="message")
  cat("12 there hello\n")
  cat("12 there goodbye\n", file=stderr())  # message does not work with testthat
  sink()
  sink(cons@err.c, type="message")
  capt <- unitizer:::get_capture(cons)
  cons <- unitizer:::unsink_cons(cons)
  expect_null(attr(cons@out.c, "waive"))
  expect_null(attr(cons@err.c, "waive"))
  expect_identical(
    capt, list(output = "there hello\n", message = "there goodbye\n")
  )
  expect_identical(
    unitizer:::close_and_clear(cons),
    structure(c(TRUE, TRUE), .Names = c("output", "message"))
  )
  expect_equal(readLines(f1), "12 there hello")
  expect_equal(readLines(f2), "12 there goodbye")
  close(c2)
  unlink(c(f1, f2))
})
# # These tests cannot be run as they blow away the entire sink stack which can
# # mess up any testing done under capture
#
# test_that("connection breaking tests", {
#   # Test the more pernicious error where we substitute the stdout sink
#
#   cons <- new("unitizerCaptCons")
#   cons <- unitizer:::set_capture(cons)
#   cat("woohoo\n")
#   cat("yohooo\n", file=stderr())
#   f1 <- tempfile()
#   sink()
#   sink(f1)
#   capt <- unitizer:::get_capture(cons)
#   cons <- unitizer:::unsink_cons(cons)
#   sink()
#   unlink(f1)
#   expect_true(attr(cons@out.c, "waive"))
#   expect_null(attr(cons@err.c, "waive"))
#   expect_identical(
#     capt, list(output = "woohoo\n", message = "yohooo\n")
#   )
#   expect_identical(
#     unitizer:::close_and_clear(cons),
#     structure(c(FALSE, TRUE), .Names = c("output", "message"))
#   )
# })
test_that("eval with capt", {
  expect_identical(
    (capt <- unitizer:::eval_with_capture(quote(1+1)))[1:7],
    list(value = 2, visible = TRUE, aborted = FALSE, conditions = list(), trace = list(), output = "[1] 2\n", message = "")
  )
  expect_is(capt[[8]], "unitizerCaptCons")
  expect_identical(
    (capt <- unitizer:::eval_with_capture(cat("wow\n", file=stderr())))[1:7],
    list(value = NULL, visible = TRUE, aborted = FALSE, conditions = list(), trace = list(), output = "", message = "wow\n")
  )
  expect_is(capt[[8]], "unitizerCaptCons")
})
