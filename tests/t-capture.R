source(file.path("_helper", "init.R"))
source(file.path("aammrtf", "ref.R")); make_ref_obj_funs("capture")

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

# - "get_capture" --------------------------------------------------------------

old.max <- options(unitizer.max.capture.chars = 100L)
cons <- new("unitizerCaptCons")
base.char <- paste(rep(letters, 10), collapse = " ")
writeChar(base.char, cons@out.c)

# Error "Argument `chrs.max`"
try(unitizer:::get_text_capture(cons, "output", TRUE, chrs.max = "howdy"))
# Warn max capt
cpt0 <- unitizer:::get_text_capture(cons, "output", TRUE)
nchar(cpt0)
base.char.2 <- paste(rev(rep(letters, 10)), collapse = " ")
writeChar(base.char.2, cons@err.c)
sink(cons@err.c, type = "message")

cpt0.err <- unitizer:::get_text_capture(cons, "message", FALSE)
sink(type = "message")
all.equal(cpt0.err, substr(base.char.2, 1, 100))

## for some reason this test stopped working; not sure why, need to look into
## it; seemingly it messes up the pointer for the next read
# writeChar("xxxxxx", cons@out.c)
# cpt2 <- unitizer:::get_text_capture(cons, "output", TRUE)
# expect_equal("xxxxxx", cpt2)
writeChar(paste0(rep("yyyyyyy", 20L), collapse = ""), cons@out.c)
# warn max capt
cpt1 <- unitizer:::get_text_capture(cons, "output", TRUE)
all.equal(cpt1, paste0(rep("y", 100), collapse = ""))
unitizer:::close_and_clear(cons)
options(old.max)

# - "get_text" -----------------------------------------------------------------

old.max <- options(unitizer.max.capture.chars = 100L)
f <- tempfile()
con <- file(f, "w+b")
base.char <- paste(letters, collapse = " ")
sink(con, type = "message")
cat(base.char, file = stderr())
# this needs to temporarily switch the sink to be able to issue the warning
# Warn: "Reached maximum"
unitizer:::get_text(con, 10)
# should still be to writing to our file, 10 chars in
cat("boogiewoogy", file = stderr())
sink(type = "message")
suppressWarnings(readLines(f))  # incomplete final line...

options(old.max)
unlink(f)

# - "connection capture works" -------------------------------------------------

out.num <- as.integer(stdout())
err.num <- as.integer(stderr())
err.con <- getConnection(sink.number(type = "message"))
cons <- new("unitizerCaptCons")
cons <- unitizer:::set_capture(cons)
cat("hello there\n")
cat("goodbye there\n", file = stderr())
capt <- unitizer:::get_capture(cons)
cons <- unitizer:::unsink_cons(cons)
capt
# expect_identical(as.integer(stdout()), out.num)
identical(as.integer(stdout()), out.num)
identical(as.integer(stderr()), err.num)
unitizer:::close_and_clear(cons)

# Now, here we add an extra stdout sink. In both cases unsink_cons will not
# touch the sinks since we're not in an expected state, leaving
# close_and_clear to cleanup
err.con <- getConnection(sink.number(type = "message"))
cons <- new("unitizerCaptCons")
cons <- unitizer:::set_capture(cons)
cat("there hello\n")
# message does not work with testthat
cat("there goodbye\n", file = stderr())
f1 <- tempfile()
f2 <- tempfile()
c2 <- file(f2, "w")
sink(f1)
sink(c2, type = "message")
cat("12 there hello\n")
# message does not work with testthat
cat("12 there goodbye\n", file = stderr())
capt <- unitizer:::get_capture(cons)
cons <- unitizer:::unsink_cons(cons)
unitizer:::close_and_clear(cons)
attr(cons@out.c, "waive")
attr(cons@err.c, "waive")
capt
readLines(f1)
readLines(f2)
close(c2)
unlink(c(f1, f2))

# Same, but this time close the sinks properly, so the connections should not
# be waived
err.con <- getConnection(sink.number(type = "message"))
cons <- new("unitizerCaptCons")
cons <- unitizer:::set_capture(cons)
cat("there hello\n")
# message does not work with testthat
cat("there goodbye\n", file = stderr())
f1 <- tempfile()
f2 <- tempfile()
c2 <- file(f2, "w")
sink(f1)
sink(c2, type = "message")
cat("12 there hello\n")
# message does not work with testthat
cat("12 there goodbye\n", file = stderr())
sink()
sink(cons@err.c, type = "message")
capt <- unitizer:::get_capture(cons)
cons <- unitizer:::unsink_cons(cons)
attr(cons@out.c, "waive")  # NULL
attr(cons@err.c, "waive")  # NULL
capt
unitizer:::close_and_clear(cons)
readLines(f1)
readLines(f2)
close(c2)
unlink(c(f1, f2))
# Try to mess up sink counter by replacing the real sink with a fake sink
# should lead to a waived connection
cons <- new("unitizerCaptCons")
cons <- unitizer:::set_capture(cons)
f1 <- tempfile()
sink()
sink(f1)
capt <- unitizer:::get_capture(cons)
cons <- unitizer:::unsink_cons(cons)
attr(cons@out.c, "waive")
attr(cons@err.c, "waive")
capt
# Try to fix so that we don't get a full stack release error
sink()
sink(cons@out.c)
unitizer:::close_and_clear(cons)
unlink(f1)
# helper function
f1 <- tempfile()
f2 <- tempfile()
c1 <- file(f1, "w+b")
c2 <- file(f2, "w+b")
sink(c2)
unitizer:::is_stdout_sink(c1)
sink()
try(unitizer:::is_stdout_sink(f1)) # error
sink(c1)
unitizer:::is_stdout_sink(c1)
sink()
close(c1)
close(c2)
unlink(c(f1, f2))

# - "connection breaking tests" ------------------------------------------------

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

# - "close_and_clear" ----------------------------------------------------------

# need some careful handling to make sure we don't mess up the testthat's
# sinking (legacy behavior)
cons <- new("unitizerCaptCons")
err.con <- cons@stderr.con
on.exit(sink(err.con, type = "message"))
# intended to cause an error
cons@stderr.con <- list()
# msg:  "Unable to restore original "
cons.txt <- capture.output(status <- unitizer:::close_and_clear(cons), 
    type = "message")
any(grepl("connection", cons.txt))
sink(err.con, type = "message")
status["message"]

# - "eval with capt" -----------------------------------------------------------

suppressWarnings(glob <- unitizer:::unitizerGlobal$new())
all.equal(
  (capt <- unitizer:::eval_with_capture(quote(1 + 1), global = glob))[1:8],
  rds(100)
)
is(capt[[9]], "unitizerCaptCons")
all.equal(
  (
    capt <- unitizer:::eval_with_capture(
      cat("wow\n", file = stderr()), global = glob)
  )[1:8],
  rds(200)
)
is(capt[[9]], "unitizerCaptCons")
