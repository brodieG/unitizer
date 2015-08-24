library(testthat)
library(unitizer)

local( {
  test_that("Invisible Expression", {
    e <- new.env()
    exp <- quote(x <- 1:30)
    expect_equal(1:30, unitizer:::eval_user_exp(exp, e)$value)
  } )
  # `eval_user_exp` must be evaluated outside of test_that; also note that by
  # design this will output stuff to stderr and stdout

  test.obj.s3 <- structure("hello", class="test_obj")
  setClass("testObj", list(a="character"))
  test.obj.s4 <- new("testObj", a="goodday")
  print.test_obj <- function(x, ...) stop("Error in Print")
  setMethod("show", "testObj", function(object) stop("Error in Show"))
  fun_signal <- function() signalCondition(simpleError("Error in Function", sys.call()))
  fun_error <- function() stop("Error in function 2")
  fun_error_cond <- function() stop(simpleError("Error in function 2", sys.call()))
  fun_error_cond_call <- function() fun_error_cond()
  fun_s3 <- function() test.obj.s3
  fun_s4 <- function() test.obj.s4
  fun_msg <- function() message("This is a Message")
  fun_warn <- function() warning("This is a warning", immediate. = TRUE)
  eval.env <- sys.frame(sys.nframe())

  ex0 <- unitizer:::eval_user_exp(quote(stop()), eval.env)
  unitizer:::set_trace(ex0$trace)
  trace0 <- traceback()
  ex1 <- unitizer:::eval_user_exp(quote(fun_signal()), eval.env)
  unitizer:::set_trace(ex1$trace)
  trace1 <- traceback()
  ex2 <- unitizer:::eval_user_exp(quote(fun_error()), eval.env)
  unitizer:::set_trace(ex2$trace)
  trace2 <- traceback()
  ex2a <- unitizer:::eval_user_exp(expression(fun_error()), eval.env)
  unitizer:::set_trace(ex2a$trace)
  trace2a <- traceback()
  ex6 <- unitizer:::eval_user_exp(quote(fun_error_cond()), eval.env)
  unitizer:::set_trace(ex6$trace)
  trace6 <- traceback()
  ex7 <- unitizer:::eval_user_exp(quote(fun_error_cond_call()), eval.env)
  unitizer:::set_trace(ex7$trace)
  trace7 <- traceback()
  ex3 <- unitizer:::eval_user_exp(quote(fun_s3()), eval.env)
  unitizer:::set_trace(ex3$trace)
  trace3 <- traceback()
  ex3a <- unitizer:::eval_user_exp(expression(fun_s3()), eval.env)
  unitizer:::set_trace(ex3a$trace)
  trace3a <- traceback()
  ex4 <- unitizer:::eval_user_exp(quote(fun_s4()), eval.env)
  ex4a <- unitizer:::eval_user_exp(expression(fun_s4()), eval.env)
  unitizer:::set_trace(ex4a$trace)
  trace4a <- traceback()
  ex5 <- unitizer:::eval_user_exp(quote(sum(1:20)), eval.env)
  ex9 <- unitizer:::eval_user_exp(quote(fun_warn()), eval.env)
  ex10 <- unitizer:::eval_user_exp(quote(fun_msg()), eval.env)
  ex11 <- unitizer:::eval_user_exp(
    quote((function() quote(stop("shouldn't error")))()), eval.env
  )

  # NOTE: deparsed test values generated with unitizer:::deparse_mixed

  test_that("User Expression Evaluation", {
    expect_equal(
      ex1,   # a condition error, signaled, not stop (hence no aborted, etc.)
      structure(list(value = NULL, visible = TRUE, aborted = FALSE, conditions = list(structure(list(message = "Error in Function", call = quote(fun_signal())), .Names = c("message", "call"), class = c("simpleError", "error", "condition"), unitizer.printed = FALSE)), trace = list()), .Names = c("value", "visible", "aborted", "conditions", "trace"))
    )
    expect_equal(
      ex2,   # a stop
      structure(list(value = NULL, visible = NULL, aborted = structure(TRUE, printed = FALSE), conditions = list(structure(list(message = "Error in function 2", call = quote(fun_error())), .Names = c("message", "call"), class = c("simpleError", "error", "condition"), unitizer.printed = FALSE)), trace = list(quote(fun_error()), quote(stop("Error in function 2")))), .Names = c("value", "visible", "aborted", "conditions", "trace"))
    )
    # ex3 and ex3a are a total PITA because the calls need to be manually copied
    # b/c they don't deparse properly even with control="all", the trace and
    # call component loose the `structure` part in the quoted portions...

    expect_equal(
      ex3,   # a stop in print;
      structure(list(value = structure("hello", class = "test_obj"), visible = TRUE, aborted = structure(TRUE, printed = TRUE), conditions = list(structure(list(message = "Error in Print", call = quote(print.test_obj(structure("hello", class = "test_obj")))), .Names = c("message", "call"), class = c("simpleError", "error", "condition"), unitizer.printed = TRUE)), trace = list(quote(print(structure("hello", class = "test_obj"))), quote(print.test_obj(structure("hello", class = "test_obj"))), quote(stop("Error in Print")))), .Names = c("value", "visible", "aborted", "conditions", "trace"))
    )
    expect_equal(
      ex3a,
      structure(list(value = structure("hello", class = "test_obj"), visible = TRUE, aborted = structure(TRUE, printed = TRUE), conditions = list(structure(list(message = "Error in Print", call = quote(print.test_obj(structure("hello", class = "test_obj")))), .Names = c("message", "call"), class = c("simpleError", "error", "condition"), unitizer.printed = TRUE)), trace = list(quote(print(structure("hello", class = "test_obj"))), quote(print.test_obj(structure("hello", class = "test_obj"))), quote(stop("Error in Print")))), .Names = c("value", "visible", "aborted", "conditions", "trace"))
    )
    # Can't deparse S4 objects, especially now that we are correctly including
    # them as part of the call of the condition

    # expect_equal(
    #   structure(list(aborted = structure(TRUE, printed = TRUE), conditions = list(structure(list(message = "Error in Show", call = quote(show(unitizerTESTRES))), .Names = c("message", "call"), class = c("simpleError", "error", "condition"), printed = TRUE)), trace = list("stop(\"Error in Show\")", "show(fun_s4())", "show(fun_s4())")), .Names = c("aborted", "conditions", "trace")),
    #   ex4[-1L]   # a stop in show, have to remove 1L because S4 object doesn't deparse
    # )
    # expect_equal(
    #   structure(list(aborted = structure(TRUE, printed = TRUE), conditions = list(structure(list(message = "Error in Show", call = quote(show(unitizerTESTRES))), .Names = c("message", "call"), class = c("simpleError", "error", "condition"), printed = TRUE)), trace = list("stop(\"Error in Show\")", "show(fun_s4())", "show(fun_s4())")), .Names = c("aborted", "conditions", "trace")),
    #   ex4a[-1L]   # a stop in show, have to remove 1L because S4 object doesn't deparse
    # )
    expect_equal(
      ex5,   # a normal expression
      structure(list(value = 210L, visible = TRUE, aborted = FALSE, conditions = list(), trace = list()), .Names = c("value", "visible", "aborted", "conditions", "trace"))
    )
    expect_equal(
      ex9,
      structure(list(value = "This is a warning", visible = FALSE, aborted = FALSE, conditions = list(structure(list(message = "This is a warning", call = quote(fun_warn())), .Names = c("message", "call"), class = c("simpleWarning", "warning", "condition"), unitizer.printed = FALSE)), trace = list()), .Names = c("value", "visible", "aborted", "conditions", "trace"))
    )
    expect_equal(
      ex10,
      structure(list(value = NULL, visible = FALSE, aborted = FALSE, conditions = list(structure(list(message = "This is a Message\n", call = quote(message("This is a Message"))), .Names = c("message", "call"), class = c("simpleMessage", "message", "condition"), unitizer.printed = FALSE)), trace = list()), .Names = c("value", "visible", "aborted", "conditions", "trace"))
    )
    expect_false(ex11$aborted)
  } )
  test_that("Trace Setting", {
    expect_identical(trace0, trace1)
    expect_identical(trace2, list("stop(\"Error in function 2\")", "fun_error()"))
    expect_identical(trace6, list("stop(simpleError(\"Error in function 2\", sys.call()))", "fun_error_cond()"))
    expect_identical(trace7, list("stop(simpleError(\"Error in function 2\", sys.call()))", "fun_error_cond()", "fun_error_cond_call()"))
    expect_identical(trace3a, list("stop(\"Error in Print\")", "print.test_obj(\"hello\")", "print(\"hello\")"))
    expect_identical(trace4a, list("stop(\"Error in Show\")", "show(<S4 object of class \"testObj\">)", "show(<S4 object of class \"testObj\">)"))
  } )
  old.width <- options(width=80L)
  a <- unitizer:::eval_with_capture(expression(stop("short stop message")))
  b <- unitizer:::eval_with_capture(expression(stop("short stop .* with regex message")))
  c <- unitizer:::eval_with_capture(expression(stop("this is a long error message that is supposed to cause R to add a new line after the error: part")))

  d <- unitizer:::eval_with_capture(expression(warning("short warning message")))
  e <- unitizer:::eval_with_capture(expression(warning("short warning message .* with regex")))
  f <- unitizer:::eval_with_capture(expression(warning("this is a long error message that is supposed to cause R to add a new line after the error: part")))

  g <- unitizer:::eval_with_capture(quote(stop("short stop message")))
  h <- unitizer:::eval_with_capture(quote(stop("short stop .* with regex message")))
  i <- unitizer:::eval_with_capture(quote(stop("this is a long error message that is supposed to cause R to add a new line after the error: part")))

  j <- unitizer:::eval_with_capture(quote(warning("short warning message")))
  k <- unitizer:::eval_with_capture(quote(warning("short warning message .* with regex")))
  l <- unitizer:::eval_with_capture(quote(warning("this is a long error message that is supposed to cause R to add a new line after the error: part")))

  m <- unitizer:::eval_with_capture(expression("a" / 3))

  exp.q <- quote({
    fun <- function() warning("error in fun")
    message("boo hay \n there \n")
    warning("this is a fairly long warning wladsfasdfasd that might wrap if we keep typing humpty dumpty sat on a wall and had a big fall");
    warning("ashorter warning blah");
    message("boo hay \n there \n")
    warning();
    fun()
    suppressWarnings(warning("quiet warn"));
    message("boo hay \n there \n")
    error(3)
  })
  x <- unitizer:::eval_with_capture(exp.q)
  exp.exp <- expression({
    fun <- function() warning("error in fun")
    message("boo hay \n there \n")
    warning("this is a fairly long warning wladsfasdfasd that might wrap if we keep typing humpty dumpty sat on a wall and had a big fall");
    warning("ashorter warning blah");
    message("boo hay \n there \n")
    warning();
    fun()
    suppressWarnings(warning("quiet warn"));
    message("boo hay \n there \n")
    error(3)
  })
  y <- unitizer:::eval_with_capture(exp.exp)
  options(old.width)
  test_that("Clean Top Level Message", {
    expect_equal(a$message, "Error: short stop message\n")
    expect_equal(b$message, "Error: short stop .* with regex message\n")
    expect_equal(
      c$message,
      "Error: \n  this is a long error message that is supposed to cause R to add a new line after the error: part\n"
    )
    expect_equal(d$message, "Warning: short warning message\n")
    expect_equal(e$message, "Warning: short warning message .* with regex\n")
    expect_equal(
      f$message,
      "Warning:\n  this is a long error message that is supposed to cause R to add a new line after the error: part\n"
    )
    expect_equal(g$message, "Error: short stop message\n")
    expect_equal(h$message, "Error: short stop .* with regex message\n")
    expect_equal(
      i$message,
      "Error: \n  this is a long error message that is supposed to cause R to add a new line after the error: part\n"
    )
    expect_equal(j$message, "Warning: short warning message\n")
    expect_equal(k$message, "Warning: short warning message .* with regex\n")
    expect_equal(
      l$message,
      "Warning:\n  this is a long error message that is supposed to cause R to add a new line after the error: part\n"
    )
    expect_equal(m$message, "Error in \"a\"/3 : non-numeric argument to binary operator\n")
    expect_equal(
      x$message,
      "boo hay \n there \n\nWarning:\n  this is a fairly long warning wladsfasdfasd that might wrap if we keep typing humpty dumpty sat on a wall and had a big fall\nWarning: ashorter warning blah\nboo hay \n there \n\nWarning: \nWarning in fun() : error in fun\nboo hay \n there \n\nError: could not find function \"error\"\n"
    )
    expect_equal(
      y$message,
      "boo hay \n there \n\nWarning:\n  this is a fairly long warning wladsfasdfasd that might wrap if we keep typing humpty dumpty sat on a wall and had a big fall\nWarning: ashorter warning blah\nboo hay \n there \n\nWarning: \nWarning in fun() : error in fun\nboo hay \n there \n\nError: could not find function \"error\"\n"
    )
  })
} )

