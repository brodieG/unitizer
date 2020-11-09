if(!identical(basename(getwd()), "testthat"))
  stop("Working dir does not appear to be /testthat, is ", getwd())

rdsf <- function(x)
  file.path(getwd(), "helper", "exec", sprintf("%s.rds", x))

library(testthat)
library(unitizer)
context("Exec")

local( {
  suppressWarnings(glob <- unitizer:::unitizerGlobal$new())
  test_that("Invisible Expression", {
    e <- new.env()
    exp <- quote(x <- 1:30)
    expect_equal(1:30, unitizer:::eval_user_exp(exp, e, global=glob)$value)
  } )
  # `eval_user_exp` must be evaluated outside of test_that; also note that by
  # design this will output stuff to stderr and stdout

  out.err <- capture.output(type="message",
    out.std <- capture.output({
      test.obj.s3 <- structure("hello", class="test_obj")
      setClass("testObj", list(a="character"))
      test.obj.s4 <- new("testObj", a="goodday")
      print.test_obj <- function(x, ...) stop("Error in Print")
      setMethod("show", "testObj", function(object) stop("Error in Show"))
      fun_signal <- function()
        signalCondition(simpleError("Error in Function", sys.call()))
      fun_error <- function() stop("Error in function 2")
      fun_error_cond <- function()
        stop(simpleError("Error in function 2", sys.call()))
      fun_error_cond_call <- function() fun_error_cond()
      fun_s3 <- function() test.obj.s3
      fun_s4 <- function() test.obj.s4
      fun_msg <- function() message("This is a Message")
      fun_warn <- function() warning("This is a warning", immediate. = TRUE)
      eval.env <- sys.frame(sys.nframe())

      ex0 <- unitizer:::eval_user_exp(quote(stop()), eval.env, global=glob)
      unitizer:::set_trace(ex0$trace)
      trace0 <- traceback()
      ex1 <-
        unitizer:::eval_user_exp(quote(fun_signal()), eval.env, global=glob)
      unitizer:::set_trace(ex1$trace)
      trace1 <- traceback()
      ex2 <- unitizer:::eval_user_exp(quote(fun_error()), eval.env, global=glob)
      unitizer:::set_trace(ex2$trace)
      trace2 <- traceback()
      ex2a <-
        unitizer:::eval_user_exp(expression(fun_error()), eval.env, global=glob)
      unitizer:::set_trace(ex2a$trace)
      trace2a <- traceback()
      ex6 <-
        unitizer:::eval_user_exp(quote(fun_error_cond()), eval.env, global=glob)
      unitizer:::set_trace(ex6$trace)
      trace6 <- traceback()
      ex7 <- unitizer:::eval_user_exp(
        quote(fun_error_cond_call()), eval.env, global=glob
      )
      unitizer:::set_trace(ex7$trace)
      trace7 <- traceback()
      ex3 <- unitizer:::eval_user_exp(quote(fun_s3()), eval.env, global=glob)
      unitizer:::set_trace(ex3$trace)
      trace3 <- traceback()
      ex3a <-
        unitizer:::eval_user_exp(expression(fun_s3()), eval.env, global=glob)
      unitizer:::set_trace(ex3a$trace)
      trace3a <- traceback()
      ex4 <- unitizer:::eval_user_exp(quote(fun_s4()), eval.env, global=glob)
      ex4a <-
        unitizer:::eval_user_exp(expression(fun_s4()), eval.env, global=glob)
      unitizer:::set_trace(ex4a$trace)
      trace4a <- traceback()
      ex5 <- unitizer:::eval_user_exp(quote(sum(1:20)), eval.env, global=glob)
      ex9 <- unitizer:::eval_user_exp(quote(fun_warn()), eval.env, global=glob)
      ex10 <- unitizer:::eval_user_exp(quote(fun_msg()), eval.env, global=glob)
      ex11 <- unitizer:::eval_user_exp(
        quote((function() quote(stop("shouldn't error")))()),
        eval.env, global=glob
      )
  }) )
  # NOTE: deparsed test values generated with unitizer:::deparse_mixed

  test_that("User Expression Evaluation", {
    # a condition error, signaled, not stop (hence no aborted, etc.)
    expect_equal_to_reference(ex1, rdsf(100))
    # a stop
    expect_equal_to_reference(ex2, rdsf(200))
    # ex3 and ex3a are a total PITA because the calls need to be manually copied
    # b/c they don't deparse properly even with control="all", the trace and
    # call component loose the `structure` part in the quoted portions...

    # a stop in print;
    expect_equal_to_reference(ex3, rdsf(300))
    expect_equal_to_reference(ex3a, rdsf(400))

    # S4 objects; these originally caused problems since they don't deparse

    expect_equal_to_reference(ex4, rdsf(500))
    expect_equal_to_reference(ex4a, rdsf(600))

    # a normal expression

    expect_equal_to_reference(ex5, rdsf(700))
    expect_equal_to_reference(ex9, rdsf(800))
    expect_equal_to_reference(ex10, rdsf(900))
    expect_false(ex11$aborted)
  } )
  test_that("Trace Setting", {
    expect_identical(trace0, trace1)
    expect_identical(trace2, list("stop(\"Error in function 2\")", "fun_error()"))
    expect_identical(trace6, list("stop(simpleError(\"Error in function 2\", sys.call()))", "fun_error_cond()"))
    expect_identical(trace7, list("stop(simpleError(\"Error in function 2\", sys.call()))", "fun_error_cond()", "fun_error_cond_call()"))
    expect_identical(trace3a, list("stop(\"Error in Print\")", "print.test_obj(\"hello\")", "print(\"hello\")"))
    # needed to tweak this one so it would pass in R-devel 3.4.1
    expect_true(
      all(
        mapply(
          function(x, y) grepl(y, x),
          trace4a,
          list(
            "stop\\(\"Error in Show\"\\)",
            "show\\(.*\"testObj\".*\\)",
            "show\\(.*\"testObj\".*\\)"
    ) ) ) )
  } )
  old.width <- options(width=80L)
  a <- unitizer:::eval_with_capture(expression(stop("short stop message")), global=glob)
  b <- unitizer:::eval_with_capture(expression(stop("short stop .* with regex message")), global=glob)
  c <- unitizer:::eval_with_capture(expression(stop("this is a long error message that is supposed to cause R to add a new line after the error: part")), global=glob)

  d <- unitizer:::eval_with_capture(expression(warning("short warning message")), global=glob)
  e <- unitizer:::eval_with_capture(expression(warning("short warning message .* with regex")), global=glob)
  f <- unitizer:::eval_with_capture(expression(warning("this is a long error message that is supposed to cause R to add a new line after the error: part")), global=glob)

  g <- unitizer:::eval_with_capture(quote(stop("short stop message")), global=glob)
  h <- unitizer:::eval_with_capture(quote(stop("short stop .* with regex message")), global=glob)
  i <- unitizer:::eval_with_capture(quote(stop("this is a long error message that is supposed to cause R to add a new line after the error: part")), global=glob)

  j <- unitizer:::eval_with_capture(quote(warning("short warning message")), global=glob)
  k <- unitizer:::eval_with_capture(quote(warning("short warning message .* with regex")), global=glob)
  l <- unitizer:::eval_with_capture(quote(warning("this is a long error message that is supposed to cause R to add a new line after the error: part")), global=glob)

  m <- unitizer:::eval_with_capture(expression("a" / 3), global=glob)

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
  x <- unitizer:::eval_with_capture(exp.q, global=glob)
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
  y <- unitizer:::eval_with_capture(exp.exp, global=glob)
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
    # `sub` needed due to inconsistencies in R 3.4 and 3.3 for top level error
    # messages
    expect_equal(
      sub("\\bError.*: ", "", x$message),
      "boo hay \n there \n\nWarning:\n  this is a fairly long warning wladsfasdfasd that might wrap if we keep typing humpty dumpty sat on a wall and had a big fall\nWarning: ashorter warning blah\nboo hay \n there \n\nWarning: \nWarning in fun() : error in fun\nboo hay \n there \n\ncould not find function \"error\"\n"
    )
    expect_equal(
      sub("\\bError.*: ", "", y$message),
      "boo hay \n there \n\nWarning:\n  this is a fairly long warning wladsfasdfasd that might wrap if we keep typing humpty dumpty sat on a wall and had a big fall\nWarning: ashorter warning blah\nboo hay \n there \n\nWarning: \nWarning in fun() : error in fun\nboo hay \n there \n\ncould not find function \"error\"\n"
    )
  })
} )

