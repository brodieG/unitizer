library(testthat)
library(testor)

local( {
  trace1 <- NULL
  trace2 <- NULL
  fun <- function() stop("Yowza")
  fun.call <- quote(fun())

  withRestarts( # has to be outside test_that
    withCallingHandlers(
      {
        trace.base <- sys.calls()
        withVisible(eval(fun.call))
      },
      error=function(cond) {
        trace1 <<- testor:::get_trace(trace.base, sys.calls(), FALSE)
        trace2 <<- testor:::get_trace(trace.base, sys.calls(), TRUE)
        invokeRestart("abort")
      } 
    ),
    abort=function() NULL
  )
  test_that("Trace Retrieval", {
    expect_equal(
      list("stop(\"Yowza\")", "fun()"),
      trace1
    )
    expect_equal(
      list("stop(\"Yowza\")", "fun()", "eval(expr, envir, enclos)", "eval(fun.call)", "withVisible(eval(fun.call))"),
      trace2
    )
  } )
  test_that("Invisible Expression", {
    exp <- quote(invisible(x <- 1:30))
    expect_equal(1:30, testor:::eval_user_exp(exp, globalenv())$value)
  } )
} )

