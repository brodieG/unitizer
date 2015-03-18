#' @include unitizer.R

NULL

setGeneric("exec", function(x, ...) standardGeneric("exec"))

#' Manages Test Expression Evaluation
#'
#' Runs test, captures value, stdout, stderr, conditions, etc.
#'
#' @keywords internal
#' @param test the call to test
#' @param test.env the environment to evaluate the \code{test} in
#' @return a \code{\link{unitizerItem-class}} object

setMethod("exec", "ANY", valueClass="unitizerItem",
  function(x, test.env) {
    if(!is.environment(test.env)) stop("Argument `test.env` must be an environment.")
    # Prep message and std output capture, note this is reset with every test expression
    # evaluation

    x.comments <- attr(x, "comment")  # need to recover comments from container since we can't attach comments directly to name
    x <- symb_mark_rem(x)             # get rid of comment container

    warn.opt <- getOption("warn")     # Need to ensure warn=1 so that things work properly
    err.opt <- getOption("error")
    std.err.capt <- tempfile()        # Inefficient to do this for every test? Convenient though
    std.out.capt <- tempfile()
    std.err.capt.con <- set_text_capture(std.err.capt, "message")
    std.out.capt.con <- set_text_capture(std.out.capt, "output")
    x.to.eval <- `attributes<-`(x, NULL)

    # Manage unexpected outcomes

    on.exit({
      options(warn=warn.opt)
      options(error=err.opt)
      capt <- get_capture(std.err.capt.con, std.err.capt, std.out.capt.con, std.out.capt)
      file.remove(std.err.capt, std.out.capt)
      message(
        "Unexpectedly exited evaluation attempt when executing test expression:\n> ",
        paste0(deparse(x.to.eval), collapse=""),
        "\nMake sure you are not calling `unitize` inside a `tryCatch`/`try` block, ",
        "invoking a restart defined outside `unitize`, evaluating an expression that ",
        "calls `quit()`/`q()`, or quitting from a `browser()`/`debug()`/`trace()`. ",
        "If none of these apply yet you are seeing this message please contact ",
        "package maintainer."
      )
    } )
    # Evaluate expression

    value <- list(value=NULL, visible=FALSE)
    aborted <- FALSE
    trace <- NULL
    passed.eval <- FALSE
    conditions <- list()
    output <- message <- character()
    options(warn=1L)
    options(error=NULL)

    res <- eval_user_exp(x.to.eval, test.env)

    on.exit(NULL)
    options(warn=warn.opt)
    options(error=err.opt)

    # Revert settings, get captured messages, if any and if user isn't capturing already

    capt <- get_capture(std.err.capt.con, std.err.capt, std.out.capt.con, std.out.capt)
    file.remove(std.err.capt, std.out.capt)
    if(aborted & is.call(x)) {   # check to see if `unitizer_sect` failed
      test.fun <- try(eval(x[[1L]], test.env), silent=TRUE)
      if(identical(test.fun, unitizer_sect)) {
        stop("Failed instantiating a unitizer section:\n", paste0(capt$message, "\n"))
    } }
    new(
      "unitizerItem", call=x.to.eval, value=res$value,
      conditions=new("conditionList", .items=res$conditions),
      output=capt$output, message=capt$message, aborted=res$aborted,
      env=test.env, comment=x.comments, trace=res$trace
    )
} )
#' Utility function to evaluate user expressions
#'
#' @keywords internal
#' @param unitizerUSEREXP an expression to evaluate
#' @param env environment the environment to evaluate the expression in
#' @return TBD
#' @seealso exec, unitizer_prompt

eval_user_exp <- function(unitizerUSEREXP, env ) {
  exp <- call("withVisible", call("eval", unitizerUSEREXP))
  res <- user_exp_handle(exp, env, "", unitizerUSEREXP)
  if(!res$aborted && res$value$visible && length(unitizerUSEREXP)) {
    res2 <- user_exp_display(res$value$value, env, unitizerUSEREXP)
    res$conditions <- append(res$conditions, res2$conditions)
    if(length(res2$trace)) res$trace <- res2$trace
    res$aborted <- res2$aborted
  }
  modifyList(res, list(value=res$value$value), keep.null=TRUE)  # convolution required due to possible NULL value
}
#' Evaluate Print/Show of an Object
#'
#' @rdname eval_user_exp
#' @keywords internal

user_exp_display <- function(value, env, expr) {
  print.env <- new.env(parent=env)
  assign("unitizerTESTRES", value, envir=print.env)
  if(isS4(value)) {
    print.type <- "show"
    disp.expr <- quote(show(unitizerTESTRES))
  } else {
    print.type <- "print"
    disp.expr <- quote(print(unitizerTESTRES))
  }
  user_exp_handle(disp.expr, print.env, print.mode=print.type, expr.raw=expr)
}
#' @rdname eval_user_exp
#' @keywords internal

user_exp_handle <- function(expr, env, print.mode, expr.raw) {
  aborted <- FALSE
  conditions <- list()
  trace <- list()
  print.type <- print.mode
  printed <- nchar(print.mode) > 1
  value <- NULL

  withRestarts(
    withCallingHandlers(
      {
        trace.base <- sys.calls()
        value <- eval(expr, env)
      },
      condition=function(cond) {
        attr(cond, "unitizer.printed") <- printed
        conditions[[length(conditions) + 1L]] <<- cond
        if(inherits(cond, "error")) {
          trace.new <- sys.calls()
          trace <<- get_trace(
            trace.base, trace.new, printed, print.type, expr.raw
          )
      } }
    ),
    abort=function() {
      aborted <<- structure(TRUE, printed=printed)
    }
  )
  list(
    value=value,
    aborted=aborted,
    conditions=conditions,
    trace=trace
  )
}
#' Recompute a Traceback
#'
#' Used for cases where the trace isn't generated because the error was run within
#' a handling loop, but we still want the trace so we can emulate command line
#' behavior.
#'
#' This will modify the .Traceback system variable (see \code{\link{traceback}}
#' documentation).
#'
#' Assumption right now is that the outer most call to \code{withCallingHandlers}
#' is the baseline level from which we want to repor the traceback.
#'
#' @keywords internal
#' @param trace a list of type generated by sys.calls()
#' @return TRUE (only purpose of this is side effect)

set_trace <- function(trace) {
  if(length(trace)) assign(".Traceback", trace, envir=getNamespace("base"))
  TRUE
}
#' Collect the Call Stack And Clean-up
#'
#' Only intended for use within \code{eval_user_exp}, will clean up the result
#' from two different \code{sys.calls} calls to extract the calls that a
#' trace would show on error.
#'
#' How much of the stack is used is affected by the \code{printed}
#' argument because if something didn't pass evaluation, it means the error
#' occurred within \code{withVisible} which in this setup means we need to
#' remove two additional levels.
#'
#' Relies on calls being evaluated in a very particular environment.
#'
#' @seealso set_trace
#' @param trace.base starting point for what we care about in the trace, as
#'   produced by \code{sys.calls}
#' @param trace.new the trace within the condition handler, as produced by
#'   \code{sys.calls}
#' @param passsed.eval whether the evaluatation succeeded in the first step (see
#'   details)
#' @param print.type character(1L) one of "print", "show", or ""
#' @param exp the expression to sub in to the print/show statements if we passed
#'   eval
#' @keywords internal

get_trace <- function(trace.base, trace.new, printed, print.type, exp) {

  # because withCallingHandlers/withRestarts don't register when calling
  # sys.calls() within them, but do when calling sys.calls() from the handling
  # function, we need to remove at least 4 calls from trace.new, and possibly
  # more if we ended up evaluating within withVisible

  len.new <- length(trace.new)

  if(
    len.new > length(trace.base) &&
    all(
      vapply(
        seq_along(trace.base), FUN.VALUE=logical(1L),
        function(x) identical(trace.base[[x]], trace.new[[x]])
    ) )
  ) {
    # Filter out calls through signalCondition rather than stop and
    # `stop+condition`

    is.stop <- identical(trace.new[[len.new]], quote(h(simpleError(msg, call))))
    is.stop.cond <- length(trace.new) > 1 &&
      identical(trace.new[[len.new - 1L]][[1L]], quote(stop))

    if(is.stop || is.stop.cond) {
      trace.new[seq_along(trace.base)] <- NULL
      if(is.function(trace.new[[length(trace.new)]])) {
        is.function(trace.new[[length(trace.new)]])  # er, does this do anything?
      }
      if(length(trace.new) >= 9L || (printed && length(trace.new) >= 6L)) {
        trace.new[1L:(if(printed) 6L else 9L)] <- NULL
        if(printed) {
          # Find any calls from the beginning that are length 2 and start with
          # print/show and then replace the part inside the print/show call with
          # the actual call

          exp.to.rep <- cumsum(
            vapply(
              trace.new, FUN.VALUE=logical(1L),
              function(x) {
                length(x) == 2L &
                grepl(paste0("^", print.type, "(\\..*)?$"), as.character(x[[1L]]))
          } ) ) == 1L:length(trace.new)
          trace.new <- lapply(seq_along(exp.to.rep),
            function(idx) {
              if(exp.to.rep[[idx]]) {
                `[[<-`(
                  trace.new[[idx]], 2L,
                  if(is.expression(exp)) exp[[length(exp)]] else exp
                )
              } else trace.new[[idx]]
        } ) }
        if(length(trace.new) >= 2L) {
          return(
            lapply(FUN=deparse,
              rev(
                head(
                  trace.new,
                  if(is.stop) -2L
                  else if (is.stop.cond) -1L
                  else stop(
                    "Logic Error: must be either stop or stop+cond; contact ",
                    "maintainer."
      ) ) ) ) ) } }
    } else return(list())
  }
  stop("Logic Error: couldn't extract trace; contact maintainer.")
}
