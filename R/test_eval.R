#' @include testor.R

NULL

setGeneric("exec", function(x, ...) standardGeneric("exec"))

#' Manages Test Expression Evaluation
#' 
#' Runs test, captures value, stdout, stderr, conditions, etc.
#' 
#' @keywords internal
#' @param test the call to test
#' @param test.env the environment to evaluate the \code{`test`} in 
#' @return a \code{`\link{testorItem-class}`} object

setMethod("exec", "ANY", valueClass="testorItem",
  function(x, test.env) {
    if(!is.environment(test.env)) stop("Argument `test.env` must be an environment.")
    # Prep message and std output capture, note this is reset with every test expression
    # evaluation

    warn.opt <- getOption("warn")     # Need to ensure warn=1 so that things work properly
    err.opt <- getOption("error")
    std.err.capt <- tempfile()        # Inefficient to do this for every test? Convenient though
    std.out.capt <- tempfile()
    std.err.capt.con <- set_text_capture(std.err.capt, "message")
    std.out.capt.con <- set_text_capture(std.out.capt, "output")

    # Manage unexpected outcomes

    on.exit({  
      options(warn=warn.opt)
      options(error=err.opt)
      capt <- get_capture(std.err.capt.con, std.err.capt, std.out.capt.con, std.out.capt)
      file.remove(std.err.capt, std.out.capt)
      stop(
        "Unexpectedly exited evaluation attempt when executing test expression\n> ", 
        paste0(deparse(x), collapse=""), 
        "\nmake sure you are not calling `runtests` inside a `tryCatch`/`try` block or ",
        "invoking a restart defined outside `runtests`; if you are not, contact ",
        "package maintainer."
      )
    } )
    # Evaluate expression

    value <- list(value=NULL, visible=FALSE)
    aborted <- FALSE
    conditions <- list()
    output <- message <- character()
    options(warn=1L)
    options(error=NULL)

    withCallingHandlers(
      withRestarts(
        value <- withVisible(eval(x, test.env)),
        abort=function() aborted <<- TRUE
      ),
      condition=function(cond) {conditions[[length(conditions) + 1L]] <<- cond}
    )
    if(value$visible) {
      if(isS4(value$value)) show(value$value) else print(value$value)
    }
    on.exit(NULL)
    options(warn=warn.opt)
    options(error=err.opt)

    # Revert settings, get captured messages, if any and if user isn't capturing already

    capt <- get_capture(std.err.capt.con, std.err.capt, std.out.capt.con, std.out.capt)
    if(aborted & is.call(x)) {   # check to see if `testor_sect` failed
      test.fun <- try(eval(x[[1L]], test.env), silent=TRUE)
      if(identical(test.fun, testor_sect)) {
        stop("Failed instantiating a testor section:\n", paste0(capt$message, "\n"))
      }
    }
    new(
      "testorItem", call=x, value=value$value, conditions=conditions, output=capt$output, 
      message=capt$message, aborted=aborted, env=test.env
    )
} )