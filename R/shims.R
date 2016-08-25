#' @include class_unions.R
#' @include fun.ref.R
#' @include global.R

NULL

# Return List With Return Call Locations
#
# List is in same format as the \code{at} parameter for trace

find_returns <- function(fun) {
  stopifnot(is.function(fun))
  ret.lang <- as.name("return")

  rec_fn <- function(x) {
    if(is.call(x) && is.name(x[[1L]]) && x[[1L]] == ret.lang) {
      list(NULL)
    } else if (is.call(x) && length(x) > 1L) {
      index.res <- list()
      for(i in tail(seq_along(x), -1L)) {
        res <- Recall(x[[i]])
        if(is.list(res))
          index.res <- c(index.res, lapply(res, function(x) c(i, x)))
      }
      index.res
    }
  }
  rec_fn(body(fun))
}
# Given a function and find_returns value, pull out the referenced statements

get_returns <- function(fun, ret.loc) {
  bod <- as.list(body(fun))
  lapply(
    ret.loc,
    function(x) {val <- bod; for(i in x) val <- val[[i]]; val}
  )
}
# Add a tracing expression at end of a function
#
# This works generically for all functions, even when they themselves use
# `on.exit`.  Total hack, but it works.
#
# Note that one trade-off on this one is that we squelch any errors produced by
# the original function, and then re-issue them as part of the trace code.  This
# is so that the error message itself shows the function name.  The drawback
# of this is that the original trace is overwritten so some information is lost
# there which could be a problem.
#
# @param fun must be character(1L), name of a function
# @param tracer an expression to insert in fun
# @param print TRUE or FALSE
# @param where a namespace

trace_at_end <- function(fun, tracer, print, where) {
  trace_editor <- function(name, file, title) {
    body(name) <- bquote(
      {
        .res <- try(withVisible(.(body(name))), silent=TRUE)
        if(inherits(.res, "try-error")) {
          cond <- attr(.res, "condition")
          stop(simpleError(message=conditionMessage(cond), call=sys.call()))
        }
        .doTrace(.(tracer))
        with(.res, if(visible) value else invisible(value))
      }
    )
    name
  }
  
  old.edit <- options(editor=trace_editor)
  on.exit(options(old.edit))
  trace(fun, edit=TRUE, where=where)
  invisible(fun)
}
# Function for testing tracing stuff

trace_test_fun <- function(x=0) {
  on.exit(NULL)
  x <- x + 1
  x <- 2
}

.unitizer.base.funs <- list(
  library=base::library,
  attach=base::attach,
  detach=base::detach
)
.unitizer.base.funs.to.shim <- c("library", "attach", "detach")
.unitizer.tracer <- quote(
  {
    .par.env <- asNamespace("unitizer")$.global$global$par.env
    parent.env(.par.env) <- as.environment(2L)
} )
setClass(
  "unitizerShimDat",
  slots=c(
    tracer="languageOrNULL",
    exit="languageOrNULL",
    at="list"
  ),
  validity=function(object) {
    if(is.null(object@tracer) && is.null(object@exit))
      return("Both `tracer` and `exit` slots may not be NULL at same time")
    if(!is.null(object@tracer) && !length(object@at))
      return("Slot `at` must be specified if slot `tracer` is specified")
    if(!all(vapply(object@at, is.integer, logical(1L))))
      return("All values in `at` slot must be integer")
  }
)
.unitizer.shim.dat <- list(
  # Complex shim for library required b/c we cannot use `on.exit`

  library=new(
    "unitizerShimDat", tracer=.unitizer.tracer,
    at=list(c(7L, 3L, 9L, 3L, 13L, 3L, 4L, 4L, 6L), 8L)
  ),
  attach=new("unitizerShimDat", exit=.unitizer.tracer),
  detach=new("unitizerShimDat", exit=.unitizer.tracer)
)

unitizerGlobal$methods(
  shimFuns=function(funs=.unitizer.base.funs.to.shim) {
    '
    Shimming is solely to ensure that the parent environment tracks position 2
    in the search path
    '
    parent.env(par.env) <<- as.environment(2L)
    err.base <- paste(
      "Unable to shim required functions to run with `par.env=NULL` because",
      "%s. Setting `par.env=.GlobalEnv`."
    )
    stopifnot(
      is.character(funs), all(!is.na(funs)),
      all(vapply(.unitizer.base.funs[funs], is.function, logical(1L))),
      all(vapply(.unitizer.base.funs.ref[funs], is.function, logical(1L)))
    )
    funs.to.shim <- mget(
      funs, ifnotfound=vector("list", length(funs)), mode="function",
      envir=.BaseNamespaceEnv
    )
    err.extra <- ""  # 0 char means no error

    if(!tracingState()) {
      err.extra <- "tracing state is FALSE"
    } else if(!all(vapply(funs.to.shim, is.function, logical(1L)))) {
      err.extra <- "some cannot be found"
    } else if(
      any(vapply(funs.to.shim, inherits, logical(1L), "functionWithTrace"))
    ) {
      err.extra <- "they are already traced"
    } else if(  # Make sure funs are unchanged; note as.character needed b/c of `covr`
      !all(
        fun.identical <- unlist(
          Map(
            function(x, y)
              identical(as.character(body(x)), as.character(body(y))),
            .unitizer.base.funs[funs],
            .unitizer.base.funs.ref[funs]
      ) ) )
    ) {
      err.extra <- paste0(
        "base functions ",paste0("`", funs[!fun.identical], "`", collapse=", "),
        " do not have the definitions they had when this package was ",
        "developed"
      )
    }
    if(nchar(err.extra)) {
      warning(sprintf(err.base, err.extra), immediate.=TRUE)
      parent.env(par.env) <<- .GlobalEnv
      return(FALSE)
    }
    # apply shims

    if(shim.fail <- !all(vapply(funs, .self$shimFun, logical(1L)))) {
      unshimFuns()  # This also resets par.env parent
      return(FALSE)
    }
    return(TRUE)
  },
  shimFun=function(name) {
    stopifnot(is.function(getFun(name)))

    # Suppress std.err because of "Tracing Function..." messages produced by trace

    capt.cons <- new("unitizerCaptCons")
    on.exit({
      txt <- try(get_text_capture(capt.cons, type="message"))
      close_and_clear(capt.cons)
      if(!inherits(txt, "try-error")) word_msg(txt)
      stop("Failed attempting to shim `", name, "`")
    } )
    capt.cons <- set_text_capture(capt.cons, "message")

    # Now shim

    if(!is(.unitizer.shim.dat[[name]], "unitizerShimDat"))
      stop("Logic Error: missing shim data")

    shimmed <- try(
      base::trace(
        name, tracer=.unitizer.shim.dat[[name]]@tracer,
        at=.unitizer.shim.dat[[name]]@at, print=FALSE,
        where=.BaseNamespaceEnv, exit=.unitizer.shim.dat[[name]]@exit
    ) )
    # Process std.err to make sure nothing untoward happened

    shim.out <- get_text_capture(capt.cons, "message")
    on.exit(NULL)
    close_and_clear(capt.cons)

    if(
      !identical(
        sprintf(
          "Tracing function \"%s\" in package \"namespace:base\"\n",
          name
        ),
        shim.out
      ) || inherits(shimmed, "try-error")
    )
      word_msg(shim.out)

    if(inherits(shimmed, "try-error")) {
      warning("Unable to trace `", name, "`", immediate.=TRUE)
      return(FALSE)
    }
    # Store shimmed functions so we can check whether they have been
    # un/reshimmed

    shim.funs[[name]] <<- getFun(name)
    TRUE
  },
  unshimFuns=function() {
    parent.env(par.env) <<- .GlobalEnv
    capt.cons <- new("unitizerCaptCons")
    on.exit({
      txt <- try(get_text_capture(capt.cons, type="message"))
      close_and_clear(capt.cons)
      if(!inherits(txt, "try-error")) word_msg(txt)
      stop(
        "Failed attempting to unshim `", i, "`; you should consider ",
        "manually untracing the function, or restarting your R session to ",
        "restore function to original value."
      )
    } )
    set_text_capture(capt.cons, "message")
    untraced <- character()

    for(i in names(shim.funs)) {
      if(identical(getFun(i), shim.funs[[i]])) {   # if not identical, then someone else shimmed / unshimmed
        base::untrace(i, where=.BaseNamespaceEnv)
        untraced <- c(untraced, i)
      }
      shim.funs[[i]] <<- NULL
    }
    unshim.out <- get_text_capture(capt.cons, "message")
    on.exit(NULL)
    close_and_clear(capt.cons)

    if(
      !identical(
        paste0(
          "Untracing function \"", untraced,
          "\" in package \"namespace:base\"\n", collapse=""
        ),
        unshim.out
      )
    ) {
      word_msg(unshim.out)
    }
    TRUE
  },
  checkShims=function() {
    fail <- FALSE
    if(!tracingState()) {
      warning(
        "Tracing state off, so disabling clean parent env", immediate.=TRUE
      )
      fail <- TRUE
    }
    shim.status <- vapply(
      names(shim.funs),
      function(i) identical(getFun(i), shim.funs[[i]]),
      logical(1L)
    )
    if(!all(shim.status)) {
      warning(
        "Traced functions unexpectedly changed, disabling clean parent env",
        immediate.=TRUE
      )
      fail <- TRUE
    }
    if(fail) {
      unshimFuns()
      FALSE
    } else TRUE
  }
)
#' Utility Function
#'
#' @keywords internal

getFun <- function(name) {
  fun <- try(
    get(name, envir=.BaseNamespaceEnv, inherits=FALSE, mode="function"),
    silent=TRUE
  )
  if(inherits(fun, "try-error")) NULL else fun
}
