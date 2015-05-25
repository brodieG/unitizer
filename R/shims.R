#' @include global.R

NULL

unitizerGlobal$methods(
  shimFuns=function(funs=.unitizer.base.funs.to.shim) {
    stopifnot(
      is.character(funs), all(!is.na(funs)),
      all(vapply(.unitizer.base.funs[funs], is.function, logical(1L)))
    )
    funs.to.shim <- .unitizer.base.funs[funs]
    if(any(vapply(funs.to.shim, inherits, logical(1L), "functionWithTrace"))) {
      warning(
        "Unable to shim required functions to run with `par.env=NULL` because ",
        "they are already traced. Setting `par.env=.GlobalEnv`"
      )
      return(FALSE)
    }
    # apply shims

    if(!all(vapply(funs, shimFun, logical(1L)))) {
      unshimFuns()
      return(FALSE)
    }
    return(TRUE)
  },
  shimFun=function(name) {
    stopifnot(is.function(getFun(name)))

    # Suppress std.err because of "Tracing Function..." messages produced by trace

    std.err <- tempfile()
    std.err.con <- file(std.err, "w+b")
    on.exit({
      try(get_text_capture(std.err.con, std.err, type="message"))
      release_sinks()
      close(std.err.con)
      unlink(std.err)
      stop("Failed attempting to shim `", name, "`")
    } )
    capt.con <- set_text_capture(std.err.con, "message")

    # Now shim

    shimmed <- try(
      trace(
        name,
        tracer=quote(
          {
            calls <- sys.calls()
            calls.len <- length(calls)
            global <- asNamespace("unitizer")$.global$global
            if(
              calls.len >= 5L &&
              identical(calls[[calls.len - 3L]][[1L]], quote(.doTrace))
            ) {
              call.orig <- calls[[calls.len - 4L]]
              call.orig[[1L]] <- .unitizer.base.funs[[name]]
              # re-eval original fun where it would have been evaluated, and then
              # return without allowing the function itself to start

              res <- eval(call, parent.frame(5L))
              parent.env(global$par.env) <- as.environment(2L)
              return(res)
            } else {
              warning(
                "Error while using shimmed version of `", name, "`, re-setting ",
                "`par.env` to `.GlobalEnv` which disables clean workspace mode",
                immediate.=TRUE
              )
              unshim <- try(global$disable("par.env"))
              if(inherits(unshim, "try-error"))
                stop(
                  "Logic Error: failed attempting to unshim `", name, "`; contact ",
                  "maintainer.  In the meantime, we recommend you restart your ",
                  "session to restore a clean global environment, and manually ",
                  "set the `par.env` argument to `.GlobalEnv` or some such to ",
                  "remove the need to shim search path functions."
                )
        } } ),
        at=1L,
        print=FALSE,
        where=.BaseNamespaceEnv
    ) )
    # Process std.err to make sure nothing untoward happened

    shim.out <- get_text_capture(capt.con, std.err, "message")
    on.exit(NULL)
    close(std.err.con)
    unlink(std.err)

    if(
      !identical(
        sprintf("Tracing function \"%s\" in package \"namespace:base\"", name),
        shim.out
      ) || inherits(shimmed, "try-error")
    )
      word_msg(shim.out)

    if(inherits(shimmed, "try-error")) {
      warning("Unable to trace `", name, "`", immediate.=TRUE)
      return(FALSE)
    }
    # Store shimmed functions so we can check whether they have been un/reshimmed

    shim.funs[[name]] <<- getFun(name)
    TRUE
  },
  unshimFuns=function() {
    for(i in names(shim.funs)) {
      if(identical(getFun(i), shim.funs[[i]]))  # if not identical, then someone else shimmed / unshimmed
        untrace(i, where=.BaseNamespaceEnv)
      shim.funs[[i]] <<- NULL
    }
  },
  checkShims=function() {
    if(!status@par.env) return(TRUE)  # currently shims only matter for par.env
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
    if(fail) disable("par.env")
    status@par.env
  }
)
#' Utility Function
#'
#' @keywords internal

getFun <- function(name) {
  fun <- try(
    get(name, where=.BaseNamespaceEnv, inherits=FALSE, mode="function"),
    silent=TRUE
  )
  if(inherits(fun, "try-error")) NULL else fun
}
