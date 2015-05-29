#' @include class_unions.R
#' @include fun.ref.R
#' @include global.R

NULL

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
  library=new(
    "unitizerShimDat", tracer=.unitizer.tracer,
    at=list(c(7L, 3L, 9L, 3L, 13L, 3L, 3L, 4L, 6L), 8L)
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
    err.base <- paste(
      "Unable to shim required functions to run with `par.env=NULL` because",
      "%s. Setting `par.env=.GlobalEnv`."
    )
    stopifnot(
      is.character(funs), all(!is.na(funs)),
      all(vapply(.unitizer.base.funs[funs], is.function, logical(1L))),
      all(vapply(.unitizer.base.funs.ref[funs], is.function, logical(1L)))
    )
    if(!tracingState()) {
      warning(sprintf(err.base, "tracing state is FALSE") ,immediate.=TRUE)
      disable("par.env")
      return(FALSE)
    }
    funs.to.shim <- mget(
      funs, ifnotfound=vector("list", length(funs)), mode="function",
      envir=.BaseNamespaceEnv
    )
    if(!all(vapply(funs.to.shim, is.function, logical(1L)))) {
      warning(sprintf(err.base, "some cannot be found"), immediate.=TRUE)
      disable("par.env")
    }
    if(any(vapply(funs.to.shim, inherits, logical(1L), "functionWithTrace"))) {
      warning(sprintf(err.base, "they are already traced"), immediate.=TRUE)
      disable("par.env")
      return(FALSE)
    }
    if(  # Make sure funs are unchanged
      !all(
        fun.identical <- unlist(
          Map(
            function(x, y) identical(body(x), body(y)),
            .unitizer.base.funs[funs],
            .unitizer.base.funs.ref[funs]
      ) ) )
    ) {
      warning(
        sprintf(
          err.base,
          paste0(
            "base functions ",
            paste0("`", funs[!fun.identical], "`",
              collapse=", "
            ),
            "do not have the definitions they had when this package was ",
            "developed"
        ) ),
        immediate.=TRUE
      )
      disable("par.env")
      return(FALSE)
    }
    # apply shims

    if(!all(vapply(funs, .self$shimFun, logical(1L)))) {
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

    if(!is(.unitizer.shim.dat[[name]], "unitizerShimDat"))
      stop("Logic Error: missing shim data")

    shimmed <- try(
      base::trace(
        name, tracer=.unitizer.shim.dat[[name]]@tracer,
        at=.unitizer.shim.dat[[name]]@at, print=FALSE,
        where=.BaseNamespaceEnv, exit=.unitizer.shim.dat[[name]]@exit
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
    # Store shimmed functions so we can check whether they have been
    # un/reshimmed

    shim.funs[[name]] <<- getFun(name)
    TRUE
  },
  unshimFuns=function() {
    for(i in names(shim.funs)) {
      if(identical(getFun(i), shim.funs[[i]]))  # if not identical, then someone else shimmed / unshimmed
        base::untrace(i, where=.BaseNamespaceEnv)
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
    get(name, envir=.BaseNamespaceEnv, inherits=FALSE, mode="function"),
    silent=TRUE
  )
  if(inherits(fun, "try-error")) NULL else fun
}
