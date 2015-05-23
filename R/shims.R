#' @include list.R

NULL

# - Shim Structures ------------------------------------------------------------

#' Capture Info for Tracing a Function
#'
#' Currently assumes we're only tracing stuff from base namespace.  We use
#' empty default expressions for \code{tracer} and \code{exit} and put them
#' in no matter what, though they should do nothing (makes rest of code simpler)
#'
#' Expectation allows us to confirm that certain lines in a function are what
#' we expect them to be.
#'
#' @keywords internal

setClass(
  "unitizerShimFun",
  slots=c(
    name="character",
    tracer="language", # languageOrNULL
    exit="language",
    at="integer",
    expectation="expression",
    fun.ref="function"
  ),
  prototype=list(at=1L, tracer=quote({}), exit=quote({}))
  validity=function(object) {
    if(!is.chr1plain(object@name) || is.na(object@name))
      return("slot `name` must be character(1L) and not NA")
    if(inherits(try(fun <- getFun(object@name)), "try-error"))
      return(paste0("`", name, "` is not a function in base"))
    if(!is.integer(at) || length(at) != 1L || at < 1L)
      return("slot `at` must be integer(1L) and strictly positive")
    TRUE
  }
)
setClass(
  "unitizerShimFunList", contains="unitizerList",
  slots=c(
    description="character"
  ),
  validity=function(object) {
    if(!is.chr1plain(object@description) || is.na(object@description))
      return("Slot `description` must be character(1L) and not NA")
    if(!all(vapply(object@.items, is, logical(1L), "unitizerShimFun")))
      return("Slot `.items` may only contain \"unitizerShimFun\" objects")
    if(!isTRUE(object@active) || !identical(object@active, FALSE))
      return("Slot `active` must be TRUE or FALSE")
    TRUE
  }
)
# - Shim Data ------------------------------------------------------------------

# Here we are assuming that each different type of shimming will have its own
# set of functions to shim; if there is overlap we'll have to reconsider this
# structure, though there isn't right now

setClass(
  "unitizerShims",
  slots=c(
    search.path="unitizerShimFunList",
    options="unitizerShimFunList",
    working.directory="unitizerShimFunList"
  ),
  prototype=list(
    search.path=new(
      "unitizerShimFunList",
      description="search path",
      .items=list(
        new(
          "unitizerShimFun", name="library", at=1L,
          tracer=quote(
            {
              untz <- asNamespace("unitizer")
              if (!character.only) {
                package <- as.character(substitute(package))
                character.only <- TRUE
              }
              .pos <- if(length(pos) == 1L && !is.na(pos)) {
                if(is.numeric(pos)) {
                  as.integer(pos)
                } else if(is.character(pos)) {
                  match(pos, search())
                } else NA_integer_
              }
              res <- try(
                untz$.library(
                  package=package, help=help, pos=pos, lib.loc=lib.loc,
                  character.only=character.only, logical.return=logical.return,
                  warn.conflicts=warn.conflicts, quietly=quietly,
                  verbose=verbose
              ) )
              if(inherits(res, "try-error")) {
                cond <- attr(res, "condition")
                stop(simpleError(conditionMessage(cond), sys.call()))
              }
              untz$search_path_track("library", .pos) # Update search path tracking
              untz$.global$bookmark()
              return(res)
        } ) ),
        new(
          "unitizerShimFun", name="attach",
          exit=quote({
            untz <- asNamespace("unitizer")
            untz$search_path_track("attach", pos)
            untz$.global$bookmark()
          })
        ),
        new(
          "unitizerShimFun",
          name="detach",
          tracer=quote(
            {
              .unitizer.detach.obj <- as.environment(pos)
              .unitizer.pos <- pos
              .unitizer.detach.name <- search()[[pos]]
          } ),
          at=4L,
          exit=quote(
            {
              untz <- asNamespace("unitizer")
              res <- untz$search_path_track("detach", .unitizer.pos)
              if(!exists(".unitizer.detach.obj") && is.integer(res) && length(res))
                stop("Logic Error: search path shorter, but no detach object")

              if(
                is.null(
                  untz$.global$tracking@search.path@
                    detached.objects[[names(res)]]
                )
              ) {
                untz$.global$tracking@search.path@
                  detached.objects[[names(res)]] <- list()
              }
              untz$.global$tracking@search.path@detached.objects[[
                names(res)]][[res]] <- new(
                  "unitizerSearchData",
                  name=.unitizer.detach.name,
                  type=if(is.loaded_package(.unitizer.detach.name))
                    "package" else "object",
                  data=.unitizer.detach.obj,
                  extra=dirname(attr(.unitizer.detach.obj, "path"))
              )
              untz$.global$bookmark()
          } ),
          expectation=`[[<-`(
            expression(), 3L, quote(packageName <- search()[[pos]])
          )
    ) ) ),
    options=new(
      "unitizerShimFunList", description="options",
      .items=list(
        new(
          "unitizerShimFun", name="options",
          exit=quote(
            {
              untz <- asNamespace("unitizer")
              untz$.global$tracking@options <- append(
                untz$.global$tracking@options,
                untz$.unitizer.base.funs$options()
              )
              untz$.global$bookmark()
            }
    ) ) ) ),
    working.directory=new(
      "unitizerShimFunList", description="working directory",
      .items=list(
        new(
          "unitizerShimFun", name="setwd",
          exit=quote(
            untz <- asNamespace("unitizer")
            untz$.global$tracking@working.directory <- append(
              untz$.global$tracking@working.directory,
              untz$.unitizer.base.funs$getwd()
            )
            untz$.global$bookmark()
  ) ) ) ) ),
  validity=function(object) {
    if(!all(slotNames(object)) %in% .unitizer.global.settings.names)
      return("Invalid slots")
    TRUE
  }
)
# - Shim Application -----------------------------------------------------------

#' Do the function shimming
#'
#' Could be done as method, but seems excessive to define generic just for this
#'
#' Here we shim by \code{`trace`}ing the \code{`libary/require/attach/detach`}
#' functions and recording each run of those functions that modifies the
#' search path with enough information to restore the search path later.
#'
#' See 'global.R' as much of the setup / management is done from there
#'
#' @param only.matching only unshim funs that we can match the shimmed version
#'   of
#' @keywords internal

shim_funs <- function(funs) {
  stopifnot(is(funs, "unitizerShimFunList"))
  shim.res <- lapply(as.list(funs), shim_fun)
  if(!all(vapply(as.list(shim.res), is, logical(1L), "unitizerShimFun"))) {
    warning(     # need to provide better guidance on what docs to look at
      "Unable to shim required functions for ", funs@description,
      " manipulation, so running in vanilla ", funs@description,
      " mode (see docs)"
    )
    unshim_funs(funs)
    return(FALSE)
  }
  # Store shimmed functions for reference

  funs[] <- shim.res
  return(funs)
}
#' @rdname shim_fun
#' @keywords internal

unshim_funs <- function(funs, only.matching=TRUE) {
  stopifnot(is(funs, "unitizerShimFunList"))
  unshim <- lapply(
    as.list(funs),
    function(x) {
      if(
        only.matching &&
      ) {

      }
      try(untrace(x@name, where=.BaseNamespaceEnv))
    }
  )
  if(any(vapply(unshim, inherits, logical(1L), "try-error"))) {
    stop(
      "Logic Error: failed attempting to revert to vanilla mode; ",
      "please consider restarting your R session to reset global parameters ",
      "and contact maintainer with details of this error."
    )
  invisible(TRUE)
}
#' @rdname shim_fun
#' @keywords internal

shim_fun <- function(funDat) {
  stopifnot(is(funDat, "unitizerShimFun"))

  # Check function can be shimmed

  fun <- try(
    get(funDat@name, .BaseNamespaceEnv, inherits=FALSE, mode="function")
  )
  if(inherits(fun, "try-error")) {
    warning(
      "Unable to retrieve `", funDat@name, "` from base namespace",
      immediate.=TRUE
    )
    return(FALSE)
  }
  if(is(fun, "functionWithTrace")) {
    warning(
      "Unable to trace `", funDat@name, "` as it is already traced",
      immediate.=TRUE
    )
    return(FALSE)
  }
  if(length(funDat@expectation)) {
    for(i in seq_along(funDat@expectation)) {
      if(is.null(funDat@expectation)) next
      if(!identical(funDat@expectation[[i]], body(fun)[[i]])) {
        warning(
          "Unable to trace `", funDat@name, "` because it appears to have ",
          "changed since the time this package was written (mismatch at line ",
          i, ")",
          immediate.=TRUE
        )
        return(FALSE)
  } } }
  # Create tracing call

  trace.lst <- list("trace", where=.BaseNamespaceEnv, print=FALSE)
  if(!is.null(exit)) trace.lst[["exit"]] <- exit
  if(!is.null(tracer)) {
    trace.lst[["tracer"]] <- tracer
    trace.lst[["at"]] <- at
  }
  # Suppress std.err because of "Tracing Function..." messages produced by trace

  std.err <- tempfile()
  std.err.con <- file(std.err, "w+b")
  on.exit({
    try(get_text_capture(std.err.con, std.err, type="message"))
    release_sinks()
    close(std.err.con)
    unlink(std.err)
    stop("Failed attempting to shim ", name)
  } )
  capt.con <- set_text_capture(std.err.con, "message")

  # Now shim

  shimmed <- try(eval(as.call(trace.lst)))

  # Process std.err to make sure nothing untoward happened

  shim.out <- get_text_capture(capt.con, std.err, "message")
  on.exit(NULL)
  close(std.err.con)
  unlink(std.err)

  if(
    !identical(
      sprintf("Tracing function \"%s\" in package \"namespace:base\"", name)),
      shim.out
    ) || inherits(shimmed, "try-error")
  )
    word_msg(shim.out)

  if(inherits(shimmed, "try-error")) {
    warning("Unable to trace `", name, "`", immediate.=TRUE)
    return(FALSE)
  }
  # Store shimmed functions so we can check whether they have been un/reshimmed

  fun.shimmed <- try(
    get(name, .BaseNamespaceEnv, inherits=FALSE, mode="function")
  )
  if(inherits(fun.shimmed), "try-error") {
    warning(
      "Unable to retrieve traced version of `", name, "`", immediate.=TRUE
    )
    return(FALSE)
  }
  funDat@fun.ref <- fun.shimmed
  return(funDat)
}

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
