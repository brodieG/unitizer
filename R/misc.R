#' Retrieves Environment Ancestry
#'
#' @keywords internal
#' @param env the environment to start with
#' @param stop.env the environment to stop with

env_ancestry <- function(env, stop.env=globalenv()) {
  if(!is.environment(env) || !is.environment(stop.env)) stop("Arguments `env` and `stop.env` must both be environments")
  out <- character()
  repeat {
    out <- c(out, env_name(env))
    if(identical(env, stop.env)) break
    if(identical(env, emptyenv())) stop("Hit empty environment while traveling up environment ancestry")
    env <- parent.env(env)
  }
  out
}
#' Gets Environment Name / Memory Code
#'
#' Captures the name that \code{`\link{print.default}`} displays when one
#' prints and environment
#'
#' @keywords internal
#' @param env an environemnt
#' @return character 1 length

env_name <- function(env) {
  if(!is.environment(env)) stop("Argument `env` must be an environment")
  sub("<environment: (.*)>", "\\1", capture.output(print.default(env))[[1]])
}

#' Functions To Ignore
#'
#' Ignored functions are not considered tests if they are called from
#' the top level.
#'
#' Also, provide a function to compare functions even when traced.
#'
#' @keywords internal
#' @param x the reference function, if is traced then y must be identical
#' @param y the current function, if \code{`x`} is not traced and \code{`y`}
#'   is traced, will compare using \code{`y@@original`} instead of \code{`y`}

funs.ignore <- list(base::`<-`, base::library, base::`=`, base::set.seed)
identical_fun <- function(x, y) {
  if(!is.function(x) || !is.function(y))
    stop("Arguments `x` and `y` must both be functions.")
  if(is(x, "functionWithTrace")) {
    return(identical(x, y))
  } else if(is(y, "functionWithTrace")) {
    return(identical(x, y@original))
  }
  identical(x, y)
}
#' Overrides Default quit() Behavior
#'
#' Necessary because quit short circuits the `on.exit` clean-up functions and
#' would leave stuff in a weird state (history not reset, etc.).
#'
#' This is used in \code{`\link{unitize}`}.
#'
#' @aliases unitizer_quit_handler
#' @keywords internal

unitizer_quit <- function(save = "default", status = 0, runLast = TRUE) {
  invokeRestart("unitizerQuitExit", list(save=save, status=status, runLast=runLast))
}
#' @keywords internal

unitizer_quit_handler <- function(quitArgs) {
  message(
    "Encountered `quit()`/`q()`; unitizer not updated.  For more graceful ",
    "quitting type `Q` (without quotes) at the unitizer prompt, or avoid using ",
    "test code that involves calls to `quit()`/`q()`."
  )
  do.call("quit", quitArgs)
}

#' Cleans a Path to be In Standard Format
#'
#' Uses \code{`\link{dirname}`} to convert paths on windows machines with back
#' slasshes to forward slash based names, and then removed excess forward
#' slashes.
#'
#' @keywords internal
#' @param path character the path name to clean up
#' @return the cleaned up path

path_clean <- function(path) {
  if(!is.character(path)) stop("Argument `path` must be character")
  path.norm <- paste0(dirname(path), "/", basename(path))
  sub("/+", "/", path.norm)
}

#' Special Deparse
#'
#' Required to deal with language objects that contain non-language objects
#' that have attributes.
#'
#' Not completely fool proof since you can probably created an object that nests
#' call and non-call stuff repeatedly that would confuse this thing.
#'
#' @keywords internal

deparse_mixed <- function(expr, width.cutoff = 500L, control = "all", ...) {

  rec_lang <- function(expr) {
    if(!is.language(expr)) stop("Logic Error: expecting language object")
    if(length(expr) > 1L) {
      for(i in seq_along(expr)) {
        if(!is.language(expr[[i]])) {
          expr[[i]] <-
            parse(
              text=deparse(expr[[i]], width.cutoff, control, ...),
              keep.source=FALSE
            )[[1L]]
        } else expr[[i]] <- Recall(expr[[i]])
    } }
    expr
  }
  rec_norm <- function(expr) {
    if(is.recursive(expr) && !is.environment(expr)) {
      for(i in seq_along(expr)) {
        if(is.language(expr[[i]])) {
          expr[[i]] <- rec_lang(expr[[i]])
        } else {
          expr[[i]] <- Recall(expr[[i]])
    } } }
    expr
  }
  deparse(rec_norm(expr), width.cutoff=width.cutoff, control=control, ...)
}
