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
#' Create a Store ID from a Test File Name
#'
#' @param x character(1L) file name ending in .r or .R
#' @return store id name, or NULL if \code{x} doesn't meet expectations
#' @export
#' @examples
#' filename_to_storeid(file.path("tests", "unitizer", "foo.R"))
#' filename_to_storeid(file.path("tests", "unitizer", "boo.r"))
#' filename_to_storeid(file.path("tests", "unitizer", "boo"))  # does not end in [rR]

filename_to_storeid <- function(x) {
  if(is.character(x) && length(x) == 1L){
    if((y <- sub("\\.[rR]$", ".unitizer", x)) != x) return(y)
    warning(
      "Unable to translate file name '", x, "' to store id; please provide ",
      "explicit store id"
    )
  } else
    warning(
      "Unable to generate store id from non `character(1L)` file \"name\""
    )
  NULL
}
#' History Management Funs
#'
#' @keywords internal

history_capt <- function(hist.file=NULL) {
  # set up local history

  if(is.null(hist.file)) return(list(con=NULL, file=NULL))

  hist.try <- try(savehistory(), silent=TRUE)
  if(inherits(hist.try, "try-error"))
    warning(conditionMessage(attr(hist.try, "condition")))
  hist.con <- try(file(hist.file, "at"))
  if(inherits(hist.con, "try-error"))
    stop("Unable to open a connection to file provided for history")
  cat(
    "## <unitizer> (original history will be restored on exit)\n",
    file=hist.con
  )
  hist.try <- try(
    loadhistory(showConnections()[as.character(hist.con), "description"]),
    silent=TRUE
  )
  if(inherits(hist.try, "try-error"))
    warning(conditionMessage(attr(hist.try, "condition")))
  list(con=hist.con, file=hist.file)
}
history_release <- function(hist.obj) {
  if(all(vapply(hist.obj, is.null, logical(1L))))
    return(invisible(TRUE))
  close(hist.obj$con)
  file.remove(hist.obj$file)
  hist.try <- try(loadhistory(), silent=TRUE)
  if(inherits(hist.try, "try-error"))
    warning(conditionMessage(attr(hist.try, "condition")))
}

#' Simplify a Path As Much as Possible to Working Directory
#'
#' @param wd NULL or character(1L) resolving to a directory, if NULL will be
#'   resolved to either \code{getwd} or what \code{getwd} was at the beginning
#'   of a \code{unitizer} run
#' @param only.if.shorter logical(1L) whether to relativize only if the
#'   resulting \code{path} is shorter than the input
#' @keywords internal

relativize_path <- function(path, wd=NULL, only.if.shorter=TRUE) {
  if(!is.character(path) || any(is.na(path)))
    stop("Argument `path` must be character and may not contain NAs")
  if(!isTRUE(only.if.shorter) && !identical(only.if.shorter))
    stop("Argument `only.if.shorter` must be TRUE or FALSE")
  if(
    !is.null(wd) && !is.character(wd) && !identical(length(wd), 1L) &&
    !file_test("-d", wd)
  )
    stop("Argument `wd` must be NULL or a reference of to a directory")
  if(is.null(wd)) {
    wd <- if(
      inherits(try(pack.env, silent=TRUE), "try-error") ||
      !is.character(pack.env$wd) || !identical(length(pack.env$wd), 1L) ||
      !file.exists(pack.env$wd)
    ) getwd() else pack.env$wd
  }
  wd <- try(normalizePath(wd, mustWork=TRUE), silent=TRUE)
  res <- if(
    !inherits(wd, "try-error") && is.character(.Platform$file.sep) &&
    identical(length(.Platform$file.sep), 1L)
  ) {
    norm <- normalizePath(path, mustWork=FALSE)
    to.norm <- file.exists(norm)

    path.pieces <- lapply(
      strsplit(norm[to.norm], .Platform$file.sep, fixed=TRUE), Filter, f=nchar
    )
    wd.pieces <- Filter(
      nchar, unlist(strsplit(wd, .Platform$file.sep, fixed=TRUE))
    )
    # /a/b/c/d/e
    # /a/b/c/F/G
    reled <- vapply(
      path.pieces,
      function(x) {
        up.to <- min(length(x), length(wd.pieces))
        if(!up.to) return(x)
        first.diff <-
          min(up.to + 1L, which(x[1:up.to] != wd.pieces[1:up.to])) - 1L
        path <- if(identical(first.diff, 0L)) {
          x
        } else {
          end <- min(up.to, first.diff)
          c(rep("..", length(wd.pieces) - end), x[-(1:end)])
        }
        do.call(file.path, as.list(path))
      },
      character(1L)
    )
    norm[to.norm] <- reled
    norm
  } else path
  if(only.if.shorter) {
    ifelse(nchar(res) < nchar(path), res, path)
  } else res
}
