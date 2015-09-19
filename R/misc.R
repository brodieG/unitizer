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
#' DEPRECATED.  Now handled by visibility status.
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
# nocov start
# can't test this without quitting R!
#' @keywords internal

unitizer_quit_handler <- function(quitArgs) {
  word_msg(
    paste0(
      "Encountered `quit()`/`q()`; unitizer not updated.  For more graceful ",
      "quitting type `Q` (without quotes) at the unitizer prompt, or avoid using ",
      "test code that involves calls to `quit()`/`q()`."
    )
  )
  do.call("quit", quitArgs)
}
# nocov end

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
  if(inherits(hist.try, "try-error")) {
    warning(conditionMessage(attr(hist.try, "condition")))
    attr(hist.con, "no.hist") <- TRUE
  }
  list(con=hist.con, file=hist.file)
}
history_release <- function(hist.obj) {
  if(all(vapply(hist.obj, is.null, logical(1L))))
    return(invisible(TRUE))
  no.hist <- attr(hist.obj$con, "no.hist")
  close(hist.obj$con)
  if(isTRUE(attr(hist.obj$file, "hist.tmp"))) file.remove(hist.obj$file)
  if(!isTRUE(no.hist)) {
    # nocov start
    # covr runs non-interactively; can't have history
    hist.try <- try(loadhistory(), silent=TRUE)
    if(inherits(hist.try, "try-error"))
      warning(conditionMessage(attr(hist.try, "condition")))
    # nocov end
  }
}
history_write <- function(hist.con, data) {
  stopifnot(is.open_con(hist.con), is.character(data))
  if(is.open_con(hist.con)) {
    cat(data, file=hist.con, sep="\n")
    if(!isTRUE(attr(hist.con, "no.hist"))) {
      # nocov start
      # covr runs non-interactively; can't have history
      hist.save <-
        try(
          loadhistory(showConnections()[as.character(hist.con), "description"]),
          silent=TRUE
        )
      if(inherits(hist.save, "try-error"))
        warning(attr(hist.save, "condition"), immediate.=TRUE)
      # nocov end
  } }
}
#' Simplify a Path As Much as Possible to Working Directory
#'
#' \itemize{
#'   \item \code{relativize_path} returns a path that can actually be used
#'     to access an actual file from the current working directory
#'   \item \code{pretty_path} (not really used currently) returns the most
#'     readable path that we can produce, but may not usable to access an actual
#'     file, main difference with \code{relativize_path} is that it will
#'     figure out if a file is in a package and return a path relative to the
#'     package directory if it turns out that one is shorter than the one
#'     produced with relativize path
#'   \item \code{unique_path} is used to separate out a common path from a list
#'     of files
#' }
#'
#' @param wd NULL or character(1L) resolving to a directory, if NULL will be
#'   resolved to \code{getwd}; used primarily for testing
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
  if(is.null(wd)) wd <- getwd()
  wd <- try(normalizePath(wd, mustWork=TRUE), silent=TRUE)
  res <- if(
    !inherits(wd, "try-error") && is.character(.Platform$file.sep) &&
    identical(length(.Platform$file.sep), 1L)
  ) {
    norm <- normalizePath(path, mustWork=FALSE)
    to.norm <- TRUE  # used to be only for existing files, but can't recall why

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
        if(length(path)) do.call(file.path, as.list(path)) else ""
      },
      character(1L)
    )
    norm[to.norm] <- reled
    norm
  } else path
  if(!nchar(res)) res <- "."
  if(only.if.shorter) {
    ifelse(nchar(res) < nchar(path), res, path)
  } else res
}
#' @rdname relativize_path

pretty_path <- function(path, wd=NULL, only.if.shorter=TRUE) {
  rel.path <- relativize_path(path, wd, only.if.shorter)
  pkg.dir <- get_package_dir(path)
  if(!length(pkg.dir) || !identical(substr(path, 1L, nchar(pkg.dir)), pkg.dir))
    return(rel.path)

  pkg.name <- try(get_package_name(pkg.dir))
  if(inherits(pkg.name, "try-error"))
    stop("Logic Error: failed getting package name; contact maintainer")
  pkg.path <- file.path(
    paste0("package:", pkg.name), substr(path, nchar(pkg.dir) + 2L, nchar(path))
  )
  if(nchar(rel.path) <= nchar(pkg.path)) rel.path else pkg.path
}
#' @rdname relativize_path

unique_path <- function(files) {
  dirs <- dirname(files)
  uniq.dir <- str_reduce_unique(dirs)
  com.dir <- substr(dirs[[1L]], 1L, nchar(dirs[[1L]]) - nchar(uniq.dir[[1L]]))
  full.dir <- dirs[[1L]]

  repeat {
    dir.tmp <- dirname(full.dir)
    if(
      nchar(dir.tmp) < nchar(com.dir) || !nchar(dir.tmp)
      || identical(dir.tmp, ".")
    ) break
    full.dir <- dir.tmp
  }
  test.files.trim <- if(sum(nchar(uniq.dir))) {
    file.path(uniq.dir, basename(files))
  } else basename(files)
  structure(test.files.trim, common_dir=full.dir)
}

#' Merge Two Lists
#'
#' Values in \code{y} ovewrite existing values in \code{x}.  This is similar to
#' \code{modifyList} but is non-recursive
#'
#' @keywords internal
#' @param x a list
#' @param y a list

merge_lists <- function(x, y, keep.null=TRUE) {
  stopifnot(
    is.list(x), is.list(y),
    !identical(length(names(x)), x), !identical(length(names(y)), y)
  )
  if(!isTRUE(keep.null)) stop("Currently `keep.null` must be TRUE")
  x[names(y)] <- y
  x
}




