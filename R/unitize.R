#' Unitize an R Test Script
#'
#' Turns a standard R script into unit tests by evaluating the expressions and
#' storing them along with their resuls.  Re-running \code{`unitize`} then
#' checks that the values remain unchanged.
#'
#' You can run \code{`unitize`} from the command line, or you can place one or
#' more \code{`unitize`} calls in an R file and source that.
#'
#' \code{`unitizer`} stores are identified by \code{`unitizer`} ids, which by
#' default are character strings containing the location of the folder the
#' \code{`unitizer`} RDS files are kept in.  By default \code{`unitize`} and
#' friends will create a \code{`unitizer`} id for you based on the test file
#' name, but you can specify your own location as an id, or even use a
#' completely different mechanism to store the \code{`unitizer`} data by
#' implementing S3 methods for \code{`\link{get_unitizer}`} and
#' \code{`\link{set_unitizer}`}.
#'
#' See "unitizer" vignette and demos for details and examples.
#'
#' @export
#' @aliases review, unitize_dir, unitize_files
#' @seealso \code{`\link{get_unitizer}`}
#' @param test.file path to the file containing tests
#' @param store.id a folder to store the \code{`unitizer`} objects in; will auto-
#'   generate to a folder at the same location as the test file with the same
#'   name as the testfile, except ending in \code{`.unitizer`} instead of
#'   \code{`.R`}.  This is the default option, you can create custom
#'   \code{`unitizer`} stores as well (see vignette and \code{`\link{get_unitizer}`}).
#' @param x for \code{`review`} only, either a \code{`unitizer`} or something that,
#'   when passed to \code{`\link{get_unitizer}`}, will retrieve a unitizer (i.e.
#'   equivalent to what would get passed in \code{`store.id`}).
#' @param interactive.mode logical(1L) whether to run in interactive mode
#' @param env.clean TRUE or environment, if TRUE tests are run in a clean
#'   environment, if an environment they are run with that environment as the
#'   parent environment.
#' @param search.path.clean logical(1L) if TRUE all items on the search path that
#'   are not part of a clean R session are detached prior to running tests.  Note
#'   namespaces for detached packages remain loaded.  Additionally, the search
#'   path is restored to its initial state upon exiting \code{`unitizer`} so any
#'   packages added/removed, or objects attached/detached from search path are
#'   restored to original state.  This feature is somewhat experimental and is
#'   disabled by default, though this will likely change in the future.  See
#'   "Reproducible Tests" vignette for details.
#' @param search.path.keep character any additional items on the search path
#'   to keep attached; has no effect unless \code{`search.path.clean`} is TRUE
#' @param force.update logical(1L) if TRUE will give the option to re-store a
#'   unitizer after re-evaluating all the tests even if all tests passed.
#' @return the \code{`unitizer`} object, invisibly.  If running in interactive
#'   mode, then the returned \code{`unitizer`} will be modified as per user
#'   input in the interactive session.

unitize <- function(
  test.file, store.id=sub("\\.[Rr]$", ".unitizer", test.file),
  interactive.mode=interactive(), env.clean=TRUE,
  search.path.clean=getOption("unitizer.search.path.clean"),
  search.path.keep=c("tools:rstudio", "package:unitizer"),
  force.update=FALSE
) {
  if(!is.character(test.file) || length(test.file) != 1L || !file_test("-f", test.file))
    stop("Argument `test.file` must be a valid path to a file")
  if(!is.logical(interactive.mode) || length(interactive.mode) != 1L || is.na(interactive.mode))
    stop("Argument `interactive.mode` must be TRUE or FALSE")
  if(!is.logical(force.update) || length(force.update) != 1L || is.na(force.update))
    stop("Argument `force.update` must be TRUE or FALSE")
  print(H1(paste0("unitizer for: ", test.file, collapse="")))
  invisible(
    unitizer_core(
      test.file, store.id, interactive.mode, env.clean, search.path.clean,
      search.path.keep, force.update=force.update
  ) )
}
#' @export

review <- function(
  x, env.clean=TRUE, search.path.clean=getOption("unitizer.search.path.clean"),
  search.path.keep=c("tools:rstudio", "package:unitizer")
) {
  u.name <- if(is.character(x) && length(x) == 1L) {
    x
  } else {
    u.name <- if(is(x, "unitizer")) x@id else x
    u.name <- try(as.character(u.name), silent=TRUE)
    if(inherits(u.name, "try-error")) u.name <- "<unknown>"

  }
  print(H1(paste0("unitizer for: ", u.name, collapse="")))
  invisible(
    unitizer_core(
      test.file=NULL, store.id=x, interactive.mode=TRUE, env.clean=env.clean,
      search.path.clean=search.path.clean, search.path.keep=search.path.keep,
      force.update=FALSE
    )
  )
}

