#' Unitize an R Test Script
#'
#' Turn standard R scripts into unit tests by evaluating the expressions and
#' storing them along with their resuls.
#'
#' \code{unitize} creates unit tests from a single R file, and
#' \code{unitize_dir} creates tests from all the R files in the specified
#' directory (analogous to \code{testthat::test_dir}).
#'
#' \code{review} allows you to review existing \code{unitizer}s and modify them
#' by dropping tests from them.  This is useful if you ever have second thoughts
#' about previously accepted tests and wish to inspect them.
#'
#' \code{unitizer} stores are identified by \code{unitizer} ids, which by
#' default are character strings containing the location of the folder the
#' \code{unitizer} RDS files are kept in.  \code{unitize} and
#' friends will create a \code{unitizer} id for you based on the test file
#' name and location, but you can specify your own location as an id, or even
#' use a completely different mechanism to store the \code{unitizer} data by
#' implementing S3 methods for \code{\link{get_unitizer}} and
#' \code{\link{set_unitizer}}.
#'
#' See \code{unitizer} vignettes and demo for details and examples.
#'
#' @export
#' @aliases unitize review unitize_dir
#' @seealso \code{\link{get_unitizer}}
#' @param test.file path to the file containing tests, if supplied path does not
#'   match an actual system path, \code{unitizer} will try to infer a possible
#'   path (see \code{\link{infer_unitizer_location}})
#' @param store.id a folder to store the \code{unitizer} objects in; if NULL
#'   will select a folder at the same location as the test file with the same
#'   name as the testfile, except ending in \code{.unitizer} instead of
#'   \code{.R}.  If this folder does not exist, it will be created.  This is the
#'   default option, you can create custom \code{unitizer} stores as well
#'   (see vignette and \code{\link{get_unitizer}}).
#' @param x for \code{review} only, either a \code{unitizer} or something that,
#'   when passed to \code{\link{get_unitizer}}, will retrieve a unitizer (i.e.
#'   equivalent to what would get passed in \code{store.id}).
#' @param interactive.mode logical(1L) whether to run in interactive mode (
#'   request user input when needed) or not (error if user input is required,
#'   e.g. if all tests do not pass).
#' @param par.env NULL or environment, if NULL tests are run in a clean
#'   environment, if an environment they are run with that environment as the
#'   parent environment.
#' @param search.path.clean logical(1L) if TRUE all items on the search path
#'   that are not part of a clean R session are detached prior to running tests.
#'   Namespaces for detached packages remain loaded, so re-attaching those
#'   packages during tests with \code{library} should carry little overhead.
#'   The search path is restored to its initial state upon exiting
#'   \code{unitizer} so any packages added/removed, or objects attached/detached
#'   from search path are restored to original state.  See "Reproducible Tests"
#'   vignette for details.
#' @param search.path.keep character any additional items on the search path
#'   to keep attached; has no effect if \code{search.path.clean} is FALSE
#' @param force.update logical(1L) if TRUE will give the option to re-store a
#'   unitizer after re-evaluating all the tests even if all tests passed.
#' @param test.dir the directory to run the tests on
#' @param test.file.regex a regular expression used to match which files in
#'   \code{test.dir} to \code{unitize}
#' @param unitizer.ids one of \itemize{
#'   \item a function that converts test file names to \code{unitzer} ids; if
#'     \code{unitize}ing multiple files will be \code{lapply}ed over each file
#'   \item a character vector with \code{unitizer} ids, must be the same
#'     length as the number of test files being reviewed
#'   \item a list of unitizer ids, must be the same length as the number of
#'     test files being reviewed; useful when you implement special storage
#'     mechanisms for the \code{unitizers} (see \code{\link{get_unitizer}})
#' }
#' @param auto.accept character(XL) ADVANCED USE ONLY: YOU CAN EASILY DESTROY
#'   YOUR \code{unitizer} WITH THIS; whether to auto-accept tests without
#'   prompting, use values in \code{c("new", "failed", "deleted", "error")} to
#'   specify which type(s) of test you wish to auto accept (i.e. same as typing
#'   \code{"Y"} at the \code{unitizer} prompt) or empty character vector to turn
#'   off (default)
#' @param pre.load a directory or a list of objects, if a directory will be
#'   converted to a list of objects by sequentially \code{\link{sys.source}}ing
#'   the files therein into an environment that has for parent
#'   \code{.GlobalEnv}, or \code{par.env} if it is specified.  The objects in
#'   you create/pass will be visible to the tests, but you will not be able to
#'   list them with \code{ls}, etc.
#' @return the \code{unitizer} object updated as per user instructions,
#'   invisibly, or for \code{unitize_dir}, a list of the \code{unitizer}
#'   objects generated by each test file, invisibly

unitize <- function(
  test.file, store.id=sub("(\\.[Rr])?$", ".unitizer", test.file),
  interactive.mode=interactive(),
  par.env=getOption("unitizer.par.env"),
  search.path.clean=getOption("unitizer.search.path.clean"),
  search.path.keep=getOption("unitizer.search.path.keep"),
  force.update=FALSE,
  auto.accept=character(0L),
  pre.load=list()
) {
  invisible(
    unitize_core(
      infer_unitizer_location(test.file), list(store.id),
      interactive.mode=interactive.mode, par.env=par.env,
      search.path.clean=search.path.clean,
      search.path.keep=search.path.keep,
      force.update=force.update, auto.accept=auto.accept, pre.load=pre.load,
      mode="unitize"
  ) )
}
#' @rdname unitize
#' @export

review <- function(
  store.id,
  par.env=getOption("unitizer.par.env"),
  search.path.clean=getOption("unitizer.search.path.clean"),
  search.path.keep=getOption("unitizer.search.path.keep")
) {
  if(!interactive()) stop("`review` only available in interactive mode")
  store.id.inf <-
  invisible(
    unitize_core(
      test.files=NA_character_,
      store.ids=list(infer_unitizer_location(store.id, type="u")),
      interactive.mode=TRUE,
      par.env=par.env, search.path.clean=search.path.clean,
      search.path.keep=search.path.keep, force.update=FALSE,
      auto.accept=character(0L), pre.load=list(), mode="review"
  ) )
}
#' @rdname unitize
#' @export

unitize_dir <- function(
  test.dir,
  store.ids=function(x) sub("(\\.[Rr])?$", ".unitizer", x),
  pattern="^[^.].*\\.[Rr]$",
  interactive.mode=interactive(),
  par.env=getOption("unitizer.par.env"),
  search.path.clean=getOption("unitizer.search.path.clean"),
  search.path.keep=getOption("unitizer.search.path.keep"),
  force.update=FALSE,
  auto.accept=character(0L),
  pre.load=file.path(test.dir, "helper"),
  mode="unitize"
) {
  # Validations
  if(
    !is.character(test.dir) || length(test.dir) != 1L || is.na(test.dir)
  )
    stop("Argument `test.dir` must be character(1L) and not NA.")
  if(
    !is.character(test.file.regex) || length(test.file.regex) != 1L ||
    is.na(test.file.regex)
  )
    stop("Argument `test.file.regex` must be character(1L) and not NA.")

  if(file.exists(test.dir) && !file_test("-d", test.dir))
    stop("Argument `test.dir` points to a file instead of a directory")

  # Infer

  test.dir <- infer_unitizer_location(test.dir, type="d")

  if(!file_test("-d", test.dir)) {
    stop("Argument `test.dir` must point to a direcctory")
  }
  test.files <- list.files(
    path=test.dir, pattern=pattern, all.files=TRUE, full.names=TRUE, no..=TRUE
  )
  # And unitizers

  if(is.function(unitizer.ids)) {
    unitizer.ids <- try(lapply(test.files, unitizer.ids))
    if(inherits(unitizer.ids, "try-error")) {
      stop(
        "Argument `store.ids` is a function, but caused an error when ",
        "attempting to use it to convert test file names to `unitizer` ids."
      )
    }
  }
  unitize_core(
    test.files=test.files, store.ids=store.ids,
    interactive.mode=interactive.mode, par.env=par.env,
    search.path.clean=search.path.clean,
    search.path.keep=search.path.keep,
    force.update=force.update, auto.accept=auto.accept, pre.load=pre.load,
    mode="unitize"
  )
}
