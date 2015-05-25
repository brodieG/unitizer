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
#' about previously accepted tests and wish to inspect them.  Note though that
#' in review mode the parent environment is always \code{.GlobalEnv} and global
#' settings are not modified in any way, so it is entirely possible that tests
#' will produce different values to those stored when you re-run them from
#' the \code{unitizer} prompt in review mode.
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
#' @seealso \code{\link{get_unitizer}}, \code{\link{infer_unitizer_location}}
#' @param test.file path to the file containing tests, if supplied path does not
#'   match an actual system path, \code{unitizer} will try to infer a possible
#'   path (see \code{\link{infer_unitizer_location}})
#' @param store.id if NULL (default), \code{unitizer} will select a directory
#'   based on the \code{test.file} name by replacing \code{.[rR]} with
#'   \code{.unitizer}.  You can also specify a directory name, or pass any
#'   object that has a defined \code{\link{get_unitizer}} method which allows
#'   you to specify non-standard \code{unitizer} storage mechanisms (see
#'   \code{\link{get_unitizer}}).  Finally, you can pass an actual
#'   \code{unitizer} object if you are using \code{review}
##' @param interactive.mode logical(1L) whether to run in interactive mode (
#'   request user input when needed) or not (error if user input is required,
#'   e.g. if all tests do not pass).
#' @param par.env NULL or environment, if NULL tests are run in a clean
#'   environment, if an environment they are run with that environment as the
#'   parent environment.
#' @param reproducible.global.settings, TRUE, FALSE, or character containing
#'   values in \code{c("search.path", "options", "working.directory")}.
#'   Controls how global settings are managed during test evaluation and review.
#'   Global settings such as the search path, global options, or the working
#'   indirectly affect the result of R expressions.  If TRUE, this parameter
#'    will cause \code{unitizer} to track the global settings throughout test
#'   evaluation and ensure they are the same when reviewing tests as they were
#'   during evaluation.  Additionally, when testing multiple \code{unitizers}
#'    with \code{unitize_dir}, the global settings will be reset prior to
#'   running each \code{unitizer} to what they were after the helper scripts are
#'   first loaded (see \code{pre.load} parameter).  The search path will also be
#'   initialized to the bare bones R search path to ensure it is consistent
#'   across \code{unitizer} runs.  If FALSE, global settings will not be
#'   affected by \code{unitizer} other than by whatever the tests themselves
#'   do.  If character, then any of the global settings included will be tracked
#'   and reset as described above. If any tracking / resetting is enabled,
#'   \code{unitizer} will reset the  global settings upon exit to what they were
#'   prior to running \code{unitizer}.  See "Reproducible Tests" vignette for
#'   more details.
#' @param force.update logical(1L) if TRUE will give the option to re-store a
#'   unitizer after re-evaluating all the tests even if all tests passed.
#' @param test.dir the directory to run the tests on
#' @param pattern a regular expression used to match what subset of files in
#'   \code{test.dir} to \code{unitize}
#' @param store.ids one of \itemize{
#'   \item a function that converts test file names to \code{unitizer} ids; if
#'     \code{unitize}ing multiple files will be \code{lapply}ed over each file
#'   \item a character vector with \code{unitizer} ids, must be the same
#'     length as the number of test files being reviewed (see \code{store.id})
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
#' @param pre.load \code{NULL}, \code{FALSE}, a directory or a list of objects:
#'   \itemize{
#'     \item if \code{NULL}, looks for a 'helper' directory in same directory as
#'       test file (or first test file if using \code{unitize_dir}) and
#'       \code{\link{sys.source}}s the files therein into an environment that
#'       has for parent \code{par.env}
#'     \item if a directory, then the same as \code{NULL}, except it uses files
#'       in the specified directory
#'     \item if a list transforms the list into an environment that has for
#'       parent \code{par.env}
#'     \item if \code{FALSE} does nothing
#'   }
#'   The environment generated by this process will be a parent to the
#'   environments the tests are run in.  The primary purpose of this file is to
#'   run \code{library} calls that are shared by multiple \code{unitizer} files.
#'   Any packages you load in these files will be unloaded upon completion of
#'   the \code{unitize} process unless you modify the \code{search.path.clean}
#'   setting. You can also pre-load objects shared amongst tests, but you should
#'   use this feature sparingly because these objects are not recorded in the
#'   \code{unitizer}s and don't show up in \code{ls} calls from the
#'   \code{unitizer} prompt, which makes it difficult to troubleshoot problems
#'   related to those objects changing between \code{unitizer} runs
#' @return the \code{unitizer} object updated as per user instructions,
#'   invisibly, or for \code{unitize_dir}, a list of the \code{unitizer}
#'   objects generated by each test file, invisibly

unitize <- function(
  test.file, store.id=NULL,
  interactive.mode=interactive(),
  par.env=getOption("unitizer.par.env"),
  reproducible.global.settings=getOption("unitizer.global.settings"),
  force.update=FALSE,
  auto.accept=character(0L),
  pre.load=NULL
) {
  test.file.inf <- infer_unitizer_location(test.file)
  store.id.inf <- store.id
  if(is.null(store.id)) store.id.inf <- filename_to_storeid(test.file.inf)
  invisible(
    unitize_core(
      test.file.inf, list(store.id.inf),
      interactive.mode=interactive.mode, par.env=par.env,
      reproducible.global.settings=reproducible.global.settings,
      force.update=force.update, auto.accept=auto.accept, pre.load=pre.load,
      mode="unitize"
  ) )
}
#' @rdname unitize
#' @export

review <- function(store.id) {
  warning("Need to handle global settings")
  if(!interactive()) stop("`review` only available in interactive mode")
  invisible(
    unitize_core(
      test.files=NA_character_,
      store.ids=list(infer_unitizer_location(store.id, type="u")),
      interactive.mode=TRUE,
      par.env=.GlobalEnv,
      reproducible.global.settings=FALSE,
      force.update=FALSE,
      auto.accept=character(0L), pre.load=list(), mode="review"
  ) )
}
#' @rdname unitize
#' @export

unitize_dir <- function(
  test.dir,
  store.ids=filename_to_storeid,
  pattern="^[^.].*\\.[Rr]$",
  interactive.mode=interactive(),
  par.env=getOption("unitizer.par.env"),
  reproducible.global.settings=getOption("unitizer.global.settings"),
  force.update=FALSE,
  auto.accept=character(0L),
  pre.load=NULL
) {
  # Validations
  if(!is.character(test.dir) || length(test.dir) != 1L || is.na(test.dir))
    stop("Argument `test.dir` must be character(1L) and not NA.")
  if(!is.character(pattern) || length(pattern) != 1L || is.na(pattern))
    stop("Argument `pattern` must be character(1L) and not NA.")
  if(file.exists(test.dir) && !file_test("-d", test.dir))
    stop("Argument `test.dir` points to a file instead of a directory")

  # Infer

  test.dir <- infer_unitizer_location(test.dir, type="d")

  if(!file_test("-d", test.dir))
    stop("Argument `test.dir` must point to a direcctory")

  test.files <- list.files(
    path=test.dir, pattern=pattern, all.files=TRUE, full.names=TRUE, no..=TRUE
  )
  if(!length(test.files))
    stop("No files to test in '", test.dir, "'")

  # And unitizers

  if(is.function(store.ids)) {
    store.ids <- try(lapply(test.files, store.ids))
    if(inherits(store.ids, "try-error")) {
      stop(
        "Argument `store.ids` is a function, but caused an error when ",
        "attempting to use it to convert test file names to `unitizer` ids."
  ) } }
  invisible(
    unitize_core(
      test.files=test.files, store.ids=store.ids,
      interactive.mode=interactive.mode, par.env=par.env,
      reproducible.global.settings=reproducible.global.settings,
      force.update=force.update, auto.accept=auto.accept, pre.load=pre.load,
      mode="unitize"
  ) )
}
