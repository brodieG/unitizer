#' Unitize an R Test Script
#'
#' Turn standard R scripts into unit tests by evaluating the expressions and
#' storing them along with their resuls.
#'
#' \code{unitize} creates unit tests from a single R file, and
#' \code{unitize_dir} creates tests from all the R files in the specified
#' directory (analogous to \code{testthat::test_dir}).
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
#' \code{review} allows you to review existing \code{unitizer}s and modify them
#' by dropping tests from them.  Tests are not evaluated in this mode; you are
#' just allowed to review the results of previous evaluations of the tests
#' Because of this, no effort is made to create reproducible state in the
#' browsing environments, unlike with \code{unitize} or \code{unitize_dir}
#' (see \code{reproducible.state} parameter).
#'
#' See \code{unitizer} vignettes and demo for details and examples.
#'
#' @export
#' @aliases unitize review unitize_dir
#' @seealso \code{\link{get_unitizer}}, \code{\link{infer_unitizer_location}}
#' @param test.file path to the file containing tests, if supplied path does not
#'   match an actual system path, \code{unitizer} will try to infer a possible
#'   path (see \code{\link{infer_unitizer_location}})
#' @param test.dir the directory to run the tests on
#' @param pattern a regular expression used to match what subset of files in
#'   \code{test.dir} to \code{unitize}
#' @param store.id if NULL (default), \code{unitizer} will select a directory
#'   based on the \code{test.file} name by replacing \code{.[rR]} with
#'   \code{.unitizer}.  You can also specify a directory name, or pass any
#'   object that has a defined \code{\link{get_unitizer}} method which allows
#'   you to specify non-standard \code{unitizer} storage mechanisms (see
#'   \code{\link{get_unitizer}}).  Finally, you can pass an actual
#'   \code{unitizer} object if you are using \code{review}.
#' @param store.ids one of \itemize{
#'   \item a function that converts test file names to \code{unitizer} ids; if
#'     \code{unitize}ing multiple files will be \code{lapply}ed over each file
#'   \item a character vector with \code{unitizer} ids, must be the same
#'     length as the number of test files being reviewed (see \code{store.id})
#'   \item a list of unitizer ids, must be the same length as the number of
#'     test files being reviewed; useful when you implement special storage
#'     mechanisms for the \code{unitizers} (see \code{\link{get_unitizer}})
#' }
#' @param state character(1L) one of
#'   \code{c("prisitine", "noopt", "basic", "off")} or an S4 object inheriting
#'   from \code{unitizerState}; modifies how \code{unitizer} manages aspects
#'   of session state that could affect test evaluation (see details).
#' @param pre NULL, or a character vector pointing to files and/or directories.
#'   If a character vector, then any files referenced therein will be sourced,
#'   and any directories referenced therein will be scanned non-recursively for
#'   visible files ending in ".r" or ".R", which are then also sourced.  If
#'   NULL, then \code{unitizer} will look for a directory named "_pre" in the
#'   directory containing the first test file and will treat it as if you had
#'   specified it in \code{pre}.  Any objects created by those scripts will be
#'   put into a parent environment for all tests.  This provides a mechanism for
#'   creating objects that are shared across different test files, as well as
#'   loading shared packages.  Unlike objects created during test evaluation,
#'   any objects created here will not be stored in the \code{unitizer} so you
#'   will have not direct way to check whether these objects changed across
#'   \code{unitizer} runs.  Additionally, typing \code{ls} from the review
#'   prompt will not list these objects.
#' @param post NULL, or a character vector pointing to files and/or directories.
#'   See \code{pre}.  If NULL will look for a directory named "_post" in the
#'   directory containing the first test file.  Scripts are run just prior to
#'   existing \code{unitizer}.  Keep in mind that \code{unitizer} can manage
#'   most aspects of global state, so you should not need to use this parameter
#'   to unload packages, remove objects, etc.  See details.
#' @param interactive.mode logical(1L) whether to run in interactive mode (
#'   request user input when needed) or not (error if user input is required,
#'   e.g. if all tests do not pass).
#' @param force.update logical(1L) if TRUE will give the option to re-store a
#'   unitizer after re-evaluating all the tests even if all tests passed.
#' @param auto.accept character(XL) ADVANCED USE ONLY: YOU CAN EASILY DESTROY
#'   YOUR \code{unitizer} WITH THIS; whether to auto-accept tests without
#'   prompting, use values in \code{c("new", "failed", "deleted", "error")} to
#'   specify which type(s) of test you wish to auto accept (i.e. same as typing
#'   \code{"Y"} at the \code{unitizer} prompt) or empty character vector to turn
#'   off (default)
#' @return the \code{unitizer} object updated as per user instructions,
#'   invisibly, or for \code{unitize_dir}, a list of the \code{unitizer}
#'   objects generated by each test file, invisibly
#'
#' @section Default Settings:
#'
#' Many of the default settings are specfied in the form \code{getOption("...")}
#' to allow the user to "permanently" set them to their prefered modes by
#' setting options in their \code{.Rprofile} file.
#'
#' @section State:
#'
#' While R generally adheres to a "functional" programming style, there are
#' several aspects of session state that can affect the results of code
#' evaluation.  State driven variability in code results is undesirable for
#' unit tests, so \code{unitizer} attempts to insulate test code from some of
#' the major drivers of state induced evaluation variability:
#' \itemize{
#'   \item Workspace
#'   \item Search Path
#'   \item Options
#'   \item Working Directory
#'   \item Random Seed
#' }
#' The default setting is to use a special parent environment for all tests that
#' does not inherit from \code{.GlobalEnv}.  This prevents objects that are
#' laying around in your workspace from interfering with your tests.
#' Additionally both the search path and options are set to what you would
#' typically find in a freshly loaded vanilla R session.  This means any non
#' default packages that are loaded when you run your tests are unloaded prior
#' to running your tests.  If you want to use the same libraries across multiple
#' tests you can load them with the \code{pre} argument.
#'
#' Finally, the random seed is set to a specific value so that tests using
#' random values get the same value at every test iteration.
#'
#' DEVNOTE: TBD about what happens with working directory; set to the
#' directory containing the first test file (TBD FOR wd; MAYBE SHOULD BE
#' SOMETHING DIFFERENT?).
#'
#' State is reset after running each test file when running multiple test
#' files with \code{unitize_dir}, which means state changes in one test file
#' will not affect the next one.
#'
#' Upon exit \code{unitizer} will restore state to what it was on entry.
#'
#' You can modify all aspects of state control with the \code{state} parameter.
#' See the \code{\link{state}} documentation and the \code{state} vignette for
#' more details.

unitize <- function(
  test.file, store.id=NULL,
  state=getOption("unitizer.state"),
  pre=NULL, post=NULL,
  history=getOption("unitizer.history.file"),
  interactive.mode=interactive(),
  force.update=FALSE,
  auto.accept=character(0L)
) {
  test.file.inf <- infer_unitizer_location(test.file)
  store.id.inf <- store.id
  if(is.null(store.id)) store.id.inf <- filename_to_storeid(test.file.inf)
  invisible(
    unitize_core(
      test.file.inf, list(store.id.inf), state=state,
      pre=pre, post=post, history=history,
      interactive.mode=interactive.mode,  force.update=force.update,
      auto.accept=auto.accept, mode="unitize"
  ) )
}
#' @rdname unitize
#' @export

review <- function(store.id) {
  if(!interactive()) stop("`review` only available in interactive mode")
  invisible(
    unitize_core(
      test.files=NA_character_,
      store.ids=list(infer_unitizer_location(store.id, type="u")),
      state="off",
      pre=FALSE, post=FALSE,
      history=getOption("unitizer.history.file"),
      interactive.mode=TRUE,
      force.update=FALSE,
      auto.accept=character(0L),
      mode="review"
  ) )
}
#' @rdname unitize
#' @export

unitize_dir <- function(
  test.dir,
  store.ids=filename_to_storeid,
  pattern="^[^.].*\\.[Rr]$",
  state=getOption("unitizer.state"),
  pre=NULL, post=NULL,
  history=getOption("unitizer.history.file"),
  interactive.mode=interactive(),
  force.update=FALSE,
  auto.accept=character(0L)
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

  test.files <- Filter(
    function(x) file_test("-f", x),
    list.files(
      path=test.dir, pattern=pattern, all.files=TRUE, full.names=TRUE, no..=TRUE
  ) )
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
      par.env=par.env, reproducible.state=reproducible.state,
      pre=pre, post=post, history=history,
      interactive.mode=interactive.mode, force.update=force.update,
      auto.accept=auto.accept, mode="unitize"
  ) )
}
