# Copyright (C) 2020  Brodie Gaslam
# 
# This file is part of "unitizer"
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' Unitize an R Test Script
#'
#' Turn standard R scripts into unit tests by storing the expressions therein
#' along with the results of their evaluation, and provides an interactive
#' prompt to review tests.
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
#' \code{\link{set_unitizer}}.  For more details about storage see those
#' functions.
#'
#' \code{review} allows you to review existing \code{unitizer}s and modify them
#' by dropping tests from them.  Tests are not evaluated in this mode; you are
#' just allowed to review the results of previous evaluations of the tests
#' Because of this, no effort is made to create reproducible state in the
#' browsing environments, unlike with \code{unitize} or \code{unitize_dir}
#' (see \code{state} parameter).
#'
#' You are strongly encouraged to read through the vignettes
#' for details and examples (\code{browseVignettes("unitizer")}).  The demo
#' (\code{demo("unitizer")}) is also a good introduction to these functions.
#'
#' @section Note:
#'
#' \code{unitizer} approximates the semantics of sourcing an R file when running
#' tests, and those of the interactive prompt when reviewing them.  The
#' semantics are not identical, and in some cases you may notice differences.
#' For example, when running tests:
#'
#' \itemize{
#'   \item All expressions are run with \code{options(warn=1)},
#'     irrespective of what the user sets that option to.
#'   \item \code{on.exit(...)} expressions will be evaluated immediately for
#'     top-level statements (either in the test file or in an
#'     \code{\link{unitizer_sect}}, thereby defeating their purpose).
#'   \item Each test expression is run in its own environment, which is enclosed
#'     by that of previous tests.
#'   \item Output and Message streams are sunk so any attempt to debug directly
#'     will be near-impossible as you won't see anything.
#' }
#'
#' When reviewing them:
#'
#' \itemize{
#'   \item \code{ls()} and \code{q()} are over-ridden by \code{unitizer} utility
#'     functions.
#'   \item Expressions are evaluated with \code{options(warn=1)} or greater,
#'     although unlike in test running it is possible to set and keep
#'     \code{options(warn=2)}.
#'   \item Some single upper case letters will be interpreted as \code{unitizer}
#'     meta-commands.
#' }
#'
#' For a more complete discussion of these differences see the introductory
#' vignette (\code{vignette('u1_intro')}), the "Special Semantics" section of
#' the tests vignette (\code{vignette('u2_tests')}), and the "Evaluating
#' Expressions at the \code{unitizer} Prompt" section of the interactive
#' environment vignette (\code{vignette('u3_interactive-env')}).
#'
#' @section Default Settings:
#'
#' Many of the default settings are specified in the form \code{getOption("...")}
#' to allow the user to "permanently" set them to their preferred modes by
#' setting options in their \code{.Rprofile} file.
#'
#' @export
#' @aliases review unitize_dir
#' @param test.file path to the file containing tests, if supplied path does not
#'   match an actual system path, \code{unitizer} will try to infer a possible
#'   path.  If NULL, will look for a file in the \dQuote{tests/unitizer} package
#'   folder if it exists, or in \dQuote{.} if it does not.
#'   See \code{\link{infer_unitizer_location}}) for details.
#' @param test.dir the directory to run the tests on; if NULL will use the
#'   \dQuote{tests/unitizer} package folder if it exists, or \dQuote{.} if it
#'   does not.  See \code{\link{infer_unitizer_location}}) for details.
#' @param pattern a regular expression used to match what subset of files in
#'   \code{test.dir} to \code{unitize}
#' @param store.id if NULL (default), \code{unitizer} will select a directory
#'   based on the \code{test.file} name by replacing \code{.[rR]} with
#'   \code{.unitizer}.  You can also specify a directory name, or pass any
#'   object that has a defined \code{\link{get_unitizer}} method which allows
#'   you to specify non-standard \code{unitizer} storage mechanisms (see
#'   \code{\link{get_unitizer}}).  Finally, you can pass an actual
#'   \code{unitizer} object if you are using \code{review}; see \code{store.ids}
#'   for \code{unitize_dir}
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
#'   \code{c("prisitine", "suggested", "basic", "off", "safe")}, an
#'   environment, or a state object produced by \code{\link{state}} or
#'   \code{\link{in_pkg}}; modifies how \code{unitizer} manages aspects of
#'   session state that could affect test evaluation, including the parent
#'   evaluation environment.  For more details see \code{\link{unitizerState}}
#'   documentation and \code{vignette("unitizer_reproducible_tests")}
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
#'   exiting \code{unitizer}. \code{post} code will be run in an environment
#'   with the environment used to run \code{pre} as the parent.  This means that
#'   any objects created in \code{pre} will be available to \code{post}, which
#'   you can use to your advantage if there are some things you do in \code{pre}
#'   you wish to undo in \code{post}. Keep in mind that \code{unitizer} can
#'   manage most aspects of global state, so you should not need to use this
#'   parameter to unload packages, remove objects, etc.  See details.
#' @param history character(1L) path to file to use to store history generated
#'   during interactive unitizer session; the default is an empty string, which
#'   leads to \code{unitizer} using a temporary file, set to NULL to disable
#'   history capture.
#' @param interactive.mode logical(1L) whether to run in interactive mode (
#'   request user input when needed) or not (error if user input is required,
#'   e.g. if all tests do not pass).
#' @param force.update logical(1L) if TRUE will give the option to re-store a
#'   unitizer after re-evaluating all the tests even if all tests passed.
#'   you can also toggle this option from the unitizer prompt by typing \code{O},
#'   though \code{force.update=TRUE} will force update irrespective of what
#'   you do with \code{O} at the prompt
#' @param auto.accept character(X) ADVANCED USE ONLY: YOU CAN EASILY DESTROY
#'   YOUR \code{unitizer} WITH THIS; whether to auto-accept tests without
#'   prompting, use values in \code{c("new", "failed", "deleted", "error")} to
#'   specify which type(s) of test you wish to auto accept (i.e. same as typing
#'   \code{"Y"} at the \code{unitizer} prompt) or empty character vector to turn
#'   off (default)
#' @param use.diff TRUE or FALSE, whether to use diffs when there is an error,
#'   if FALSE uses \code{\link{all.equal}} instead.
#' @return \code{unitize} and company are intended to be used primarily for
#'   the interactive environment and side effects.  The functions do return
#'   summary data about test outcomes and user input as
#'   \code{unitizer_result} objects, or for \code{unitize_dir} as
#'   \code{unitizer_results} objects, invisbly.  See
#'   \code{\link{unitizer_result}}.
#' @seealso \code{\link{unitizerState}}, \code{\link{unitizer.opts}},
#'   \code{\link{get_unitizer}}, \code{\link{infer_unitizer_location}},
#'   \code{\link{unitizer_result}}

unitize <- function(
  test.file=NULL, store.id=NULL,
  state=getOption("unitizer.state"),
  pre=NULL, post=NULL,
  history=getOption("unitizer.history.file"),
  interactive.mode=interactive(),
  force.update=FALSE,
  auto.accept=character(0L),
  use.diff=getOption("unitizer.use.diff")
) {
  # Initial spacer, must be done in each top level call

  cat("\n")

  test.file.inf <- infer_unitizer_location(test.file)
  if(!file_test("-f", test.file.inf))
    stop("Argument `test.file` must resolve to a file")

  store.id.inf <- store.id
  if(is.null(store.id)) store.id.inf <- filename_to_storeid(test.file.inf)
  invisible(
    unitize_core(
      test.file.inf, list(store.id.inf), state=state,
      pre=pre, post=post, history=history,
      interactive.mode=interactive.mode,  force.update=force.update,
      auto.accept=auto.accept, mode="unitize", use.diff=use.diff
    )[[1L]]
  )
}
#' @rdname unitize
#' @export

review <- function(store.id=NULL) {
  if(!interactive_mode()) stop("`review` only available in interactive mode")
  # Initial spacer, must be done in each top level call

  cat("\n")

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
      mode="review",
      use.diff=TRUE
    )[[1L]]
  )
}
#' @rdname unitize
#' @export

unitize_dir <- function(
  test.dir=NULL,
  store.ids=filename_to_storeid,
  pattern="^[^.].*\\.[Rr]$",
  state=getOption("unitizer.state"),
  pre=NULL, post=NULL,
  history=getOption("unitizer.history.file"),
  interactive.mode=interactive(),
  force.update=FALSE,
  auto.accept=character(0L),
  use.diff=getOption("unitizer.use.diff")
) {
  # Validations
  if(
    (!is.character(test.dir) || length(test.dir) != 1L || is.na(test.dir)) &&
    !is.null(test.dir)
  )
    stop("Argument `test.dir` must be character(1L) and not NA, or NULL.")
  if(!is.character(pattern) || length(pattern) != 1L || is.na(pattern))
    stop("Argument `pattern` must be character(1L) and not NA.")
  if(!is.null(test.dir) && file.exists(test.dir) && !file_test("-d", test.dir))
    stop("Argument `test.dir` points to a file instead of a directory")

  # Initial spacer, must be done in each top level call

  cat("\n")

  # Infer

  test.dir <- infer_unitizer_location(test.dir, type="d")

  if(!file_test("-d", test.dir))
    stop("Argument `test.dir` must point to a direcctory")

  test.files <- Filter(
    function(x) file_test("-f", x),
    sort(
      list.files(
        path=test.dir, pattern=pattern, all.files=TRUE, full.names=TRUE,
        no..=TRUE
  ) ) )
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
      state=state,
      pre=pre, post=post, history=history,
      interactive.mode=interactive.mode, force.update=force.update,
      auto.accept=auto.accept, mode="unitize", use.diff=use.diff
  ) )
}
