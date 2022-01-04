# Copyright (C) 2021 Brodie Gaslam
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

#' Convert a \code{testthat} Test File to a \code{unitizer}
#'
#' Converts a \bold{copy} of an existing \code{testthat} test file to a
#' \code{unitizer} test file and test store, or a directory of such files to
#' a corresponding \code{unitizer} directory.  See examples.
#'
#' @section Disclaimers:
#'
#' If you already have an extensive test suite in \code{testthat} and you do not
#' intend to modify your tests or code very much there is little benefit (and
#' likely some drawbacks) to migrating your tests to \code{unitizer}.  Please
#' see the introduction vignette for a (biased) view of the pros and cons of
#' \code{unitizer} relative to \code{testthat}.
#'
#' These translation functions are provided for your convenience.  The
#' \code{unitizer} author does not use them very much since he seldom needs to
#' migrate \code{testthat} tests.  As a result, they have not been tested as
#' thoroughly as the rest of \code{unitizer}.  Translation is designed to work
#' for the most common \code{testthat} use cases, but may not for yours.  Make
#' sure you \code{\link{review}} the resulting \code{unitizer}s to make sure
#' they contain what you expect before you start relying on them.  This is
#' particularly important if your \code{testthat} test files are not meant to
#' be run stand-alone with just \code{test_file} (see "Differences That May
#' Cause Problems").
#'
#' Note you can also \code{unitize} your \code{testthat} files \bold{without}
#' translating them (see notes).
#'
#' @section Workflow:
#'
#' \enumerate{
#'   \item Start a fresh R session
#'   \item Run your \code{testthat} tests with \code{test_dir} to
#'     ensure they are still passing.  If your tests are are runnable only
#'     via \code{test_check} because they directly access the namespace of your
#'     package, see "Differences That May Cause Problems" below
#'   \item Run \code{testthat_dir_translate}
#'   \item [optional] use \code{\link{review}} to review the resulting
#'      unitizer(s)
#' }
#' We recommend using \code{testthat_translate_dir} over
#' \code{testthat_translate_file} because the former also copies and loads any
#' helper files that may be defined.  Since libraries used by multiple test
#' files are commonly loaded in these helper files, it is likely that just
#' translating a single file without also copying the helper files will not
#' work properly.
#'
#' @section How the Conversion Works:
#'
#' For a subset of the \code{expect_*} functions we extract the \code{object}
#' parameter and discard the rest of the expectation.  For example
#' \preformatted{expect_equal(my_fun(25), 1:10)} becomes
#' \preformatted{my_fun(25)}.  The idea is that on unitizing the expression the
#' result will be output to screen and can be reviewed and accepted.
#' Not all \code{expect_*} functions are substituted.  For example,
#' \code{expect_is} and \code{expect_that} are left unchanged because the tests
#' for those functions do not or might not actually test the values of
#' \code{object}.  \code{expect_gt} and similar are also left unchanged as that
#' would require more work than simply extracting the \code{object} parameter.
#'
#' It is perfectly fine to \code{unitize} an \code{expect_*} call unsubstituted.
#' \code{unitizer} captures conditions, values, etc., so if an \code{expect_*}
#' test starts failing, it will be detected.
#'
#' \code{unitizer} will then evaluate and store the results of such expressions.
#' Since in theory we just checked our \code{testthat} tests were working,
#' presumably the re-evaluated expressions will produce the same values.  Please
#' note that the translation process does not actually check this is true (see
#' "Differences That May Cause Problems") so \code{review}ing the results is a
#' good idea.
#'
#' \code{test_that} calls are converted to \code{\link{unitizer_sect}} calls,
#' and the contents thereof are processed as described above.  Calls to
#' \code{context} are commented out since there currently is no \code{unitizer}
#' equivalent.  Other \code{testthat} calls are left unchanged and their return
#' values used as part of the \code{unitizer} tests.
#'
#' Only top level calls are converted.  For example, code like
#' \code{for(i in 1:10) expect_equal(my_fun(i), seq(i))} or even
#' \code{(expect_equal(my_fun(10), 1:10))} will not be converted since
#' \code{expect_equal} is nested inside a \code{for} and \code{(} respectively.
#' You will need to manually edit these calls (or just let them remain as is,
#' which is not an issue).
#'
#' We identify calls to extract based purely on the function symbols (i.e. we
#' do not check whether \code{expect_equal} actually resolves to
#' \code{testthat::expect_equal} in the context of the test file).
#'
#' The \code{unitizer} files will be created in a sibling folder to the folder
#' containing the \code{testthat} files.  The names of the new files will be
#' based on the old files.  See params \code{target.dir}, \code{name.new},
#' \code{name.pattern}, and \code{name.replace} for more details.  We encourage
#' you to try the default settings first as those should work well in most
#' cases.
#'
#' When using \code{testthat_translate_dir}, any files that match
#' \code{"^helper.*[rR]$"} are copied over to a '/_pre' subdirectory
#' in \code{"target.dir"}, and are pre-loaded by default before the tests are
#' \code{unitize}d.
#'
#' @section \code{unitizer} Differences That May Cause Problems:
#'
#' If you run your tests during development with \code{test_dir} odds
#' are the translation will work just fine.  On the other hand, if you rely
#' exclusively on \code{test_check} you may need to use
#' \code{state=unitizerStateNoOpt(par.env="pkgName")} when you translate to
#' make sure your tests have access to the internal namespace functions.
#' See \code{\link{unitizerState}} for details on how to modify state tracking.
#'
#' If your tests were translated with the \code{state} parameter changed from
#' its default value, you will have to use the same value for that parameter in
#' future \code{unitize} or \code{unitize_dir} runs.
#'
#' @section Alternate Use Cases:
#'
#' If you wish to process \code{testthat} files for use with the standard R
#' \dQuote{.Rout} / \dQuote{.Rout.save process} you can set the \code{unitize}
#' and \code{use.sects} parameters to FALSE.
#'
#' @export
#' @seealso \code{\link{unitize}}, \code{\link{unitizerState}}
#' @aliases testthat_translate_name testthat_translate_dir
#' @param file.name a path to the \code{testthat} test file to convert
#' @param dir.name a path to the \code{testthat} directory to convert
#' @param target.dir the directory to create the \code{unitizer} test file and
#'   test store in; for \code{testthat_translate_file} only: if NULL will return
#'   as a character vector what the contents of the translated file would have
#'   been instead of writing the file
#' @param keep.testthat.call whether to preserve the \code{testthat} call that
#'   was converted, as a comment
#' @param filter regular expression to select what files in a director are
#'   translated
#' @param ... params to pass on to \code{testthat_translate_name}
#' @param name.new character(1L) the base name for the \code{unitizer} files;
#'   do not include an extension as we will add it (".R" for the testfile,
#'   ".unitizer" for the data directory); set to NULL to generate the name
#'   from the \code{testthat} file name
#' @param name.pattern character(1L) a regular expression intended to match
#'   the \code{testthat} test file name (see \code{name.replace}) if
#'   \code{name.pattern} matches, then the new file name will be
#'   constructed with this (used as \code{replace} parameter to
#'   \code{\link{sub}}); in addition we will add ".R" and ".unitizer" as the
#'   extensions for the new files so do not include extensions in your
#'   \code{name.replace} parameter
#' @param name.replace character(1L) the replacement token, typically would
#'   include a \code{"\\1"} token that is filled in by the match group from
#'   \code{name.pattern}
#' @param prompt character(1L): \itemize{
#'     \item "always" to always prompt before writing new files
#'     \item "overwrite" only prompt if existing file is about to be overwritten
#'     \item "never" never prompt
#'   }
#' @param force logical(1L) whether to allow writing to a \code{target.dir} that
#'   contains files (implies \code{prompt="never"} when
#'   \code{testthat_translate_dir} runs \code{testthat_translate_file})
#' @param state what state control to use (see same argument for
#'   \code{\link{unitize}})
#' @param interactive.mode logical(1L) primarily for testing purposes, allows
#'   us to force prompting in non-interactive mode; note that \code{unitize}
#'   and \code{unitize_dir} are always called in non-interactive mode by these
#'   functions, this parameter only controls prompts generated directly by these
#'   functions.
#' @param use.sects TRUE (default) or FALSE whether to translate
#'   \code{test_that} sections to \code{unitizer_sect} or simply to turn them
#'   into comment banners.
#' @param unitize TRUE (default) or FALSE whether to run \code{unitize} after
#'   the files are translated.
#' @return a file path or a character vector (see \code{target.dir})
#' @examples
#' \dontrun{
#' library(testthat)  # required
#' testthat_translate_file("tests/testthat/test-random.R")
#'
#' # Translate `dplyr` tests (assumes `dplyr` source is in './dplyr')
#' # Normally we would use default `state` value but we cannot in this case
#' # due to conflicting packages and setup
#'
#' testthat_translate_dir(
#'   "dplyr/tests/testthat", state=unitizerStateSafe(par.env="dplyr")
#' )
#' # Make sure translation worked (checking one file here)
#' # *NOTE*: folder we are looking at has changed
#'
#' review("dplyr/tests/unitizer/summarise.unitizer")
#'
#' # Now we can unitize any time we change our code
#'
#' unitize_dir(
#'   "dplyr/tests/unitizer", state=unitizerStateSafe(par.env="dplyr")
#' )
#' }

testthat_translate_file <- function(
  file.name, target.dir=file.path(dirname(file.name), "..", "unitizer"),
  state=getOption("unitizer.state"),
  keep.testthat.call=TRUE, prompt="always", interactive.mode=interactive(),
  use.sects=TRUE, unitize=TRUE, ...
) {
  untz.file <- testthat_transcribe_file(
    file.name, target.dir, keep.testthat.call, prompt,
    interactive.mode=interactive.mode, use.sects=use.sects, ...
  )
  if(!is.null(target.dir) && unitize) {
    unitize(
      test.file=untz.file, auto.accept="new", state=state,
      interactive.mode=FALSE
    )
  }
  return(untz.file)
}
#' Transcribes a \code{testtaht} File Into \code{unitizer} Format
#'
#' Internal use only, required so we can ensure the parse succeeded because of
#' possible parse-deparse issues independent of running \code{unitize}, since
#' \code{unitize} cannot be run inside a \code{tryCatch} block.
#'
#' @keywords internal
#' @inheritParams testthat_translate_file

testthat_transcribe_file <- function(
  file.name, target.dir=file.path(dirname(file.name), "..", "unitizer"),
  keep.testthat.call=TRUE, prompt="always", interactive.mode, use.sects=TRUE,
  ...
) {
  if(!is.character(file.name) || length(file.name) != 1L)
    stop("Argument `file.name` must be character(1L)")
  if(!file_test("-f", file.name))
    stop("Argument `file.name` does not point to a readable file")
  if(isTRUE(!use.sects %in% c(TRUE, FALSE)))
    stop("Argument `use.sects` must be TRUE or FALSE.")
  valid.prompt <- c("always", "overwrite", "never")
  if(
    !is.character(prompt) || length(prompt) != 1L || is.na(prompt) ||
    !prompt %in% valid.prompt
  )
    stop(
      "Argument prompt must be character(1L), not NA, and in ",
      deparse(valid.prompt)
    )
  funs.to.extract <- tt_trans_funs

  # These should probably be defined at top level in package...

  cln.dbl <- quote(`::`)
  cln.trp <- quote(`:::`)
  tt.symb <- quote(testthat)
  t_t.symb <- quote(test_that)
  cont.symb <- quote(context)

  # convert the calls back to character, done as an in-body function since only
  # ever called here, and a bunch of variables are useful to share

  testthat_extract_all <- function(expr, mode="all", use.sects=TRUE) {

    stopifnot(mode %in% c("all", "sub"))
    result.final <- character()

    for(i in seq_along(expr)) {
      success <- FALSE
      result <- character()

      if(is.call(expr[[i]])) {
        # pull out function symbol for idendification

        sub.call <- expr[[i]][[1L]]
        if(
          is.call(sub.call) && length(sub.call) == 3L && (
            identical(sub.call[[1L]], cln.dbl) ||
            identical(sub.call[[1L]], cln.trp)
          ) && identical(sub.call[[2L]], tt.symb)
        ) {
          sub.call <- sub.call[[3L]]
        }
        if(
          is.symbol(sub.call) && identical(sub.call, t_t.symb)
        ) {
          # test_that call requires special handling,

          if(!identical(mode, "all")) {  # check we don't have nested `test_that`
            result <- c(
              result, paste0(
                "# [ERROR: testthat -> unitizer] cannot extract nested ",
                "`test_that` calls"
            ) )
            result <- c(result, deparse(expr[[i]]))
          } else {
            # First extract params

            res.extract <- testthat_match_call(
              expr[[i]], tt_fun, c("code", "desc")
            )
            if(any(nchar(res.extract$msg))) { # failed
              result <- c(result, res.extract$msg)
              result <- c(result, deparse(expr[[i]]))
            } else {
              result <- c(
                result, attr(expr[[i]], "comment"),
                if(keep.testthat.call)
                  paste0("# ", deparse(comm_and_call_extract(expr[[i]])$call))
              )
            }
            # Now parse the `code` param looking for

            code <- res.extract$call$code
            code.block <- FALSE
            if(
              code.block <- is.language(code) && length(code) > 1L &&
              identical(code[[1L]], as.name("{"))
            ) {
              sub.expr <- code[-1L]
            } else sub.expr <- code
            sub.res <- Recall(sub.expr, mode="sub")
            if(code.block && use.sects) {
              sub.res <- paste0(
                c("{", paste0("    ", sub.res, collapse="\n"), "}"),
                collapse="\n"
              )
            } else sub.res <- paste0(sub.res, collapse="\n")
            # Put it all together

            new <- if(use.sects) {
              paste0(
                "unitizer_sect(", paste0(deparse(res.extract$call$desc), sep=""),
                ", ", sub.res, ")"
              )
            } else {
              c(
                paste0(
                  "# ",
                  as.character(
                    H3(paste0(deparse(res.extract$call$desc), sep="")),
                    width=getOption('width') - 2
                  )
                ),
                sub.res
              )
            }
            result <- c(result, new)
          }
        } else if (
          is.symbol(sub.call) && identical(sub.call, cont.symb)
        ) {
          res.pre <- comm_and_call_extract(expr[[i]])
          result <- c(
            result, res.pre$comments,
            paste0("# ", paste0(deparse(res.pre$call), collapse="\n"))
          )
        } else {
          # pull out comments for all of these if relevant

          res.pre <- comm_and_call_extract(expr[[i]])
          result <- c(result, res.pre$comments)

          # expect_* or other similar test that calls require extraction

          if(
            is.symbol(sub.call) && (
              fun.id <- match(
                as.character(sub.call),
                names(tt_trans_funs), nomatch=0L
            ) )
          ) {
            res.extract <- testthat_match_call(
              res.pre$call, tt_trans_funs[[fun.id]],
              c("object") # should probably also check for `expected` or `regexp`, but current logic doesn't allow...
            )
            result <- c(
              result,
              if(any(nchar(res.extract$msg))) res.extract$msg,
              if(keep.testthat.call)
                  paste0("# ", deparse(comm_and_call_extract(expr[[i]])$call)),
              deparse(res.extract$call[["object"]])
            )
          } else {  # normal calls or anything
            result <- c(result, deparse(res.pre$call))
          }
        }
      } else {
        # Not a call; not sure whether we can actually ever get here due to
        # parse_with_comments wrapping, but just in case

        res <- comm_and_call_extract(expr[[i]])
        result <- c(result, res$comments, deparse(res$call))
      }
      result.final <- c(result.final, result)
    }
    result.final
  }
  # Parse and translate

  parsed <- parse_tests(file.name)
  translated <- testthat_extract_all(parsed, use.sects=use.sects)

  if(!is.null(target.dir)) {
    # Create unitizer

    untz.base <- testthat_translate_name(file.name, target.dir, ...)
    untz.test <- paste0(untz.base, ".R")

    # prompt if needed to create directories

    if(file.exists(target.dir) && ! file_test("-d", target.dir)) {
      stop("Argument `target.dir` must be a directory")
    }
    if(!file.exists(target.dir)) {
      if(!identical(prompt, "never") && !identical(prompt, "overwrite")) {
        u.inp <- if(interactive.mode) {
          simple_prompt(
            paste0("Create directory ", target.dir," for unitizer tests?")
          )
        } else "N"
        if(!identical(u.inp, "Y"))
          stop("Unable to proceed without creating target directory")
      }
      if(
        inherits(
          try(dir.create(target.dir, recursive=TRUE)), "try-error"
        )
      )
        stop(
          "Unable to create test directory `", normalize_path(target.dir),
          "`; see prior errors."
        )
    }
    # prompt if file already exists

    if(!identical(prompt, "never") && file.exists(untz.test)) {
      u.inp <- if(interactive.mode) {
        simple_prompt(
          paste0("Overwrite file '", normalize_path(untz.test), "'?")
        )
      } else "N"
      if(!identical(u.inp, "Y"))
        stop(
          "Unable to proceed without user approval as `",
          untz.test, "` already exists."
        )
    }
    # Create files, run tests ...

    if(inherits(try(file.create(untz.test)), "try-error"))
      stop("Unable to create test file '", untz.test, "'; see prior errors.")

    try(cat(translated, file=untz.test, sep="\n"))
    return(untz.test)
  }
  return(translated)
}
#' @rdname testthat_translate_file
#' @export

testthat_translate_dir <- function(
  dir.name, target.dir=file.path(dir.name, "..", "unitizer"),
  filter="^test.*\\.[rR]", state=getOption("unitizer.state"),
  keep.testthat.call=TRUE, force=FALSE, interactive.mode=interactive(),
  use.sects=TRUE, unitize=TRUE, ...
) {
  # Validate

  chr.1.args <- list(
    dir.name=dir.name, target.dir=target.dir, filter=filter
  )
  for(i in names(chr.1.args))
    if(
      !is.character(chr.1.args[[i]]) || length(chr.1.args[[i]]) != 1L ||
      is.na(chr.1.args[[i]])
    )
      stop("Argument `", i, "` must be character(1L) and not NA")

  if(!isTRUE(force) && !identical(force, FALSE))
    stop("Argument `prompt` must be TRUE or FALSE")

  if(!file_test("-d", dir.name))
    stop("Argument `", dir.name, "` is not a directory name")

  # Get file names

  file.list <- sort(dir(dir.name))
  files.helper <- normalize_path(
    file.path(dir.name, grep("^helper.*[rR]$", file.list, value=TRUE))
  )
  files.test <- normalize_path(
    file.path(dir.name, grep(filter, file.list, value=TRUE))
  )
  res <- character(length(files.test))
  unparseable <- unparseable.src <-  character()

  if(length(files.test)) {
    # Checks
    if(file.exists(target.dir) && !file_test("-d", target.dir))
      stop("`target.dir` (", target.dir, ") exists but is not a directory")
    if(
      file.exists(target.dir) && !force &&
      length(dir(all.files=TRUE, include.dirs=TRUE, no..=TRUE))
    )
      stop(
        "`target.dir` '", normalize_path(target.dir) ,"' contains files so we ",
        "cannot proceed; manually clear or set `force` to TRUE.  This is a ",
        "safety feature to ensure files are not accidentally overwritten."
      )

    dir.create(target.dir, recursive=TRUE)

    # Load helper files and copy them to new location

    if(length(files.helper)) {
      dir.create(help.dir <- file.path(target.dir, "_pre"))
      file.copy(files.helper, help.dir)
    }
    # Translate files, need to unitize one by one mostly because we wrote the
    # `testthat_translate_file` function first, but would probably be better
    # if we separate the file translation and unitizing

    for(i in seq_along(files.test)) {
      # Attempt to parse to make sure parse -> deparse translation didn't go
      # awry

      untz.file <- testthat_transcribe_file(
        files.test[[i]], target.dir, keep.testthat.call, prompt="never",
        interactive.mode=interactive.mode, use.sects=use.sects, ...
      )
      res[[i]] <- untz.file
      if(inherits(try(parse(untz.file)), "try-error")) {
        unparseable[[length(unparseable) + 1L]] <- untz.file
        unparseable.src[[length(unparseable.src) + 1L]] <- files.test[[i]]
    } }
    # Exclude failing files so we can just unitize the directory

    unlink(unparseable)

    # Unitize all files in directory

    if(unitize) {
      unitize_dir(
        test.dir=target.dir, auto.accept="new", state=state,
        interactive.mode=FALSE
      )
    }
  }
  if(length(unparseable))
    warning(
      "Unable to parse the translated versions of the following file(s), ",
      "so they are not unitized:\n",
      paste0("  * ", basename(unparseable.src), collapse="\n"),
      "\nThis likely happed because they contain language constructs that do ",
      " not survive the parse - deparse cycle.  You can re-run the translation ",
      "with `testthat_translate_file` and look at the resulting translated ",
      "file.  Additionally, the parse error should be part of the output above."
    )
  invisible(res)
}
#' @rdname testthat_translate_file
#' @export

testthat_translate_name <- function(
  file.name, target.dir=file.path(dirname(file.name), "..", "unitizer"),
  name.new=NULL, name.pattern="^(?:test\\W*)?(.*)(?:\\.[rR])$",
  name.replace="\\1"
) {
  # Check args

  args <- as.list(environment())
  for(i in names(args)) {
    if(identical(i, "name.new")) next
    arg <- args[[i]]
    if(!is.character(arg) || length(arg) != 1L || is.na(arg))
      stop("Argument `", i, "` must be character(1L) and not NA")
  }
  if(
    !is.null(name.new) && !(
      is.character(name.new) || length(name.new) != 1L || is.na(name.new)
    )
  )
    stop("Argument `name.new` must be NULL, or character(1L) and not NA")

  # Special cases

  if(is.null(target.dir)) return(NULL)
  if(!is.null(name.new)) {
    if(basename(name.new) != name.new)
      stop(
        "Argument `name.new` should be a file name without any directories ",
        "specified; you may specify those with `target.dir`"
      )
    return(file.path(target.dir, name.new))
  }

  # Transform name

  base.new <- sub(name.pattern, name.replace, basename(file.name))
  if(!nchar(base.new))
    stop(
      "Produced zero char name when attempting to make unitizer file name ",
      "from `testthat` file name, please review `file.name`, `name.pattern`, ",
      "and `name.replace`"
    )
  if(basename(base.new) != base.new)
    stop(
      "File name creating process produced sub-directories; make sure ",
      "`name.pattern` and `name.replace` are such that the resulting file ",
      "names do not contain sub-directories"
    )
  file.path(target.dir, base.new)
}
# Pull out parameter from call
#
# @param target.params parameters required in the matched call

testthat_match_call <- function(call, fun, target.params) {
  call.matched <- try(match.call(definition=fun, call), silent=TRUE)
  fail.msg <- ""
  if(inherits(call.matched, "try-error")) {
    fail.msg <- conditionMessage(attr(call.matched, "condition"))
    if(!any(nchar(fail.msg))) fail.msg <- "Failed matching call"
  } else if (!all(param.matched <- target.params %in% names(call.matched))) {
    fail.msg <- paste0(
      "`", paste0(deparse(target.params[!param.matched]), sep=""),
      "` parameter", if(length(which(!param.matched)) > 1L) "s",
      " missing"
    )
  }
  if(any(nchar(fail.msg))) {
    fail.msg <- paste0(
      "# ",
      word_wrap(
        paste0(
          "[ERROR: testthat -> unitizer] ", paste0(fail.msg, collapse="\n")
        ), width=78L
    ) )
  }
  list(call=call.matched, msg=fail.msg)
}
# Signature of translatable funs

tt_trans_funs <- list(
  expect_condition = function (object, regexp = NULL, class = NULL,
    ..., info = NULL, label = NULL)
NULL,
  expect_equal = function (object, expected, ..., tolerance = if (edition_get() >=
      3) testthat_tolerance(), info = NULL, label = NULL, expected.label = NULL)
  NULL,
  expect_equivalent = function (object, expected, ..., info = NULL,
      label = NULL, expected.label = NULL)
  NULL,
  expect_error = function (object, regexp = NULL, class = NULL,
      ..., info = NULL, label = NULL)
  NULL,
  expect_false = function (object, info = NULL, label = NULL)
  NULL,
  expect_identical = function (object, expected, info = NULL,
      label = NULL, expected.label = NULL, ...)
  NULL,
  expect_message = function (object, regexp = NULL, class = NULL,
      ..., all = FALSE, info = NULL, label = NULL)
  NULL,
  expect_null = function (object, info = NULL, label = NULL)
  NULL,
  expect_output = function (object, regexp = NULL, ..., info = NULL,
      label = NULL, width = 80)
  NULL,
  expect_silent = function (object)
  NULL,
  expect_true = function (object, info = NULL, label = NULL)
  NULL,
  expect_warning = function (object, regexp = NULL, class = NULL,
      ..., all = FALSE, info = NULL, label = NULL)
  NULL
)
# test_that signature

tt_fun <- function (desc, code) NULL
