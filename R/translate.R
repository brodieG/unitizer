#' Convert a \code{testthat} Test File to a \code{unitizer}
#'
#' Converts a \bold{copy} of an existing \code{testthat} test file to a
#' \code{unitizer} test file and test store. Expressions tested against
#' expectations are extracted, evaluated, and used as the tests.
#'
#' @section Disclaimers:
#'
#' If you already have an extensive test suite in \code{testthat} and you do not
#' intend to modify your tests or code very much there is little benefit (and
#' likely some drawbacks) to migrating your tests to \code{unitizer}.  Please
#' see the introduction vignette for a (biased) view of the pros and cons of
#' \code{unitizer} relative to \code{testthat}.
#'
#' These translation functions are provided for your convenience.  Keep
#' in mind that the \code{unitizer} author does not use them very much since
#' he seldom needs to migrate \code{testthat} tests.  As a result, they have not
#' been tested as thoroughly as the rest of \code{unitizer}.  Make sure you
#' \code{\link{review}} the resulting \code{unitizer}s to make sure they contain
#' what you expect.  This is particularly important if your \code{testthat}
#' test files are not meant to be run stand-alone with just \code{test_file} (
#' see "Differences That May Cause Problems").
#'
#' @section Workflow:
#'
#' \enumerate{
#'   \item Start a fresh R session
#'   \item Run your \code{testthat} tests with \code{test_file} to
#'     ensure they are still passing.  If you cannot use \code{test_file}
#'     because your tests require prior set-up, or are runnable only via
#'     \code{test_check} because they directly access the namespace of your
#'     package, see "Differences That May Cause Problems" below
#'   \item Run \code{testthat_to_unitizer}
#'   \item [optional] use \code{\link{review}} to review the resulting unitizer
#'   \item [optional] manually clean up the test file
#' }
#'
#' @section What We Do:
#'
#' Conversion works by identifying calls to exported \code{testthat} functions
#' that have an \code{object} argument.  Generally speaking this includes
#' functions of the form \code{expect_*}.
#'
#' Only top level calls, or calls that are nested within the top level of the
#' \code{test_that} \code{code} parameter are converted.  For example, if
#' you nest a \code{expect_true} within anything other than \code{test_that},
#' including just plain parentheses, that call will not be processed.  We match
#' the functions to convert based on symbols only; if you assign a
#' \code{testthat} function to another symbol, or another function to a
#' \code{testthat} symbol the conversion will not know the difference.
#' Additionally we expect that the arguments to the \code{testthat} functions
#' will be the test expressions themselves (as opposed to variables containing
#' test expressions or the like).
#'
#' Calls to \code{test_that} are replaced with calls to \code{unitizer_sect}.
#' Calls to \code{context} are commented out since there currently is no
#' \code{unitizer} equivalent.  Other \code{testthat} calls are left unchanged
#' and their return values used as part of the \code{unitizer} tests.
#'
#' The \code{unitizer} files will be created in a sibling folder to the folder
#' containing the \code{testthat} files.  The names of the new files will be
#' based on the old files.  See params \code{target.dir}, \code{name.new},
#' \code{name.pattern}, and \code{name.replace} for more details.  We encourage
#' you to try the default settings first as those should work well in most
#' cases.
#'
#' @section \code{unitizer} Differences That May Cause Problems:
#'
#' \code{unitize} by default runs each \code{unitizer} test file in a clean
#' environment with a clean search path.  This means that each test file
#' must load all packages, data, etc. that it relies on, including the package
#' you are testing.  If your tests only work properly with the search path as
#' you set it up just before running the tests, or if you need access to the
#' global environment,
#'
#' ... NEED TO RATIONALIZE THIS
#'
#' In these cases you can use \code{\link{unitize_tt}},
#' which is just a wrapper around \code{unitize} with parameters set for
#' behavior closer to what you would get with \code{test_file}.
#'
#' @note In order for the conversion to succeed \code{testthat} must be
#' installed on your system.  We do not rely on \code{NAMESPACE} imports to
#' avoid an import dependency on \code{testthat} that is only required for this
#' ancillary function, especially since none of the \code{testthat} functions
#' are called directly.  We use the functions for matching arguments.
#'
#' @export
#' @aliases testthat_translate_name
#' @param file.name a path to the \code{testthat} test file to convert
#' @param target.dir the directory to create the \code{unitizer} test file and
#'   test store in, if NULL will not store the result (note return value is
#'   still useful)
#' @param keep.testthat.call whether to preserve the \code{testthat} call that
#'   was converted, as a comment (does not apply to \code{test_that} calls)
#' @param ... params to pass on to \code{testthat_translate_name}
#' @param name.new character(1L) the base name for the \code{unitizer} files;
#'   do not include an extension as we will add it (".R" for the testfile,
#'   ".unitizer" for the data directory); set to NULL to generate the name
#'   from the \code{testthat} file name
#' @param name.pattern character(1L) a regular expression intended to match
#'   the \code{testthat} test file name (see \code{name.replace})
#' @param if \code{name.pattern} matches, then the new file name will be
#'   constructed with this (used as \code{replace} parameter to
#'   \code{\link{sub}}); in addition we will add ".R" and ".unitizer" as the
#'   extensions for the new files so do not include extensions in your
#'   \code{replace} parameter
#' @param prompt character(1L): \itemize{
#'     \item "always" to always prompt before writing new files
#'     \item "overwrite" only prompt if existing file is about to be overwritten
#'     \item "never" never prompt
#'   }
#' @param force logical(1L) whether to allow writing to a \code{target.dir} that
#'   contains files (implies \code{prompt="never"} when
#'   \code{testthat_translate_dir}) runs \code{testthat_translate_file})
#' @param eval.env parent environment for tests, use if your \code{testthat}
#'   tests relied on having direct access to package environment as is the case
#'   with tests run with \code{testthat::test_check} (in which case, pass
#'   \code{getNamespace("my_package_name")}); if you use this setting remember
#'   to set the \code{env.clean} parameter to \code{\link{unitize}} to the
#'   same environment any time you run \code{unitize} in the future
#' @return character the contents of the translated file (saved to
#'   \code{target.dir} if that parameter is not \code{NULL})

testthat_translate_file <- function(
  file.name, target.dir=file.path(dirname(file.name), "..", "unitizer"),
  eval.env=parent.frame(), keep.testthat.call=FALSE, prompt="always", ...
) {
  if(!is.character(file.name) || length(file.name) != 1L)
    stop("Argument `file.name` must be character(1L)")
  if(!file_test("-f", file.name))
    stop("Argument `file.name` does not point to a readable file")
  valid.prompt <- c("always", "overwrite", "never")
  if(
    !is.character(prompt) || length(prompt) != 1L || is.na(prompt) ||
    !prompt %in% valid.prompt
  )
    stop(
      "Argument prompt must be character(1L), not NA, and in ",
      deparse(valid.prompt)
    )
  if(!is.null(eval.env) && !is.environment(eval.env))
    stop("Argument `eval.env` must be an environment or NULL")

  is_testthat_attached()

  # Get function list to extract

  tt.env <- as.environment("package:testthat")
  funs.special <- c("test_that", "context")  # functions that shouldn't be simply replaced
  obj.names <- ls(tt.env)
  obj.list <- mget(obj.names, tt.env)
  obj.funs <- vapply(obj.list, function(x) is.function(x) && ! isS4(x), logical(1L))
  fun.list <- obj.list[obj.funs]
  fun.names <- obj.names[obj.funs]

  funs.to.extract <- sapply(
    fun.list,
    function(x) is.function(x) && "object" %in% names(formals(x))
  ) & ! fun.names %in% funs.special

  # These should probably be defined at top level in package...

  cln.dbl <- quote(`::`)
  cln.trp <- quote(`:::`)
  tt.symb <- quote(testthat)
  t_t.symb <- quote(test_that)
  cont.symb <- quote(context)

  # convert the calls back to character, done as an in-body function since only
  # ever called here, and a bunch of variables are useful to share

  testthat_extract_all <- function(expr, mode="all") {

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
                "# [ERROR: testthat -> unitizer] cannot extract nested `test_that` ",
                "calls"
            ) )
            result <- c(result, deparse(expr[[i]]))
          } else {
            # First extract params

            res.extract <- testthat_match_call(
              expr[[i]], fun.list[[which(fun.names == "test_that")]],
              c("code", "desc")
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
              identical(code[[1L]], quote(`{`))
            ) {
              sub.expr <- code[-1L]
            } else sub.expr <- code
            sub.res <- Recall(sub.expr, mode="sub")
            if(code.block) {
              sub.res <- paste0(
                c("{", paste0("    ", sub.res, collapse="\n"), "}"),
                collapse="\n"
              )
            } else sub.res <- paste0(sub.expr, collapse="\n")
            # Put it all together

            result <- c(
              result,
              paste0(
                "unitizer_sect(", paste0(deparse(res.extract$call$desc), sep=""),
                ", ", sub.res, ")"
            ) )
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
                fun.names[funs.to.extract], nomatch=0L
            ) )
          ) {
            res.extract <- testthat_match_call(
              res.pre$call, fun.list[funs.to.extract][[fun.id]], "object"
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
  translated <- testthat_extract_all(parsed)

  if(!is.null(target.dir)) {
    # Create unitizer

    untz.base <- testthat_translate_name(file.name, target.dir, ...)
    untz.test <- paste0(untz.base, ".R")
    untz.store <- paste0(untz.base, ".unitizer")

    # prompt if needed to create directories

    if(file.exists(target.dir) && ! file_test("-d", target.dir)) {
      stop("Argument `target.dir` must be a directory")
    }
    if(!file.exists(target.dir)) {
      if(!identical(prompt, "never") && !identical(prompt, "overwrite")) {
        u.inp <- simple_prompt(
          paste0("Create directory ", target.dir," for `unitizer` tests?")
        )
        if(!identical(u.inp, "Y"))
          stop("Unable to proceed without creating target directory")
      }
      if(
        inherits(
          try(dir.create(target.dir, recursive=TRUE)), "try-error"
        )
      )
        stop(
          "Unable to create test directory `", normalizePath(target.dir),
          "`; see prior errors."
        )
    }
    # prompt if file already exists

    if(!identical(prompt, "never")) {
      if(file.exists(untz.store) || file.exists(untz.test)) {

        u.inp <- simple_prompt(
          paste0(
            "Overwrite files: ",
            paste(
              if(file.exists(untz.store)) normalizePath(untz.store),
              if(file.exists(untz.test)) normalizePath(untz.test),
              sep=", "
        ) ) )
        if(!identical(u.inp, "Y"))
          stop(
            "Unable to proceed without user approval as one of `",
            untz.test, "` or `", untz.store,
            "` already exists."
          )
      }
    }
    # Create files, run tests ...

    if(inherits(try(file.create(untz.test)), "try-error"))
      stop("Unable to create test file `", untz.test, "`; see prior errors.")

    write.test <- try(cat(translated, file=untz.test, sep="\n"))
    if(inherits(write.test, "try-error"))
      stop("Unable to write test file; see previous error")
    unitize(  # run for side effects of creating store
      test.file=untz.test, store.id=untz.store, auto.accept="new",
      env.clean=if(is.null(eval.env)) TRUE else eval.env
    )
  }
  return(translated)
}
#' @export
#' @rdname testthat_translate_file

testthat_translate_dir <- function(
  dir.name, target.dir=file.path(dir.name, "..", "unitizer"),
  filter="^test.*\\.[rR]", eval.env=NULL, keep.testthat.call=FALSE,
  force=FALSE, ...
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

  if(!is.null(eval.env) && !is.environment(eval.env))
    stop("Argument `eval.env` must be an environment or NULL")

  env <- new.env(parent=if(is.null(eval.env)) parent.frame() else eval.env)

  # Get file names

  file.list <- sort(dir(dir.name))
  files.helper <- normalizePath(
    file.path(dir.name, grep("^helper.*[rR]$", file.list, value=TRUE))
  )
  files.test <- normalizePath(
    file.path(dir.name, grep(filter, file.list, value=TRUE))
  )
  res <- vector("list", length(files.test))
  if(length(files.test)) {
    # Checks
    if(file.exists(target.dir) && !file_test("-d", target.dir))
      stop("`target.dir` (", target.dir, ") exists but is not a directory")
    if(
      file.exists(target.dir) && !force &&
      length(dir(all.files=TRUE, include.dirs=TRUE, no..=TRUE))
    )
      stop(
        "`target.dir` contains files so we cannot proceed; manually clear ",
        "or set `force` to TRUE.  This is a safety feature to ensure files are ",
        "not accidentally overwritten."
      )

    # Load helper files

    if(inherits(try(for(i in files.helper) sys.source(i, env)), "try-error"))
      stop("Error pre-loading helper files; see previous errors")

    # Translate files

    env.par <- new.env(parent=env)

    for(i in seq_along(files.test))
      res[[i]] <- testthat_translate_file(
        files.test[[i]], target.dir, eval.env=env.par, keep.testthat.call,
        prompt="never", ...
      )
  }
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
      "Produced zero char name when attempting to make `unitizer` file name ",
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
#' Pull out parameter from call
#'
#' @param target.params parameters required in the matched call
#' @keywords internal

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
#' Confirm that `testthat` Is Attached
#'
#' Would normally do this via NAMESPACE, but do not want to introduce an
#' explicit dependency to \code{testthat} since it is only required for the
#' conversion to \code{unitizer}

is_testthat_attached <- function() {
  if(inherits(try(as.environment("package:testthat"), silent=TRUE), "try-error"))
    stop("Package `testthat` must be loaded and attached to search path")
  TRUE
}

