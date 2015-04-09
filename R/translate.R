#' Convert a \code{testthat} Test File to a \code{unitizer}
#'
#' Converts a \bold{copy} of an existing \code{testthat} test file to a
#' \code{unitizer} test file and test store. Expressions tested against
#' expectations are extracted, evaluated, and used as the tests.
#'
#' Workflow is:
#' \enumerate{
#'   \item Run your \code{testthat} test with \code{test_file} or some such to
#'     ensure they are still passing
#'   \item Run \code{testthat_to_unitizer}
#'   \item [optional] use \code{\link{review}} to review the resulting unitizer
#'   \item [optional] manually clean up the test file
#' }
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
#' In order for the conversion to succeed \code{testthat} must be installed
#' on your system.  We do not rely on \code{NAMESPACE} imports to avoid an
#' import dependency on \code{testthat} that is only required for this
#' ancillary function, especially since none of the \code{testthat} functions
#' are called directly.  We use the functions for matching arguments.
#'
#' @export
#' @param file.name a path to the \code{testthat} test file to convert
#' @param target.dir the directory to create the \code{unitizer} test file and
#'   test store in, if NULL will not store the result (note return value is
#'   still useful)
#' @param keep.testthat.call whether to preserve the \code{testthat} call that
#'   was converted, as a comment (does not apply to \code{test_that} calls)
#' @return character the contents of the translated file (saved to
#'   \code{target.dir} if that parameter is not \code{NULL})

testthat_to_unitizer <- function(
  file.name, target.dir = file.path(dirname(file.name), "..", "unitizer"),
  keep.testthat.call = FALSE
) {
  if(!is.character(file.name) || length(file.name) != 1L)
    stop("Argument `file.name` must be character(1L)")
  if(!file_test("-f", file.name))
    stop("Argument `file.name` does not point to a readable file")

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
              success <- TRUE
              result <- c(result, attr(expr[[i]], "comment"))
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
            result <- c(result, if(any(nchar(res.extract$msg))) res.extract$msg)
            result <- c(result, deparse(res.extract$call[["object"]]))
            success <- !any(nchar(res.extract$msg))
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
      if(success && keep.testthat.call) {
        # Add back call as comment

        result <- c(
          paste0("# ", deparse(comm_and_call_extract(expr[[i]])$call)),
          result
        )
      }
      result.final <- c(result.final, result)
    }
    result.final
  }
  # Parse and translate

  parsed <- parse_tests(file.name)

  translated <- testthat_extract_all(parsed)
  if(!is.null(target.dir)) {
    stop("Running/storing Not implemented yet")
  }
  return(translated)
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

