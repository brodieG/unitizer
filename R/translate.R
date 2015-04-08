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
#' @param file.name a path to the \code{testthat} test file to convert
#' @param target.dir the directory to create the \code{unitizer} test file and
#'   test store in
#' @param keep.testthat.call whether to preserve the \code{testthat} call that
#'   was converted, as a comment (does not apply to \code{test_that} calls)
#' @return logical(1L) TRUE on success

testthat_to_unitizer <- function(
  file.name, target.dir = file.path(dirname(file.name), "..", "unitizer"),
  keep.testthat.call = FALSE
) {
  is_testthat_attached()

  # Get function list to extract

  tt.env <- as.environment("package:testthat")
  funs.special <- c("test_that", "context")  # functions that shouldn't be simply replaced
  obj.names <- ls(tt.env)
  obj.list <- mget(fun.names, tt.env)
  obj.funs <- vapply(obj.list, function(x) is.function(x) && ! isS4(x), logical(1L))
  fun.list <- obj.list[obj.funs]
  fun.names <- obj.names[obj.funs]

  fun.to.extract <- sapply(
    fun.list,
    function(x) is.function(x) && "object" %in% names(formals(x))
  ) & ! fun.names %in% funs.special

  # Parse

  comments <- TRUE
  parsed <- try(parse_with_comments(file.name))
  if(inherits(parsed, "try-error")) {
    warning("Unable to recover comments in parse; attempting simple parse")
    parsed <- parse(file.name, keep.source=FALSE)
    comments <- FALSE
  }
  # Extract

  cln.dbl <- quote(::)
  cln.trp <- quote(:::)
  tt.symb <- quote(testthat)
  t_t.symb <- quote(test_that)

  res.expr <- expression()
  res.comments <- list()

  res.idx <- failures <- attempts <- 0L

  for(i in seq_along(parsed)) {
    res.idx <- res.idx + 1L
    res.comments[[res.idx]] <- character(0L)

    if(is.call(parsed[[i]])) {
      sub.call <- parsed[[i]][[1L]]
      if(
        is.call(sub.call) && length(sub.call) == 3L && (
          identical(sub.call[[1L]], cln.dbl) ||
          identical(sub.call[[1L]], cln.trp)
        ) && identical(sub.call[[2L]], tt.symb)
      ) {
        sub.call <- sub.call[[3L]]
      }
      # test_that call requires special handling

      if(
        (sub.call.symb <- is.symbol(sub.call)) && identical(sub.call, test_that)
      ) {
        res.extract <- testtthat_extract(
          parsed[[i]], fun.list[[which(fun.names == "test_that")]], "code"
        )
        if(any(nchar(res.extract$msg))) {
          res.comments[[res.idx]] <- c(res.comments[[res.idx]], res.extract$msg)
        }





      } else {
        # pull out comments for all of these if relevant

        res.pre <- comm_and_call_extract(parsed[[i]])
        res.comments[[res.idx]] <- res.pre$comments

        # expect_* or other similar test that calls require extraction

        if(
          is.symbol(sub.call) &&
          (fun.id <- match(fun.name, fun.names[funs.to.extract], nomatch=0L))
        ) {
          attempts <- attempts + 1L
          res.extract <- testtthat_extract(
            res.pre$call, fun.list[funs.to.extract][[fun.id]], "object"
          )
          res.expr[[res.idx]] <- res.extract$call
          res.comments[[res.idx]] <- c(
            res.comments[[res.idx]],
            if(any(nchar(res.extract$msg))) res.extract$msg
          )
        } else {  # normal calls or anything
          res.expr[[res.idx]] <- res.pre$call
        }


      }
    } else {
      # Not a call; not sure whether we can actually ever get here due to
      # parse_with_comments wrapping, but just in case

      res <- comm_and_call_extract(parsed[[i]])
      res.expr[[res.idx]] <- res$call
      res.comments[[res.idx]] <- res$comments
    }

  }


  # Evaluate each call in the file and extract at the same time













  if(
    inherits(
      try(fun.names <- ls(as.environment("package:testthat"))),
      "try-error"
    )
  ) {
    stop("Package `testthat` must be loaded before you run this function")
  }

  fun.list <- mget(ls(as.environment("package:testthat")), as.environment("package:testthat"))
  fun.names[sapply(fun.list, function(x) is.function(x) && "object" %in% names(formals(x)))]



  for(exp in parsed) {

  }

}
#' Pull out parameter from call
#'
#' @keywords internal

testthat_extract <- function(call, fun, target.param) {
  call.matched <- try(match.call(definition=fun, call), silent=TRUE)
  fail.msg <- ""
  if(inherits(call.matched, "try-error")) {
    fail.msg <- conditionMessage(attr(x, "condition"))
    if(!any(nchar(fail.msg))) fail.msg <- "Failed matching call"
  } else if (! target.param %in% names(call.matched)) {
    fail.msg <- paste0("no `", target.param, "` parameter to match and extract")
  }
  if(any(nchar(fail.msg))) {
    fail.msg <- paste0(
      "# ",
      word_wrap(
        paste0(
          "[ERROR: testthat -> unitizer] ", paste0(fail.msg, collapse="\n")
        ), width=78L
    ) )
  } else call <- call[[target.param]]
  list(call=call, msg=fail.msg)
}


#' Confirm that `testthat` Is Attached
#'
#' Would normally do this via NAMESPACE, but do not want to introduce an
#' explicit dependency to \code{testthat} since it is only required for the
#' conversion to \code{unitizer}

is_testthat_attached <- function() {
  if(!inherits(try(as.environment("package:testthat"), silent=TRUE)))
    stop("Package `testthat` must be loaded")
  TRUE
}

