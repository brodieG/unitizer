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
#' including just plain parentheses, that call will not be processed.
#'
#' Calls to \code{test_that} are replaced with calls to \code{unitizer_sect}.
#' Calls to \code{context} are commented out since there currently is no
#' \code{unitizer} equivalent.  Other \code{testthat} calls are left unchanged
#' and their return values used as part of the \code{unitizer} tests.
#'
#'
#' We make not guarantees that this function will work correctly as
#' \code{testthat} evolves.
#'
#'
#'

testthat_to_unitizer <- function(
  file.name, target.dir = file.path(dirname(file.name), "..", "unitizer")
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
eval_testthat <- function(expr, env) {

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

