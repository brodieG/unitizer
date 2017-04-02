#' unitizer
#'
#' Simplifies regression tests by comparing objects produced by test
#' code with earlier versions of those same objects.  If objects are unchanged
#' the tests pass.  `unitizer` provides an interactive interface to review
#' failing tests or new tests.  See vignettes for details.
#'
#' @import methods
#' @import stats
#' @import diffobj
#' @importFrom utils capture.output file_test getParseData installed.packages
#'   loadhistory modifyList object.size packageVersion remove.packages
#'   savehistory
#' @name unitizer
#' @docType package

NULL
