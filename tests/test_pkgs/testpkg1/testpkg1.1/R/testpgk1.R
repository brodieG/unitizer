#' Package for Testing Unitizer
#'
#' @name testpkg1
#' @docType package

NULL

#' @export

fun1 <- function(a, check.num=TRUE, warn=TRUE) {
  a.int <- as.integer(a)
  if(check.num) stopifnot(is.numeric(a))
  if(warn && !is.integer(a)) {
    warning("Coercing to integer")
  }
  c(tail(a, 1L), head(a, -1L))
}

