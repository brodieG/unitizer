#' Package for Testing Unitizer
#'
#' @name testpkg1
#' @docType package

NULL

#' @export

fun1 <- function(a, check.num=FALSE, warn=FALSE) {
  a.int <- as.integer(a)
  if(check.num) stopifnot(is.numeric(a))
  if(warn && !is.integer(a)) {
    warning("Coercing to integer")
  }
  c(tail(a.int, 1L), head(a.int, -1L))
}
#' @export

fun2 <- function(a, rev=FALSE) if(rev) rev(fun1(a)) else fun1(a)
