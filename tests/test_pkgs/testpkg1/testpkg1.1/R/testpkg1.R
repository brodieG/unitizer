#' Package for Testing Unitizer
#'
#' @name testpkg1
#' @docType package

NULL

#' @export

fun1 <- function(a, check.num=TRUE, warn=TRUE, warn.extra=FALSE) {
  a.int <- as.integer(a)
  if(check.num) stopifnot(is.numeric(a))
  if(warn && !is.integer(a)) {
    warning("Coercing value to integer")
  }
  if(warn.extra) warning("bonus warning")
  c(tail(a.int, 1L), head(a.int, -1L))
}
#' @export

fun2 <- function(a, rev=TRUE) if(rev) rev(fun1(a)) else fun1(a)

#' @export

fun3 <- function(a, rev=TRUE, warn.extra=TRUE)
  if(rev)
    rev(fun1(a, warn.extra=warn.extra)) else fun1(a, warn.extra=warn.extra)
