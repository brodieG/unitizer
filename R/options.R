
#' Set Option if Not Set
#'
#' Utility function used by .onLoad to set a global option if it is not set
#' already.  This is equivalent to conditionally using \code{option} to set
#' options that are not defined yet.
#'
#' @export
#' @param x the name of the option to set
#' @param value the value to set the option to
#' @return the result of calling \code{option} if option is not set, or NULL
#'   invisibly if it is already set

setOptIfNot <- function(x, value) {
  if(!is.chr1(x)) stop("Argument `x` must be chracter(1L) and not NA")
  if(!x %in% names(options())) {
    opt.call <- list(quote(options))
    opt.call[[x]] <- value
    eval(as.call(opt.call), parent.frame())
  } else invisible(NULL)
}


