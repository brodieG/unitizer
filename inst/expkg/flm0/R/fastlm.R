#' Calculate Slope, Intercept and Rsq
#'
#' @import stats
#' @export
#' @param x numeric the independent variable
#' @param y numeric the dependent variable
#' @return list with three parameters: slope, intercept, and RSql

fastlm <- function(x, y) {
  if(!is.numeric(x) || !is.numeric(y)) stop("Arguments `x` and `y` must be numeric.")
  if(length(x) != length(y)) stop("Arguments `x` and `y` must be the same length.")

  res <- summary(lm(y ~ x, data.frame(x=x, y=y)))
  intercept <- res$coefficients[1, 1]
  slope <- res$coefficients[2, 1]
  rsq <- res$r.squared

  structure(c(intercept=intercept, slope=slope, rsq=rsq), class="fastlm")
}
#' Retrieve Slope, Intercept, and R Squared
#'
#' @export
#' @aliases get_intercept get_rsq
#' @param x fastlm object
#' @return numeric(1L)

get_slope <- function(x) {
  if(!inherits(x, "fastlm")) stop("Argument `x` must be a fastlm object")
  x[["slope"]]
}
#' @export

get_intercept <- function(x) {
  if(!inherits(x, "fastlm")) stop("Argument `x` must be a fastlm object")
  x[["intercept"]]
}
#' @export

get_rsq <- function(x) {
  if(!inherits(x, "fastlm")) stop("Argument `x` must be a fastlm object")
  x[["rsq"]]
}
# non-exported to test the in_pkg business

hidden_fun <- function() TRUE
