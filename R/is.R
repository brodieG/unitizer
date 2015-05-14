#' Check Whether File Connection Are Valid
#'
#' Use \code{`is.open_con`} to verify that a connection is open in addition to being
#' valid
#'
#' @keywords internal
#' @aliases is.open_con
#' @param x object to test
#' @param file.name 1 length character the name of the file that \code{`x`} must point to
#' @return TRUE if valid, 1 length character vector if not explaining why it's not

is.valid_con <- function(x, file.name=NULL) {
  if(!is.null(file.name)) {
    if(!is.character(file.name) || length(file.name) != 1L) stop("Argument `file.name` must be NULL or a one length character vector.")
  }
  if(!inherits(x, c("file", "connection"))) return("must inherit from \"file\" and \"connection\"")
  if(!is.integer(x)) return("must be an integer")
  cons <- showConnections()
  if(!isTRUE(as.character(x) %in% rownames(cons))) return("connection does not exist in `showConnections`")
  if(!is.null(file.name)) {
    if(!identical(file.name, cons[as.character(x), "description"])) return("file name does not match connection description")
  }
  return(TRUE)
}
is.open_con <- function(x, file=NULL) {
  if(!isTRUE(msg <- is.valid_con(x, file))) return(msg)
  if(!isOpen(x)) return("must be an open connection")
  return(TRUE)
}
#' Confirm Object is In \code{package_version} form
#' @keywords internal

is.package_version <- function(x)
  inherits(x, "package_version") && inherits(x, "numeric_version") &&
  is.list(x) && identical(length(x), 1L)

#' Test for plain characterness
#'
#' Used primarily for assessing whether a store id should use default mechanism
#' or should be coerced to character
#'
#' @keywords internal

is.chr1plain <- function(x)
  !is.object(x) && is.character(x) && identical(length(x), 1L)


#' Check Whether Provided Store ID Is in Default Form
#'
#' @keywords internal

is.default_unitizer_id <- function(x) is.chr1plain(x) && !is.na(x)
