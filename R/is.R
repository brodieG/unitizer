#' Check Whether File Connection Are Valid
#' 
#' Use \code{`is.open_con`} to verify that a connection is open in addition to being
#' valid
#' 
#' @export
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
