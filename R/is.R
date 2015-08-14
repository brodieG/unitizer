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
#' Test for common scalar cases that we run into ALL THE TIME!!!
#'
#' @rdname is.simpleobj
#' @keywords internal
#' @param x object to test

is.chr1plain <- function(x)
  !is.object(x) && is.character(x) && identical(length(x), 1L)

#' @rdname is.simpleobj
#' @keywords internal

is.chr1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

#' @rdname is.simpleobj
#' @keywords internal

is.TF <- function(x) isTRUE(x) || identical(x, FALSE)

#' @rdname is.simpleobj
#' @keywords internal

is.int.pos.2L <- function(x)
  is.numeric(x) && length(x) == 2L && !any(is.na(x)) &&
  all.equal(x, round(x)) && all(x) > 0L

is.int.pos.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !any(is.na(x)) &&
  all.equal(x, round(x)) && all(x) > 0L

is.int.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !any(is.na(x)) && all.equal(x, round(x))

#' Check Whether Provided Store ID Is in Default Form
#'
#' @keywords internal

is.default_unitizer_id <- function(x) is.chr1plain(x) && !is.na(x)

#' Valid State Settings
#'
#' @return a \code{unitizerState} object

is.valid_state <- function(x) {
  if(
    !is(x, "unitizerState") &&
    !(is.chr1(x) && x %in% .unitizer.valid.state.abbr)
  ) {
    word_msg(
      "Argument `x` must be character(1L) %in% ",
      deparse(.unitizer.valid.state.abbr), " or must inherit from S4 class ",
      " `unitizerState`"
    )
    return(FALSE)
  }
  if(is.character(x)) x <- switch(
    x, pristine=new("unitizerStatePristine"), noopt=new("unitizerStateNoOpt"),
    basic=new("unitizerStateBasic"), off=new("unitizerStateOff"),
    safe=new("unitizerStateSafe")
  )
  if(x@options > x@search.path) {
    word_msg(
      "Options state tracking (", x@options, ") must be less than search path ",
      "state tracking (", x@search.path, ").", sep=""
    )
    return(FALSE)
  }
  return(x)
}


