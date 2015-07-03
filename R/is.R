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

#' Check Whether Provided Store ID Is in Default Form
#'
#' @keywords internal

is.default_unitizer_id <- function(x) is.chr1plain(x) && !is.na(x)

#' Check for reproducible state variable
#'
#' @keywords internal

is.valid_rep_state <- function(x) {
  if (
      (
        is.integer(x) || (
          is.numeric(x) &&
          identical(as.integer(x), x)
        )
      ) &&
      is.character(names(x)) &&
      all(names(x) %in% .unitizer.global.settings.names) &&
      all(as.integer(x) %in% 0:2)
  ) {
    return(TRUE)
  } else {
    word_msg(
      "Argument `reproducible.state` must be FALSE, or ",
      "integer with values in 0:2 and names in ",
      deparse(.unitizer.global.settings.names, width=500L)
    )
    return(FALSE)
  }
  if(
    identical(x[["options"]], 2L) &&
    !identical(x[["search.path"]], 2L)
  ) {
    word_msg(
      "Argument `reproducible.state` has an invalid state: 'options' is set ",
      "to 2, but 'search.path' is not"
    )
    return(FALSE)
  }
  if(identical(x[["random.seed"]], 2L)) {
    prev.seed <- mget(
      ".Random.seed", envir=.GlobalEnv, ifnotfound=list(NULL)
    )[[1L]]
    seed.dat <- getOption("unitizer.seed")
    if(inherits(try(do.call(set.seed, seed.dat)), "try-error")) {
      word_msg(
        "Unable to set random seed; make sure `getOption('unitizer.seed')` ",
        "is a list of possible arguments to `set.seed`."
      )
      return(FALSE)
    }
    if(is.null(prev.seed) && exists(".Random.seed", envir=.GlobalEnv))
      rm(".Random.seed", envir=.GlobalEnv) else
        assign(".Random.seed", prev.seed, envir=.GlobalEnv)
  }
  TRUE
}


