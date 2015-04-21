#' Retrieve Unitizer
#'
#' If this errors, calling function should abort
#'
#' @keywords internal
#' @param store.id anything for which there is a defined \code{`\link{get_unitizer}`}
#'   method; by default should be the path to a unitizer; if \code{`\link{get_unitizer}`}
#'   returns \code{`FALSE`} then this will create a new unitizer
#' @param par.frame the environment to use as the parent frame for the \code{`unitizer`}
#' @param test.file the R file associated with the store id
#' @return a \code{`unitizer`} object, or anything, in which case the calling
#'   code should exit

load_unitizer <- function(store.id, par.frame, test.file) {

  if(inherits(try(unitizer <- get_unitizer(store.id)), "try-error")) {
    stop(
      "Unable to retrieve/create `unitizer` at location ", store.id,
      "; see prior errors for details."
  ) }
  # Retrieve or create unitizer environment (note that the search path trimming)
  # happens later.  Also note that pack.env$zero.env can still be tracking the
  # top package under .GlobalEnv

  if(identical(unitizer, FALSE)) {
    unitizer <- new("unitizer", id=store.id, zero.env=new.env(parent=par.frame))
  } else if(!is(unitizer, "unitizer")) {
    if(!identical(class(store.id), "character"))
      stop("Logic Error: `get_unitizer.", class(store.id)[[1]], "` did not return a unitizer")
    stop("Logic Error: `get_unitizer` did not return a unitizer; contact maintainer.")
  } else {
    parent.env(unitizer@zero.env) <- par.frame
    ver <- unitizer@version
    unitizer <- upgrade(unitizer, par.frame=par.frame)
    if(!identical(ver, unitizer@version)) { # there was an upgrade, so store new file
      success <- try(set_unitizer(store.id, unitizer))
      if(inherits(success, "try-error"))  {
        stop(
          "Logic Error: failed attempting to store upgraded `unitizer`; contact ",
          " maintainer."
        )
      }
      message("Unitizer store updated to version ", unitizer@version)
  } }
  unitizer@test.file.loc <- test.file
  unitizer
}
#' @keywords internal
#' @rdname load_unitizer

store_unitizer <- function(unitizer, store.id, wd) {
  if(!is(unitizer, "unitizer") || is.null(store.id)) return(invisible(TRUE))

  if(!identical((new.wd <- getwd()), wd)) setwd(wd)  # Need to do this in case user code changed wd
  success <- try(set_unitizer(store.id, unitizer))
  setwd(new.wd)
  if(!inherits(success, "try-error")) {
    message("unitizer updated")
  } else {
    stop("Error attempting to save `unitizer`, see previous messages.")
  }
  return(invisible(TRUE))
}
