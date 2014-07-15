#' Set and Retrieve Store Contents
#' 
#' These functions are not used directly; rather, they are used by
#' \code{`\link{runtests}`} to get and set the \code{`unitizer`} objects.
#' You should only need to understand these functions if you are
#' looking to implement a special storage mechanism for the \code{`unitizer`}
#' objects.
#' 
#' By default, only a character method is defined, which will interpret
#' its inputs as a filesystem path.
#' 
#' You may write your own methods for special storage situations (
#' e.g SQL database, ftp server, etc) with the understanding that the
#' getting method may only accept one argument, the \code{`store.id`}, and
#' the setting method only two arguments, the \code{`store.id`} and the
#' \code{`unitizer`}.
#' 
#' S3 dispatch will be on \code{`store.id`}, and \code{`store.id`} may
#' be any R object that identifies the unitizer.  For example, a potential
#' SQL implementation where the unitizers get stored in blobs may look 
#' like so:
#' \preformatted{
#' my.sql.store.id <- structure(
#'   list(
#'     server="myunitizerserver.mydomain.com:3306",
#'     database="unitizers",
#'     table="project1",
#'     id="cornercasetests"
#'   ),
#'   class="sql_unitizer" 
#' )
#' get_store.sql_unitizer <- function(store.id) { # FUNCTION BODY }
#' set_store.sql_unitizer <- function(store.id, unitizer) { # FUNCTION BODY }
#' 
#' runtests("unitizer/cornertestcases.R", my.sql.store.id)
#' } 
#' For inspirations for the bodies of the _store functions look at the source
#' code for \code{`unitizer:::get_store.character`} and \code{`unitizer:::set_store.character`}.
#' Expectations for the functions are as follows.  \code{`get_store`} must return:
#' \itemize{
#'   \item a \code{`\link{unitizer-class}`} object if \code{`store.id`} exists and contains a valid object
#'   \item FALSE if the object doesn't exist (e.g. first time run-through, so reference copy doesn't exist yet)
#'   \item \code{`\link{stop}`} on error
#' }
#' \code{`set_store`} must return:
#' \itemize{
#'   \item TRUE on success
#'   \item \code{`\link{stop}`} on error
#' }
#' 
#' @aliases set_store
#' @export
#' @param store.id a filesystem path to the store (an .rds file)
#' @param unitizer a \code{`\link{unitizer-class}`} object containing the store data
#' @return
#'   \itemize{
#'     \item set_store TRUE if unitizer storing worked, error otherwise
#'     \item get_store a \code{`\link{unitizer-class}`} object, FALSE
#'       if \code{`store.id`} doesn't exist yet , or error otherwise
#'   }

get_store <- function(store.id) {
  UseMethod("get_store")
}
#' @method get_store character
#' @S3method get_store character

get_store.character <- function(store.id) {
  if(!is.character(store.id) || length(store.id) != 1L ) {
    stop("Argument `store.id` must be a 1 length character vector")
  }
  if(file_test("-d", store.id)) stop("Argument `store.id` refers to a directory instead of a file.")
  if(!file_test("-f", store.id)) return(FALSE)
  if(inherits(try(unitizer <- readRDS(store.id)), "try-error")) {
    stop("Failed loading unitizer; see prior error messages for details")
  }
  if(!is(unitizer, "unitizer")) stop("Retrieved object is not a `unitizer`")
  if(!identical(store.id, unitizer@id)) {
    if(is.character(unitizer@id) & length(unitizer@id) == 1L) {
      warning(
        "ID in retrieved `unitizer` (", unitizer@id, ") doesn't match `store.id`; this may ",
        "be happening because you moved the store relative to the script that created it"
    ) } else {
      stop(
        "Logic Error: ID in retrieved `unitizer` is not a 1 length character vector as expected ",
        "(typeof: ", typeof(unitizer@id), ", length: ", length(unitizer@id),"); contact maintainer."
  ) } }  
  unitizer
}
#' @method get_store default
#' @S3method get_store default

get_store.default <- function(store.id) {
  stop("No method defined for object of class \"", class(store.id)[[1]], "\"")
}
#' @export
#' @rdname get_store

set_store <- function(store.id, unitizer) {
  UseMethod("set_store")
}
#' @method set_store default
#' @S3method set_store default

set_store.default <- function(store.id, unitizer) {
  stop("No method defined for object of class \"", class(store.id)[[1]], "\"")
}
#' @method set_store character
#' @S3method set_store character

set_store.character <- function(store.id, unitizer) {
  if(!is.character(store.id) || length(store.id) != 1L) {
    stop("Argument `store.id` must be a 1 length character vector")
  }
  new.file <- FALSE
  if(!file.exists(store.id)) {
    if(!isTRUE(file.create(store.id))) stop("Could not create `store.id`; make sure it is a valid file name; see warning for details")
    new.file <- TRUE
  }
  if(!is(unitizer, "unitizer")) {
    if(new.file) file.remove(store.id)
    stop("Argument `unitizer` must be a unitizer")
  } 
  if(inherits(try(saveRDS(unitizer, store.id)), "try-error")) {
    stop("Failed setting unitizer; see prior error messages for details.")
  }
  TRUE
}