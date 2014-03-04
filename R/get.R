#' Set and Retrieve Store Contents
#' 
#' These functions are not used directly; rather, they are used by
#' \code{`\link{runtests}`} to get and set the \code{`testor`} objects.
#' You should only need to understand these functions if you are
#' looking to implement a special storage mechanism for the \code{`testor`}
#' objects.
#' 
#' By default, only a character method is defined, which will interpret
#' its inputs as a filesystem path.
#' 
#' You may write your own methods for special storage situations (
#' e.g SQL database, ftp server, etc) with the understanding that the
#' getting method may only accept one argument, the \code{`store.id`}, and
#' the setting method only two arguments, the \code{`store.id`} and the
#' \code{`testor`}.
#' 
#' S3 dispatch will be on \code{`store.id`}, and \code{`store.id`} may
#' be any R object that identifies the testor.  For example, a potential
#' SQL implementation where the testors get stored in blobs may look 
#' like so:
#' \preformatted{
#' my.sql.store.id <- structure(
#'   list(
#'     server="mytestorserver.mydomain.com:3306",
#'     database="testors",
#'     table="project1",
#'     id="cornercasetests"
#'   ),
#'   class="sql_testor" 
#' )
#' get_store.sql_testor <- function(store.id) { # FUNCTION BODY }
#' set_store.sql_testor <- function(store.id, testor) { # FUNCTION BODY }
#' 
#' runtests("testor/cornertestcases.R", my.sql.store.id)
#' } 
#' For inspirations for the bodies of the _store functions look at the source
#' code for \code{`testor:::get_store.character`} and \code{`testor:::set_store.character`}.
#' Expectations for the functions are as follows.  \code{`get_store`} must return:
#' \itemize{
#'   \item a \code{`\link{testor-class}`} object if \code{`store.id`} exists and contains a valid object
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
#' @param testor a \code{`\link{testor-class}`} object containing the store data
#' @return
#'   \itemize{
#'     \item set_store TRUE if testor storing worked, error otherwise
#'     \item get_store a \code{`\link{testor-class}`} object, FALSE
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
  if(inherits(try(testor <- readRDS(store.id)), "try-error")) {
    stop("Failed loading testor; see prior error messages for details")
  }
  if(!is(testor, "testor")) stop("Retrieved object is not a `testor`")
  if(!identical(store.id, testor@id)) {
    if(is.character(testor@id) & length(testor@id) == 1L) {
      warning(
        "ID in retrieved `testor` (", testor@id, ") doesn't match `store.id`; this may ",
        "be happening because you moved the store relative to the script that created it"
    ) } else {
      stop(
        "Logic Error: ID in retrieved `testor` is not a 1 length character vector as expected ",
        "(typeof: ", typeof(testor@id), ", length: ", length(testor@id),"); contact maintainer."
  ) } }  
  testor
}
#' @method get_store default
#' @S3method get_store default

get_store.default <- function(store.id) {
  stop("No method defined for object of class \"", class(store.id)[[1]], "\"")
}
#' @export
#' @rdname get_store

set_store <- function(store.id, testor) {
  UseMethod("set_store")
}
#' @method set_store default
#' @S3method set_store default

set_store.default <- function(store.id, testor) {
  stop("No method defined for object of class \"", class(store.id)[[1]], "\"")
}
#' @method set_store character
#' @S3method set_store character

set_store.character <- function(store.id, testor) {
  if(!is.character(store.id) || length(store.id) != 1L) {
    stop("Argument `store.id` must be a 1 length character vector")
  }
  new.file <- FALSE
  if(!file.exists(store.id)) {
    if(!isTRUE(file.create(store.id))) stop("Could not create `store.id`; make sure it is a valid file name; see warning for details")
    new.file <- TRUE
  }
  if(!is(testor, "testor")) {
    if(new.file) file.remove(store.id)
    stop("Argument `testor` must be a testor")
  } 
  if(inherits(try(saveRDS(testor, store.id)), "try-error")) {
    stop("Failed setting testor; see prior error messages for details.")
  }
  TRUE
}