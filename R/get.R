#' Set and Retrieve Store Contents
#'
#' These functions are not used directly; rather, they are used by
#' \code{`\link{unitize}`} to get and set the \code{`unitizer`} objects.
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
#' get_unitizer.sql_unitizer <- function(store.id) { # FUNCTION BODY }
#' set_unitizer.sql_unitizer <- function(store.id, unitizer) { # FUNCTION BODY }
#'
#' unitize("unitizer/cornertestcases.R", my.sql.store.id)
#' }
#' For inspirations for the bodies of the _store functions look at the source
#' code for \code{`unitizer:::get_unitizer.character`} and \code{`unitizer:::set_unitizer.character`}.
#' Expectations for the functions are as follows.  \code{`get_unitizer`} must:
#' \itemize{
#'   \item return a \code{`\link{unitizer-class}`} object if \code{`store.id`} exists and contains a valid object
#'   \item return FALSE if the object doesn't exist (e.g. first time run-through, so reference copy doesn't exist yet)
#'   \item \code{`\link{stop}`} on error
#' }
#' \code{`set_unitizer`} must:
#' \itemize{
#'   \item return TRUE on success
#'   \item \code{`\link{stop}`} on error
#' }
#'
#' @aliases get_unitizer
#' @export
#' @param store.id a filesystem path to the store (an .rds file)
#' @param unitizer a \code{`\link{unitizer-class}`} object containing the store data
#' @return
#'   \itemize{
#'     \item set_unitizer TRUE if unitizer storing worked, error otherwise
#'     \item get_unitizer a \code{`\link{unitizer-class}`} object, FALSE
#'       if \code{`store.id`} doesn't exist yet , or error otherwise
#'   }

set_unitizer <- function(store.id, unitizer) {
  UseMethod("set_unitizer")
}
#' @export

set_unitizer.default <- function(store.id, unitizer) {
  stop("No method defined for object of class \"", class(store.id)[[1]], "\"")
}
#' @export

set_unitizer.character <- function(store.id, unitizer) {
  if(!is.character(store.id) || length(store.id) != 1L) {
    stop("Argument `store.id` must be a 1 length character vector")
  }
  if(!is(unitizer, "unitizer")) stop("Argument `unitizer` must be a unitizer")
  new.file <- FALSE
  if(!file.exists(store.id)) {
    if(!isTRUE(dir.create(store.id)))
      stop("Could not create `store.id`; make sure it is a valid file name; see warning for details")
  } else if (!file_test("-d", store.id)) {
    stop("'", store.id, "' is not a directory.")
  }
  if(inherits(try(saveRDS(unitizer, paste0(store.id, "/data.rds"))), "try-error")) {
    stop("Failed setting unitizer; see prior error messages for details.")
  }
  TRUE
}
#' @rdname set_unitizer
#' @export

get_unitizer <- function(store.id) {
  UseMethod("get_unitizer")
}
#' @export

get_unitizer.character <- function(store.id) {
  if(!is.character(store.id) || length(store.id) != 1L ) {
    stop("Argument `store.id` must be a 1 length character vector")
  }
  if(!file.exists(store.id)) return(FALSE)
  if(!file_test("-d", store.id)) stop("Argument `store.id` must refer to a directory")
  if(
    !file.exists(paste0(store.id, "/data.rds")) ||
    !file_test("-f", paste0(store.id, "/data.rds"))
  ) {
    stop(
      "Argument `store.id` does not appear to refer to a directory containing",
      "`unitizer` objects."
    )
  }
  if(inherits(try(unitizer <- readRDS(paste0(store.id, "/data.rds"))), "try-error")) {
    stop("Failed loading unitizer; see prior error messages for details")
  }
  if(!is(unitizer, "unitizer")) stop("Retrieved object is not a `unitizer` store")
  if(!identical(path_clean(store.id), path_clean(unitizer@id))) {
    if(is.character(unitizer@id) & length(unitizer@id) == 1L) {
      # The following warning occurred more often than not as a result of changes
      # in working directory, so just quashing for now; could use `normalizePath`
      # instead...
      # warning(
      #   "ID in retrieved `unitizer` (", unitizer@id, ") doesn't match `store.id`; this may ",
      #   "be happening because you moved the store relative to the script that created it",
      #   immediate. = TRUE
      # )
    } else {
      stop(
        "Logic Error: ID in retrieved `unitizer` is not a 1 length character vector as expected ",
        "(typeof: ", typeof(unitizer@id), ", length: ", length(unitizer@id),"); contact maintainer."
  ) } }
  unitizer
}
#' @export

get_unitizer.default <- function(store.id) {
  stop("No method defined for object of class \"", class(store.id)[[1]], "\"")
}

#' Infers Path From Context
#'
#' If working directory appears to be an R package (contains description, an
#' R folder, a tests folder)
#'
#' Rules:
#' \itemize{
#'   \item if
#' }
#'

infer_path <- function(name=".", type="f") {
  if(file_test("-d", name)) {  # Is a directory, check if a package

  }



}
#' Check Whether a Directory Likey Contains An R Package
#'
#' Approximate check based on DESCRIPTION file and directory structure.
#'
#' @param name a directory to check for package-ness
#' @param has.tests whether to require that the package have tests to qualify
#' @return TRUE if criteria met, character vector explaining first failure
#'   otherwise


is_package_dir <- function(name, has.tests=FALSE) {
  if(!file_test("-d", name)) stop("Argument `name` must be a directory")

  # DESCRIPTION file matches directory?

  if(!file_test("-f", file.path(name, "DESCRIPTION")))
    return("No DESCRIPTION file")
  desc <- try(readLines(file.path(name, "DESCRIPTION")))
  if(inherits(desc, "try-error"))
    return("Unable to read DESCRIPTION file")

  pkg.pat <- "^\\s*package:\\s+(\\S+)\\s*$"
  desc.pkg <- grep(pkg.pat, desc, value=T, perl=T, ignore.case=TRUE)
  if(length(desc.pkg) != 1L)
    return(
      paste0(
        "DESCRIPTION file ",
        if(length(desc.pkg)) "had more than one" else "did not have a",
        " package name entry"
    ) )
  desc.pkg.name <- sub(pkg.pat, "\\1", desc.pkg, perl=T, ignore.case=TRUE)
  dir.name <- if(identical(dirname(name), ".")) name else basename(name)

  if(!identical(tolower(dir.name), tolower(desc.pkg.name)))
    return(
      paste0(
        "DESCRIPTION package name (", desc.pkg.name,
        ") does not match dir name (", dir.name, ")"
    ) )
  # Has requisite directories?

  if(!file_test("-d", file.path(name, "R")))
    return("Missing 'R' directory")
  if(has.tests && !file_test("-d", file.path(name, "tests")))
    return("Missing 'tests' directory")

  # Woohoo

  TRUE
}

