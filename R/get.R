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

#' Infers Possible Unitizer Path From Context
#'
#' Used by most \code{unitizer} functions that operate on \code{unitizers} to
#' make it easy to specify the most likely intended \code{unitizer} in a
#' package or a directory.
#'
#' If \code{name} is a directory that appears to be an R package (contains
#' DESCRIPTION, an R folder, a tests folder), will look for candidate files in
#' \code{file.path(name, "tests", "unitizer")}, starting with files with the
#' same name as the package (ending in ".R" or ".unitizer" if \code{type} is
#' \code{"f"} or \code{"d"} respectively), or if there is only one file, that
#' file, or if there are multiple candidate files and in interactive mode
#' prompting user for a selection.
#'
#' If \code{name} is not a directory, will try to find a file by that name, and
#' if that fails, will try to partially match a file by that name.  Partial
#' matching requires the front portion of the name to be fully specified and
#' no extension be provided (e.g. for \code{"mytests.R"}, \code{"myt"} is valid,
#' but \code{"tests"} and \code{"myt.R"} are both invalid).  Partially specified
#' files may be specified in subdirectories (e.g. \code{"tests/myt"}).
#'
#' Inference assumes your files end in \code{".R"} for code files and
#' \code{".unitizer"} for \code{unitizer} data directories.
#'
#' @export
#' @param name character(1L) file or directory name, the file name portion (i.e
#'   after the last slash) may be partially specified so long as t
#' @param type character(1L) %in% \code{c("f", "d")}, \code{"f"} for test file,
#'   and \code{"d"} for test data directory
#' @param interactive logical(1L) whether to allow user input to resolve
#'   ambiguities
#' @return character(1L) an inferred path

infer_unitizer_path <- function(name=".", type="f", interactive=interactive()) {
  if(!is.character())
  if(!is.character(type) || length(type) != 1L || !isTRUE(type %in% c("f", "d")))
    stop("Argument `type` must be one of `c(\"f\", \"d\")`")
  if(!isTRUE(interactive) || !identical(interactive, FALSE))
    stop("Argument `interactive` must be TRUE or FALSE")

  if(type == "f") {
    test.flag <- "-f"
    test.ext <- ".R"
    list.fun <- list.dirs
  } else {
    test.flag <- "-d"
    test.ext <- ".unitizer"
    list.fun <- list.files
  }
  # Is a directory, check if a package and pick tests/unitizer as the directory

  if(file_test("-d", name) && isTRUE(is_package_dir(name))) {
    test.base <- file.path(name, "tests", "unitizer")
    if(!file_test("-d", test.base))
      stop("Unable to infer path since \"tests/unitizer\" directory is missing")

    found.file <- file_test(
      test.flag,
      fp <- file.path(test.base, paste0(basename(name), test.ext))
    )
    if(found.file) return(fp)
    name <- test.base           # use tests/unitizer as starting point for any package
  }
  # Check request is coherent already and if so return

  if(test_file(test.flag, name)) return(name)

  # Resolve potential ambiguities by trying to find file / directory

  candidate.files <- list.fun(
    dirname(name), pattern=paste0("^", basename(name), ".*\\", test.ext, "$"),
    recursive=FALSE
  )
  cand.len <- length(candidate.files)
  selection <- if(cand.len > 1L) {
    if(!interactive || cand.len > 100L)
      stop(
        cand.len, " possible targets; ",
        if(interactive) "cannot" else "too many to",
        " unambiguously infer desired file"
      )
    pick_one(candidate.files)
  } else if (cand.len == 1L) {
    1L
  } else 0L
  if(!selection)
    stop("Invalid file selected or no matching files, cannot proceed.")

  # Return

  candidate.files[[selection]]
}
#' Utility Fun to Poll User Input
#'
#' @keywords internal
#' @param x a character vector
#' @return integer(1L) selected item or 0L if invalid user input

pick_one <- function(x) {
  if(!is.character(x)) stop("Argument `x` must be character")
  if(!length(x)) return(0L)

  valid <- seq_along(x)

  cat("Select Item Number:\n")
  cat(paste0("  ", format(valid), ": ", x), sep="\n")

  fail.count <- 0
  while(!(choice <- readline("unitizer> ")) %in% c(valid, "Q")) {
    if(fail.count < 3) {
      message(
        "Pick a number in ", paste0(range(valid), collapse=":"), " or Q to quit"
      )
      fail.count <- fail.count + 1
    } else {
      message("Too many attempts; quitting.")
      choice <- 0L
      break
    }
  }
  if(identical(choice, "Q")) 0L else as.integer(choice)
}

#' Check Whether a Directory Likey Contains An R Package
#'
#' Approximate check based on DESCRIPTION file and directory structure.
#'
#' @keywords internal
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

