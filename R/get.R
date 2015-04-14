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
#' Used by most \code{unitizer} functions that operate on \code{unitizer}s to
#' make it easy to specify the most likely intended \code{unitizer} in a
#' package or a directory.
#'
#' This is implemented as an S3 generic to allow third parties to define
#' inference methods for other types of \code{store.id}, but the documentation
#' here is for the \code{"character"} method which is what \code{unitizer} uses
#' by default.  Note a \code{NULL} as \code{store.id} dispatches
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
#' @seealso \code{\link{get_unitizer}} for discussion of alternate
#'   \code{store.id} objects
#' @param store.id character(1L) file or directory name, the file name portion (i.e
#'   after the last slash) may be partially specified
#' @param type character(1L) in \code{c("f", "d")}, \code{"f"} for test file,
#'   and \code{"d"} for test data directory
#' @param interactive.mode logical(1L) whether to allow user input to resolve
#'   ambiguities
#' @param ... arguments to pass on to other methods
#' @return character(1L) an inferred path, or \code{store.id} with a warning if
#'   path cannot be inferred

infer_unitizer_location <- function(store.id, ...)
  UseMethod("infer_unitizer_location")

#' @rdname infer_unitizer_location
#' @export

infer_unitizer_location.default <- function(store.id, ...) {
  if(missing(store.id)) return(infer_unitizer_location.character(".", ...))
  store.id
}

#' @rdname infer_unitizer_location
#' @export

infer_unitizer_location.character <- function(
  store.id, type="f", interactive.mode=interactive(), ...
) {
  if(!is.character(store.id) || length(store.id) != 1L || is.na(store.id))
    stop("Argument `store.id` must be character(1L) and not NA")
  if(!is.character(type) || length(type) != 1L || !isTRUE(type %in% c("f", "d")))
    stop("Argument `type` must be one of `c(\"f\", \"d\")`")
  if(!isTRUE(interactive.mode) && !identical(interactive.mode, FALSE))
    stop("Argument `interactive.mode` must be TRUE or FALSE")

  if(type == "f") {
    test.fun <- function(x) file_test("-f", x)
    test.ext <- ".R"
    list.fun <- list.files
  } else {
    test.fun <- is_unitizer_dir
    test.ext <- ".unitizer"
    list.fun <- list.dirs
  }
  # Check for exact match first and return that if found

  if(test.fun(store.id)) return(store.id)

  # Is a directory, check if a package and pick tests/unitizer as the directory

  if(!file_test("-d", store.id)) {
    dir.store.id <- dirname(store.id)
    file.store.id <- basename(store.id)
  } else {
    dir.store.id <- store.id
    file.store.id <- ""
  }
  dir.store.id <- normalizePath(dir.store.id)

  if(file_test("-d", dir.store.id) && isTRUE(is_package_dir(dir.store.id))) {
    test.base <- file.path(dir.store.id, "tests", "unitizer")
    if(!file_test("-d", test.base))
      stop("Unable to infer path since \"tests/unitizer\" directory is missing")

    found.file <- test.fun(
      fp <- file.path(
        test.base, paste0(file.store.id, basename(dir.store.id), test.ext)
    ) )
    if(found.file) return(fp)
    dir.store.id.proc <- test.base           # use tests/unitizer as starting point for any package
  } else {
    dir.store.id.proc <- dir.store.id
  }
  # Check request is coherent already and if so return

  f.path <- file.path(dir.store.id.proc, file.store.id)
  if(test.fun(f.path)) return(f.path)

  # Resolve potential ambiguities by trying to find file / directory

  candidate.files <- grep(
    paste0("^", file.store.id, ".*\\", test.ext, "$"),
    basename(list.fun(dir.store.id.proc, recursive=FALSE)),
    value=TRUE
  )
  cand.len <- length(candidate.files)
  selection <- if(cand.len > 1L) {
    if(!interactive.mode || cand.len > 100L) {
      warning(
        cand.len, " possible targets; ",
        if(interactive.mode) "cannot" else "too many to",
        " unambiguously infer desired file",
        immediate.=TRUE
      )
      return(store.id)
    }
    dir.disp <- if(!identical(dir.store.id, dir.store.id.proc)) {
      paste0(
        " from \"",
        sub(
          paste0("^", normalizePath(dir.store.id), "/?"), "",
          normalizePath(dir.store.id.proc)
        ),
        "\""
      )
    } else ""

    pick_one(candidate.files, paste0("Select file", dir.disp, ":\n"))
  } else if (cand.len == 1L) {
    1L
  } else if (cand.len == 0L) {
    warning("No possible matching files", immediate.=TRUE)
    return(store.id)
  }
  if(!selection && interactive.mode) {
    warning("Invalid user selection", immediate.=TRUE)
    return(store.id)
  } else if(!selection)
    stop(
      "Logic Error: should never have non.interative zero selection; ", "
      contact maintainer."
    )
  # Return

  file.path(dir.store.id.proc, candidate.files[[selection]])
}
#' Utility Fun to Poll User Input
#'
#' @keywords internal
#' @param x a character vector
#' @return integer(1L) selected item or 0L if invalid user input

pick_one <- function(x, prompt="Select Item Number:\n") {
  if(!is.character(x)) stop("Argument `x` must be character")
  if(!length(x)) return(0L)

  valid <- seq_along(x)

  cat(prompt)
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
#' Check Whether a Directory as a Unitizer Data Directory
#'
#' Just checks that it \emph{could} be a data directory, the test ultimately is
#' to attempt a \code{\link{get_unitizer}} call and see if we actually resurrect
#' a working \code{unitizer}
#'
#' @keywords internal
#' @param dir character(1L) directory to check
#' @return logical(1L)

is_unitizer_dir <- function(dir)
  is.character(dir) && length(dir) == 1L && !is.na(dir) &&
  file_test("-d", dir) && file_test("-f", file.path(dir, "data.rds"))

#' Preload Objects Generated By Files Into An Environment
#'
#' @keywords internal
#' @param dir.name the directory of files to load
#' @param env.par the parent of the environment to source the files into
#' @return an environment containing the objects created by the sourcing of the
#'   files in \code{dir.name}

pre_load <- function(dir.name, env.par=.GlobalEnv) {
  if(!is.character(dir.name) || length(dir.name) != 1L || is.na(dir.name))
    stop("Argument `dir.name` must be character(1L) and not NA")
  if(file.exists(dir.name) && !file_test("-d", dir.name))
    stop("Argument `dir.name` must be a directory")
  if(!is.environment(env.par)) stop("Argument `env.par` must be an environment")

  env <- new.env(parent=env.par)
  file.list <- dir(dir.name)

  source_many(file.list, env)
}
#' Run \code{sys.source} on a list of files
#'
#' @keywords internal
#' @param files character()
#' @param env an environment
#' @return an environment

source_many <- function(files, env) {
  if(!is.character(files)) stop("Argument `files` must be character")
  if(!is.environment(env)) stop("Argument `env` must be an environment")

  file.norm <- try(normalizePath(files, must.work=TRUE))
  if(inherits(file.norm, "try-error"))
    stop("Unable to normalize file paths; see previous error")

  if(inherits(try(for(i in files) sys.source(i, env)), "try-error"))
    stop(
      "Error sourcing file `", i, ", see above for details"
    )
  env
}
