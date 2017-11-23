#' Set and Retrieve Store Contents
#'
#' These functions are not used directly; rather, they are used by
#' \code{\link{unitize}} to get and set the \code{unitizer} objects.
#' You should only need to understand these functions if you are
#' looking to implement a special storage mechanism for the \code{unitizer}
#' objects.
#'
#' By default, only a character method is defined, which will interpret
#' its inputs as a filesystem path.
#'
#' You may write your own methods for special storage situations (
#' e.g SQL database, ftp server, etc) with the understanding that the
#' getting method may only accept one argument, the \code{store.id}, and
#' the setting method only two arguments, the \code{store.id} and the
#' \code{unitizer}.
#'
#' S3 dispatch will be on \code{store.id}, and \code{store.id} may
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
#' Make sure you also define an \code{as.character} method for your object to
#' produce a human readable identifying string.
#'
#' For inspirations for the bodies of the _store functions look at the source
#' code for \code{unitizer:::get_unitizer.character} and
#' \code{unitizer:::set_unitizer.character}.
#' Expectations for the functions are as follows.  \code{get_unitizer} must:
#' \itemize{
#'   \item return a \code{unitizer-class} object if \code{store.id}
#'      exists and contains a valid object
#'   \item return FALSE if the object doesn't exist (e.g. first time
#'     run-through, so reference copy doesn't exist yet)
#'   \item \code{\link{stop}} on error
#' }
#' \code{set_unitizer} must:
#' \itemize{
#'   \item return TRUE on success
#'   \item \code{\link{stop}} on error
#' }
#'
#' @aliases get_unitizer
#' @export
#' @param store.id a filesystem path to the store (an .rds file)
#' @param unitizer a \code{unitizer-class} object containing the store
#'   data
#' @return
#'   \itemize{
#'     \item set_unitizer TRUE if unitizer storing worked, error otherwise
#'     \item get_unitizer a \code{unitizer-class} object, FALSE
#'       if \code{store.id} doesn't exist yet, or error otherwise; note that
#'       the \code{unitizer_results} method returns a list
#'   }

set_unitizer <- function(store.id, unitizer) {
  UseMethod("set_unitizer")
}
#' @export

set_unitizer.default <- function(store.id, unitizer) {
  stop(
    "No method defined for object of class \"", class(store.id)[[1]], "\"; ",
    "make sure that the specified `store.id` is a reference to a valid ",
    "unitizer store and had defined `get_unitizer` and `set_unitizer` methods."
  )
}
#' @export

set_unitizer.character <- function(store.id, unitizer) {
  if(!is.character(store.id) || length(store.id) != 1L) {
    stop("Argument `store.id` must be a 1 length character vector")
  }
  if(is.object(store.id) || !is.null(attributes(store.id)))
    stop("Argument `store.id` must be a bare character vector")
  if(!is(unitizer, "unitizer")) stop("Argument `unitizer` must be a unitizer")
  new.file <- FALSE
  if(!file.exists(store.id)) {
    if(!isTRUE(dir.create(store.id)))
      stop(
        "Could not create `store.id`; make sure it is a valid file name; see ",
        "warning for details"
      )
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
#' @rdname set_unitizer
#' @export

get_unitizer.character <- function(store.id) {
  if(!is.character(store.id) || length(store.id) != 1L ) {
    stop("Argument `store.id` must be a 1 length character vector")
  }
  if(is.object(store.id) || !is.null(attributes(store.id)))
    stop("Argument `store.id` must be a bare character vector")
  if(!file.exists(store.id)) return(FALSE)

  if(!is_unitizer_dir(store.id)) {
    stop(
      "Argument `store.id` does not appear to refer to a unitizer directory"
    )
  }
  if(inherits(try(unitizer <- readRDS(paste0(store.id, "/data.rds"))), "try-error")) {
    stop("Failed loading unitizer; see prior error messages for details")
  }
  if(!is(unitizer, "unitizer")) stop("Retrieved object is not a unitizer store")
  # if(!identical(path_clean(store.id), path_clean(unitizer@id))) {
  #   stop(
  #     "This check needs to be modified to not make any assumptions about ",
  #     "unitizer structure since we don't know it is conforming yet"
  #   )
  #   if(is.character(unitizer@id) & length(unitizer@id) == 1L) {
  #     # The following warning occurred more often than not as a result of changes
  #     # in working directory, so just quashing for now; could use `normalizePath`
  #     # instead...
  #     # warning(
  #     #   "ID in retrieved unitizer (", unitizer@id, ") doesn't match `store.id`; this may ",
  #     #   "be happening because you moved the store relative to the script that created it",
  #     #   immediate. = TRUE
  #     # )
  #   } else {
  #     stop(
  #       "Internal Error: ID in retrieved unitizer is not a 1 length character vector as expected ",
  #       "(typeof: ", typeof(unitizer@id), ", length: ", length(unitizer@id),"); contact maintainer."
  #   )
  # } }
  unitizer
}
#' @rdname set_unitizer
#' @export

get_unitizer.default <- function(store.id) {
  stop(
    "No method defined for object of class \"", class(store.id)[[1]], "\"; ",
    "make sure that the specified `store.id` is a reference to a valid ",
    "unitizer store and had defined `get_unitizer` and `set_unitizer` methods."
  )
}
#' @rdname set_unitizer
#' @export

get_unitizer.unitizer_result <- function(store.id) {
  store.id <- attr(store.id, "store.id")
  get_unitizer(store.id)
}
# used purely for testing, but has to be exported
#' @export

get_unitizer.unitizer_error_store <- function(store.id)
  structure("error", class="unitizer_store_error")

#' @rdname set_unitizer
#' @export

get_unitizer.unitizer_results <- function(store.id) {
  lapply(store.id, get_unitizer)
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
#' by default.
#'
#' If \code{store.id} is a directory that appears to be an R package (contains
#' DESCRIPTION, an R folder, a tests folder), will look for candidate files in
#' \code{file.path(store.id, "tests", "unitizer")}, starting with files with the
#' same name as the package (ending in ".R" or ".unitizer" if \code{type} is
#' \code{"f"} or \code{"u"} respectively), or if there is only one file, that
#' file, or if there are multiple candidate files and in interactive mode
#' prompting user for a selection.  If \code{type} is \code{"d"}, then will
#' just provide the \code{"tests/unitizer"} directory.
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
#' If \code{store.id} is NULL, the default \code{infer_unitizer_location} method
#' will attempt to find the top level package directory and then call the
#' character method with that directory as \code{store.id}.  If the parent
#' package directory cannot be found, then the character method is called with
#' the current directory as the argument.
#'
#' @export
#' @seealso \code{\link{get_unitizer}} for discussion of alternate
#'   \code{store.id} objects
#' @param store.id character(1L) file or directory name, the file name portion
#'   (i.e after the last slash) may be partially specified
#' @param type character(1L) in \code{c("f", "u", "d")}, \code{"f"} for test
#'   file, \code{"d"} for a directory, \code{"u"} for a \code{unitizer}
#'   directory
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
  if(is.null(store.id)) {
    def.dir <- if(
      length(pkg.dir <- get_package_dir(".")) &&
      file_test("-d", file.path(pkg.dir, "tests", "unitizer"))
    ) {
      file.path(pkg.dir, "tests", "unitizer")
    } else "."
    return(infer_unitizer_location.character(def.dir, ...))
  }
  store.id
}

#' @rdname infer_unitizer_location
#' @export

infer_unitizer_location.character <- function(
  store.id, type="f", interactive.mode=interactive(), ...
) {
  if(!is.character(store.id) || length(store.id) != 1L || is.na(store.id))
    stop("Argument `store.id` must be character(1L) and not NA")
  if(
    !is.character(type) || length(type) != 1L ||
    !isTRUE(type %in% c("f", "u", "d"))
  )
    stop("Argument `type` must be one of `c(\"f\", \"u\", \"d\")`")
  if(!isTRUE(interactive.mode) && !identical(interactive.mode, FALSE))
    stop("Argument `interactive.mode` must be TRUE or FALSE")

  # BEWARE, you can't just change `text.ext` here without reviewing how it is
  # used

  if(type == "f") {
    test.fun <- function(x) file_test("-f", x)
    test.ext <- ".R"
    list.fun <- list.files
    type.name <- "test file"
  } else if(type == "u") {
    test.fun <- is_unitizer_dir
    test.ext <- ".unitizer"
    list.fun <- list.dirs
    type.name <- "unitizer"
  } else if(type == "d") {
    test.fun <- function(x) file_test("-d", x)
    test.ext <- NULL
    list.fun <- list.dirs
    type.name <- "test directory"
  }
  inf_msg <- function(name)
    meta_word_msg(
      "Inferred", type.name, "location:", relativize_path(name), sep=" "
    )

  # Is a directory, check if a package and pick tests/unitizer as the directory

  if(!file_test("-d", store.id)) {
    dir.store.id <- dirname(store.id)
    file.store.id <- basename(store.id)
  } else {
    dir.store.id <- store.id
    file.store.id <- NULL
  }
  dir.store.id <- normalize_path(dir.store.id)
  at.package.dir <-
    file_test("-d", dir.store.id) && isTRUE(is_package_dir(dir.store.id))

  # Check for exact match first and return that if found, unless we are in dir
  # mode and the directory is a package directory

  if(!(identical(type, "d") && at.package.dir) && test.fun(store.id))
    return(store.id)

  # If we're not at the package directory, check to see if we are in a package
  # directory and change directory to that, but only do that if we're not
  # already matching an actual directory

  if(
    !at.package.dir && !is.null(file.store.id) &&
    length(pkg.dir.tmp <- get_package_dir(dir.store.id))
  ) {
    at.package.dir <- TRUE
    dir.store.id <- pkg.dir.tmp
  }
  if(at.package.dir) {
    test.base <- file.path(dir.store.id, "tests", "unitizer")
    if(!file_test("-d", test.base))
      stop("Unable to infer path since \"tests/unitizer\" directory is missing")

    found.file <- test.fun(
      fp <- do.call(
        file.path,
        as.list(
          c(test.base, paste0(file.store.id, basename(dir.store.id), test.ext))
    ) ) )
    if(found.file) {
      inf_msg(fp)
      return(fp)
    }
    # use tests/unitizer as starting point for any package
    dir.store.id.proc <- test.base
  } else {
    dir.store.id.proc <- dir.store.id
  }
  # Check request is coherent already and if so return

  f.path <- do.call(file.path, as.list(c(dir.store.id.proc, file.store.id)))
  if(test.fun(f.path)) {
    inf_msg(f.path)
    return(f.path)
  }
  # Resolve potential ambiguities by trying to find file / directory

  candidate.files <- grep(
    paste0("^", file.store.id, if(!is.null(test.ext)) ".*\\", test.ext, "$"),
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
          paste0("^", normalize_path(dir.store.id), "/?"), "",
          normalize_path(dir.store.id.proc)
        ),
        "\""
      )
    } else ""
    # Select one:

    valid <- seq_along(candidate.files)
    cat(paste0("Possible matching files", dir.disp, ":\n"))
    cat(paste0("  ", format(valid), ": ", candidate.files), sep="\n")

    pick <- unitizer_prompt(
      "Pick a matching file",
      valid.opts=c("Type a number"),
      exit.condition=exit_fun, valid.vals=valid,
      hist.con=NULL, global=NULL, browse.env=.GlobalEnv
    )
    if(identical(pick, "Q")) {
      message("No file selected")
      0L
    } else {
      pick <- as.integer(pick)
      message("Selected file: ", candidate.files[[pick]])
      pick
    }
  } else if (cand.len == 1L) {
    1L
  } else if (cand.len == 0L) {
    warning("No possible matching files for '", store.id, "'", immediate.=TRUE)
    return(store.id)
  }
  if(!selection && interactive.mode) {
    warning("Invalid user selection", immediate.=TRUE)
    return(store.id)
  } else if(!selection) {
    # nocov start
    stop(
      "Internal Error: should never have non.interactive zero selection; ", "
      contact maintainer."
    )
    # nocov end
  }
  # Return

  file.final <- file.path(dir.store.id.proc, candidate.files[[selection]])
  if(cand.len == 1L) inf_msg(file.final)
  file.final
}
# Check Whether Directories Are Likely R Package Source Directories
#
# Heuristic check to see whether a directory contains what likely could be
# built into an R package.  This is based on the DESCRIPTION file and directory
# structure.
#
# \code{is_package_dir} checks whether a directory is the top level directory
# of a package.
#
# \code{get_package_dir} checks whether a directory or any of its parents is the
# top level directory of a package, and returns the top level package directory
# or character(0L) if not
#
# @keywords internal
# @param name a directory to check for package-ness
# @param has.tests whether to require that the package have tests to qualify
# @param DESCRIPTION the DESCRIPTION file path
# @return TRUE if criteria met, character vector explaining first failure
#   otherwise

is_package_dir <- function(name, has.tests=FALSE) {
  stopifnot(is.character(name), is.TF(has.tests))
  if(!is.character(name)) return("not character so cannot be a directory")
  if(!file_test("-d", name)) return("not an existing directory")
  pkg.name <- try(get_package_name(name), silent=TRUE)
  if(inherits(pkg.name, "try-error"))
    return(conditionMessage(attr(pkg.name, "condition")))

  # Has requisite directories?

  if(!file_test("-d", file.path(name, "R")))
    return("missing 'R' directory")
  if(has.tests && !file_test("-d", file.path(name, "tests")))
    return("missing 'tests' directory")

  # Woohoo

  TRUE
}
get_package_dir <- function(name=getwd(), has.tests=FALSE) {
  stopifnot(
    is.character(name), !any(is.na(name)), is.TF(has.tests),
    as.logical(length(name))
  )
  name <- normalize_path(name, mustWork=FALSE)
  if(length(name) > 1L) name <- attr(unique_path(name), "common_dir")
  is.package <- FALSE
  prev.dir <- par.dir <- name

  repeat {
    if(isTRUE(is_package_dir(par.dir, has.tests))) {
      return(par.dir)
    } else if (isTRUE(is_rcmdcheck_dir(par.dir, has.tests))) {
      return(get_rcmdcheck_dir(par.dir, has.tests))
    } else if (isTRUE(is_testinstpkg_dir(par.dir, has.tests))) {
      return(get_testinstpkg_dir(par.dir, has.tests))
    }
    if(nchar(par.dir <- dirname(prev.dir)) >= nchar(prev.dir)) break
    prev.dir <- par.dir
  }
  character(0L)
}
# Checks Whether a Directory Could be of the Type Used by R CMD check

is_rcmdcheck_dir <- function(name, has.tests=FALSE) {
  stopifnot(is.chr1(name), is.TF(has.tests))
  dir <- basename(name)
  if(grepl(".*\\.Rcheck$", dir)) {
    pkg.name <- sub("(.*)\\.Rcheck", "\\1", dir)
    if(identical(pkg.name, dir))
      stop(
        "Logic error; failed extracting package name from Rcheck dir; ",
        "contact maintianer"
      )
    if(isTRUE(is.pd <- is_package_dir(file.path(name, pkg.name), has.tests))) {
      return(TRUE)
    } else return(is.pd)
  } else return("not a .Rcheck directory")
}
# Checks Whether a Directory could be of the type generated by
# tools::testInstalledPackages

is_testinstpkg_dir <- function(name, has.tests=FALSE) {
  stopifnot(is.chr1(name), is.TF(has.tests))
  dir <- basename(name)
  if(grepl(".*-tests$", dir)) {
    pkg.name <- sub("(.*)-tests$", "\\1", dir)
    if(identical(pkg.name, dir))
      stop(
        "Internal error; failed extracting package name from ",
        "testInstalledPackages dir; contact maintainer."
      )
    if(
      isTRUE(
        is.pd <- is_package_dir(file.path(dirname(name), pkg.name), has.tests)
      )
    ) {
      return(TRUE)
    } else return(is.pd)
  }
}
# Extracts the Source Directory from an R CMD check directory

get_rcmdcheck_dir <- function(name, has.tests=FALSE) {
  stopifnot(is.chr1(name), is.TF(has.tests))
  if(isTRUE(chk.dir <- is_rcmdcheck_dir(name, has.tests))) {
    pkg.name <- sub("(.*)\\.Rcheck", "\\1", basename(name))
    return(file.path(name, pkg.name))
  } else stop("Internal Error: not an R CMD check dir") # nocov
}
# Extracts the Source Directory from an testInstalledPackage directory, looks
# like this will be a bit of a bear to test, see the code in covr that uses
# tools::testInstalledPackage for some potential things to worry about

get_testinstpkg_dir <- function(name, has.tests=FALSE) {
  stopifnot(is.chr1(name), is.TF(has.tests))
  if(isTRUE(chk.dir <- is_testinstpkg_dir(name, has.tests))) {
    pkg.name <- sub("(.*)-tests$", "\\1", basename(name))
    return(file.path(dirname(name), pkg.name))
  } else stop("Internal Error: not an testInstalledPackage dir") # nocov
}
# Pulls Out Package Name from DESCRIPTION File
#
# Dir must be a package directory, check with is_package_dir first

get_package_name <- function(pkg.dir) {
  stopifnot(is.chr1(pkg.dir))

  DESCRIPTION <- file.path(pkg.dir, "DESCRIPTION")
  if(!file_test("-f", DESCRIPTION)) stop("No DESCRIPTION file")
  desc <- try(readLines(DESCRIPTION))
  if(inherits(desc, "try-error")) stop("Unable to read DESCRIPTION file")

  pkg.pat <- "^\\s*package:\\s+(\\S+)\\s*$"
  desc.pkg <- grep(pkg.pat, desc, value=T, perl=T, ignore.case=TRUE)
  if(length(desc.pkg) != 1L)
    stop(
      "DESCRIPTION file ",
      if(length(desc.pkg)) "had more than one" else "did not have a",
      " package name entry"
    )
  desc.pkg.name <- sub(pkg.pat, "\\1", desc.pkg, perl=T, ignore.case=TRUE)
  return(desc.pkg.name)
}
# Check Whether a Directory as a Unitizer Data Directory
#
# Just checks that it \emph{could} be a data directory, the test ultimately is
# to attempt a \code{\link{get_unitizer}} call and see if we actually resurrect
# a working \code{unitizer}
#
# @keywords internal
# @param dir character(1L) directory to check
# @return logical(1L)

is_unitizer_dir <- function(dir)
  is.character(dir) && length(dir) == 1L && !is.na(dir) &&
  file_test("-d", dir) && file_test("-f", file.path(dir, "data.rds"))

# Run \code{sys.source} On All Provided Files
#
# Sorts them before sourcing on the paths as specified in \code{files}, returns
# an environment containing any objects created by the code in the source
# files.  If a file is a directory, the files in that directory are sourced,
# though only top level files in that directory are sourced.
#
# @keywords internal
# @param files character() pointing to files or directories
# @param env an environment
# @return environment, or a character message explaining why it failed

source_files <- function(files, env.par, pattern="\\.[rR]$") {
  stopifnot(
    is.character(files),
    is.chr1(pattern),
    !inherits(try(grepl(pattern, "a"), silent=TRUE), "try-error"),
    is.environment(env.par)
  )
  file.norm <- try(normalize_path(files, mustWork=TRUE))
  if(inherits(file.norm, "try-error"))
    return("Unable to normalize file paths; see previous error")

  env <- new.env(parent=env.par)

  for(i in sort(file.norm)) {
    sub.files <- if(file_test("-d", i)) {
      dir.cont <- try(
        list.files(
          i, pattern=pattern, all.files=TRUE, full.names=TRUE, no..=TRUE
      ) )
      if(inherits(dir.cont, "try-error"))
        return("Unable to list file contents of directory; see previous error")
      dir.cont
    } else i
    for(j in sub.files) {
      fail <- inherits(try(sys.source(j, env)), "try-error")
      if(fail)
        return(paste0("Error sourcing file `", j, "`, see above for details"))
    }
  }
  env
}
