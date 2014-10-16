#' @include class_unions.R

NULL

#' Class To Track History Changes
#'
#' @keywords internal

setClass(
  "searchHist",
  list(
    name="character",
    type="character",
    mode="character",
    pos="integer",
    extra="environmentOrNULL"
  ),
  prototype=list(extra=NULL),
  validity=function(object) {
    if(length(object@name) != 1L) return("Slot `name` must be one length")
    if(length(object@pos) != 1L) return("Slot `pos` must be one length")
    if(length(object@type) != 1L || ! object@type %in% c("package", "object"))
      return("Slot `type` must be character(1L) and in c(\"package\", \"object\")")
    if(length(object@mode) != 1L || ! object@mode %in% c("add", "remove"))
      return("Slot `mode` must be character(1L) and in c(\"add\", \"remove\")")
  }
)
#' Class To Track History Changes
#'
#' @keywords internal

setClass("searchHistList", contains="unitizerList",
  validity=function(object) {
    if(!all(vapply(object@.items, is, logical(1L), "searchHist")))
      return("slot `.items` may only contain \"searchHist\" objects")
    TRUE
} )

#' Objects used to Track What Environment to Use as Parent to Zero Env
#'
#' Note \code{`"package:unitizer"`} is not actually detached but is included in
#' the list so that it is replaced in the same position on the search path
#'
#' Packages are stored as character values, other objects are stored directly.
#'
#' @aliases zero.env.par objects.attached
#' @keywords internal

pack.env <- new.env()

#' Clears out Global State
#'
#' Should be called by `unitize` each time
#'
#' @return the package environment we use as a global variable
#' @keywords internal

reset_packenv <- function() {
  pack.env$zero.env.par <- new.env(parent=.GlobalEnv)
  pack.env$lib.copy <- base::library
  pack.env$history <- new("searchHistList")
  pack.env$search.init <- character()        # Initial search path b4 any modifications

  pack.env
}
#' Error message shared across functions
#'
#' @keywords internal

unitizer.search.fail.msg <- paste0("We recommend you restart R to restore the search path ",
  "to a clean state.  Please contact maintainer to share this error.  In ",
  "the mean time, you can run `unitizer(clean.search.path=FALSE)` to disable ",
  "search path manipulation."
)

#' Set-up Shims and Other Stuff for Search Path Manip
#'
#' Here we shim by \code{`trace`}ing the \code{`libary/require/attach/detach`}
#' functions and recording each run of those functions that modifies the
#' search path with enough information to restore the search path later.
#'
#' @return logical(1L) TRUE indicates success
#' @keywords internal

search_path_setup <- function() {

  # Make sure no one is already tracing

  fail.shim <- character()
  if(is(base::library, "functionWithTrace")) fail.shim <- c(fail.shim, "library")
  if(is(base::require, "functionWithTrace")) fail.shim <- c(fail.shim, "require")
  if(is(base::attach, "functionWithTrace")) fail.shim <- c(fail.shim, "attach")
  if(is(base::detach, "functionWithTrace")) fail.shim <- c(fail.shim, "detach")

  if(length(fail.shim)) {
    warning("Cannot shim ", paste0(fail.shim, collapse=", "), "because already traced")
    return(FALSE)
  }
  shimmed <- try({
    # Shim library, note we cannot use the `exit` param since `library` uses
    # on.exit

    library.shim <- quote({
      search.pre <- search()
      unitizer.env <- asNamespace("unitizer")$pack.env
      if (!character.only) {
        package <- as.character(substitute(package))
        character.only <- TRUE
      }
      library <- unitizer.env$lib.copy
      res <- library(
        package=package, help=help, pos = pos, lib.loc = lib.loc,
        character.only = character.only, logical.return = logical.return,
        warn.conflicts = warn.conflicts, quietly = quietly,
        verbose = verbose
      )
      # Succeeded in attaching package, so record in history

      if(
        (isTRUE(res) || is.character(res)) &&
        identical(length(search.pre) + 1L, length(search()))
      ) {
        unitizer.env$history <- unitizer::append(
          unitizer.env$history,
          list(new("searchHist", name=package, type="package", mode="add", pos=as.integer(pos), extra=NULL))
        )
      }
      parent.env(unitizer.env$zero.env.par) <- as.environment(2L) # Keep unitizer rooted just below globalenv
      return(res)
    })
    trace(library, library.shim, at=1L, where=.BaseNamespaceEnv, print=FALSE)

    # Shim require

    trace(
      base::require, quote(.unitizer.search.path.init <- search()),
      exit=quote({
        if(identical(length(search()), length(.unitizer.search.path.init) + 1L)) {
          if(is.character(package) && length(package) == 1L) {
            unitizer.env <- asNamespace("unitizer")$pack.env
            unitizer.env$history <- unitizer::append(
              unitizer.env$history,
              list(
                new(
                  "searchHist", name=package, type="package", mode="add",
                  pos=as.integer(pos), extra=NULL
            ) ) )
            parent.env(unitizer.env$zero.env.par) <- as.environment(2L) # Keep unitizer rooted just below globalenv
        } }
      }),
      where=.BaseNamespaceEnv, print=FALSE
    )
    # Shim attach

    trace(
      base::attach, at=1L, tracer=quote(.unitizer.search.path.init <- search()),
      exit=quote({
        if(identical(length(search()), length(.unitizer.search.path.init) + 1L)) {
          if(is.character(name) && length(name) == 1L) {
            unitizer.env <- asNamespace("unitizer")$pack.env
            unitizer.env$history <- unitizer::append(
              unitizer.env$history,
              list(
                new(
                  "searchHist", name=name, type="object", mode="add",
                  pos=as.integer(pos), extra=NULL
            ) ) )
            parent.env(unitizer.env$zero.env.par) <- as.environment(2L) # Keep unitizer rooted just below globalenv
        } }
      }),
      where=.BaseNamespaceEnv, print=FALSE
    )
    # Shim detach

    if(!identical(as.list(body(base::detach))[[3]], quote(packageName <- search()[[pos]])))
      stop("Logic Error: Unable to shim `base:detach`, contact package maintainer.")

    trace(
      base::detach, at=3L, tracer=quote({
        .unitizer.search.path.init <- search()
        if (!missing(name)) {  # snippet lifted directly from `detach`, necessary so we can get object b4 detach
          name.quote <- if (!character.only) substitute(name) else name
          pos <- if (is.numeric(name.quote))
            name.quote
          else {
            if (!is.character(name)) name.quote <- deparse(name)
            match(name.quote, search())
          }
          if (is.na(pos)) stop("invalid 'name' argument")
        }
        .unitizer.package.name <- search()[[pos]]
        .unitizer.obj <- as.environment(.unitizer.package.name)
        .unitizer.type <- if(unitizer:::is.loaded_package(.unitizer.package.name)) "package" else "object"
      }),
      exit=quote({
        if(identical(length(search()), length(.unitizer.search.path.init) - 1L)) {
          if(
            is.numeric(pos) && length(pos) == 1L &&
            pos >= min(seq_along(.unitizer.search.path.init)) &&
            pos <= max(seq_along(.unitizer.search.path.init))
          ) {
            unitizer.env <- asNamespace("unitizer")$pack.env
            unitizer.env$history <- unitizer::append(
              unitizer.env$history,
              list(
                new(
                  "searchHist",
                  name=if(identical(.unitizer.type, "package"))
                    sub("^package:", "", packageName) else packageName,
                  type=.unitizer.type, mode="remove",
                  pos=as.integer(pos), extra=.unitizer.obj
            ) ) )
            parent.env(unitizer.env$zero.env.par) <- as.environment(2L) # Keep unitizer rooted just below globalenv
        } }
      }),
      where=.BaseNamespaceEnv, print=FALSE
    )
  })
  if(inherits(shimmed, "try-error")) {
    warning("Unable to shim all of library/require/attach/detach.")
    search_path_unsetup()
    return(FALSE)
  }
  # Track initial values

  pack.env$search.init <- search()

  return(TRUE)
}
#' Search Path Unsetup
#'
#' Undoes all the shimming we applied
#'
#' @keywords internal

search_path_unsetup <- function() {
  unshim <- try({  # this needs to go
    untrace(library, where=.BaseNamespaceEnv)
    untrace(require, where=.BaseNamespaceEnv)
    untrace(attach, where=.BaseNamespaceEnv)
    untrace(detach, where=.BaseNamespaceEnv)
  })
  if(inherits(unshim, "try-error")) {
    warning(
      "failed trying to unshim library/require/attach/detach, ",
      "which means some of those functions are still modified for search path ",
      "manipulation by `unitizer`.  Restarting R should restore the original ",
      "functions.  Please forward this warning to `unitizer` maintainer."
    )
  }
  invisible()
}
#' Reconstruct Search Path From History
#'
#' This is an internal check to make sure the shims on \code{`library/require/attach/detach`}
#' worked correctly.
#'
#' @param verbose whether to output details of failures, purely for internal debugging
#' @keywords internal

search_path_check <- function(verbose=FALSE) {
  hist <- pack.env$history
  names <- vapply(as.list(hist), slot, "", "name")
  types <- vapply(as.list(hist), slot, "", "type")
  modes <- vapply(as.list(hist), slot, "", "mode")
  poss <- vapply(as.list(hist), slot, 1L, "pos")

  names <- ifelse(types == "package", paste0("package:", names), names)

  search.init <- pack.env$search.init

  for(i in seq_along(hist)) {
    if(modes[[i]] == "add") {
      if(
        (types[[i]] == "package" && !names[[i]] %in% search.init) ||
        types[[i]] == "object"
      ) {
        search.init <- append(search.init, names[[i]], after=poss[[i]] - 1L)
      }
    } else if (modes[[i]] == "remove") {
      if(!identical(search.init[[poss[[i]]]], names[[i]])) {
        if(verbose) message("Object to detach `", names[[i]], "` not at expected position (", poss[[i]], ").")
        return(FALSE)
      }
      search.init <- search.init[-poss[[i]]]
    }
  }
  if(!identical(search(), search.init)) {
    if(verbose) {
      message(
        "Mismatches between expected search path and actual:\n - expected: ",
        deparse(search.init), "\n - actual: ", deparse(search())
      )
    }
    return(FALSE)
  }
  return(TRUE)
}
#' Restore Search Path to Bare Bones R Default
#'
#' \code{`search_path_trimp`} attempts to recreate a clean environment by
#' unloading all packages and objects that are not loaded by default in the
#' default R  configuration. Will fail if a user loaded packages with a
#' \code{`pos`} argument such that they package ends up later in the search list
#' than \code{`package:stats`} (or \code{`tools:rstudio`} if present), basically
#' we're assuming that default load includes everything up to
#' \code{`package:stats`}).  Note this only detaches packages and objects and
#' does not unload namespaces.
#'
#' Note this does not unload namespaces, but rather just detaches them from
#' the namespace
#'
#' @seealso \code{`\link{search_path_restore}`}  \code{`\link{search}`}
#' @keywords internal
#' @return invisibly TRUE on success, FALSE on failure

search_path_trim <- function() {
  # Make sure search path is compatible with what we're doing

  search.path.pre <- search()
  base.path <- c(
    "package:stats", "package:graphics", "package:grDevices", "package:utils",
    "package:datasets", "package:methods", "Autoloads", "package:base"
  )
  if(!identical(base.path, tail(search.path.pre, 8L))) {
    warning(
      "Cannot use a clean search path  as the parent environment for tests because ",
      "the last eight elements in the search path are not: ",
      paste0(base.path, collapse=", "), ".  This may be happening because you ",
      "attached a package at a position before `package:stats` using the `pos` ",
      "argument.  We are using `.GlobalEnv` as the parent environment for our ",
      "tests instead."
    )
    return(invisible(FALSE))
  }
  detach.count <- 8L
  if(
    length(search.path.pre > 9L) &&
    identical(tail(search.path.pre, 9L)[[1L]], "tools:rstudio")
  ) {
    detach.count <- 9L
  } else if (
    "tools:rstudio" %in% search.path.pre
  ) {
    warning(
      "Cannot use a clean search path for tests because `tools:rstudio` is not ",
      "at the expected position in the search list;"
    )
    return(invisible(FALSE))
  }
  packs.to.detach <- tail(head(search.path.pre, -detach.count), -1L)

  # Set-up on exit function to attempt to restore search path in case something
  # went wrong

  on.exit(search_path_restore())

  # detach each object, and record them for purposes of restoring them later

  for(i in seq_along(packs.to.detach)) {
    pack <- packs.to.detach[[i]]
    if(!is.character(pack) || length(pack) != 1L) {
      stop("Logic Error: search path object was not character(1L); contact maintainer")
    }
    if(inherits(try(obj <- as.environment(pack)), "try-error")) {
      stop(
        "Logic Error: unable to convert search path element to environment;",
        " contact maintainer"
      )
    }
    # Is it a package or an object?  Considered package if name starts with
    # "package:" and the package shows up as a namespace env

    is.pack <- is.loaded_package(pack)  # run before detaching

    # Detach all but `unitizer`

    if(!identical(pack, "package:unitizer")) {
      if(inherits(try(detach(pack, character.only=TRUE)), "try-error")) {
        warning(
          "Unable to detach `", pack, "` while attempting to create a clean ",
          "search path.  ", unitizer.search.fail.msg
        )
        return(invisible(FALSE))
  } } }
  # Make sure trimming worked

  if(!search_path_check()) {
    warning(
      "Search path is inconsistent with expectations after we attempted to ",
      "a clean search path.  ", unitizer.search.fail.msg
    )
    return(invisible(FALSE))
  }
  on.exit(NULL)   # clear clean-up b/c we succeeded
  invisible(TRUE)
}
#' Restore Search Path to State Before \code{`search_path_trim`}
#'
#' Undoes \code{`search_path_trim`}
#'
#' @seealso \code{`\link{search_path_trim}`}  \code{`\link{search}`}
#' @keywords internal
#' @return TRUE on success, FALSE on failure, invisibly

search_path_restore <- function() {

  # Make sure everything is as we expect before we actually do anything

  if(!search_path_check()) {
    warning(
      "Unexpected search path, this likely occurred because you ",
      "somehow bypassed in your test code  the shimmed versions of ",
      "`base::library/require/attach/detach` that `unitizer` overloads or ",
      "otherwise modified the search path in an unexpected manner.  We are ",
      "unable to restore the search path to its original form.  ",
      unitizer.search.fail.msg
    )
    return(invisible(FALSE))
  }
  # Step back through history, undoing each step

  for(i in rev(seq_along(pack.env$history))) {
    hist <- pack.env$history[[i]]
    res <- try({
      if(hist@mode == "add") {  # Need to remove
        if(                     # Keep namespace
          hist@type == "object" ||
          (hist@type == "package" && hist@name %in% pack.env$search.init)
        ) {
          detach(pos=hist@pos, character.only=TRUE)
        } else detach(pos=hist@pos, unload=TRUE, character.only=TRUE)
      } else if(hist@mode == "remove") { # Need to add back
        if(hist@type == "package") {
          library(
            hist@name, pos=hist@pos, quietly=TRUE, character.only=TRUE,
            lib.loc=dirname(attr(hist@extra, "path"))
          )
        } else if (hist@type == "object") {
          attach(hist@extra, pos=hist@pos, name=hist@name, warn.conflicts=FALSE)
        }
      }
    })
    if(inherits(res, "try-error")) {
      warning(
        "Failed attempting to restore search path at step ",
        hist@mode, " `", hist@name, "`.  Unable to fully restore search path.  ",
        unitizer.search.fail.msg
      )
      return(invisible(FALSE))
    }
  }
  invisible(TRUE)
}
#' Check Whether a Package Is Loaded
#'
#' A package is considered loaded if it is in the search path and there is a
#' namespace loaded with the same name as the package
#'
#' @keywords internal
#' @param pkg.name character(1L) must be in format "package:pkgname"
#' @return TRUE if it is a loaded package

is.loaded_package <- function(pkg.name) {
  if(!is.character(pkg.name) || length(pkg.name) != 1L)
    stop("Argument `pkg.name` must be character 1L")
  if(!isTRUE(grepl("^package:", pkg.name)))
    stop("Argument `pkg.name` must be in format \"package:<package name>\"")
  just.name <- sub("^package:(.*)", "\\1", pkg.name)
  pkg.name %in% search() && just.name %in% loadedNamespaces()
}
