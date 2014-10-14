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
    if(!length(object@name) != 1L) return("Slot `name` must be one length")
    if(!length(object@pos) != 1L) return("Slot `pos` must be one length")
    if(!length(object@type) != 1L || ! object@type %in% c("package", "object")) 
      return("Slot `type` must be character(1L) and in c(\"package\", \"object\")")
    if(!length(object@mode) != 1L || ! object@type %in% c("add", "remove")) 
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
pack.env$zero.env.par <- new.env(parent=.GlobalEnv)
pack.env$unitizer.pos <- 0L
pack.env$base.packs <- character()
pack.env$search <- character()
pack.env$lib.copy <- base::library
pack.env$history <- new("searchHistList")
pack.env$search.init <- character()        # Initial search path b4 any modifications
pack.env$search.base <- character()        # "Clean" search path

#' Set-up Shims and Other Stuff for Search Path Manip
#' 
#' Here we shim by \code{`trace`}ing the \code{`libary/require/attach/detach`}
#' functions and recording each run of those functions that modifies the 
#' search path with enough information to restore the search path later.
#' 
#' @keywords internal 

search_path_setup <- function() {
  
  # Track initial values

  pack.env$search.init <- search()

  # Shim library

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
      isTRUE(res) || is.character(res) && 
      identical(length(search.pre), length(search()) + 1L)
    ) {
      unitizer.env$history <- append(
        unitizer.env$history, 
        new("searchHist", name=package, type="package", mode="add", pos=pos, extra=NULL)
      )
    }
    return(res)
  })
  trace(base::library, library.shim, at=1L)

  # Shim require

  trace(
    base::require, quote(.unitizer.search.path.init=search()), 
    exit=quote({
      if(identical(length(search()), length(.unitizer.search.path.init) + 1L)) {
        if(is.character(package) && length(package) == 1L) {
          unitizer.env <- asNamespace("unitizer")$pack.env        
          unitizer.env$history <- append(
            unitizer.env$history, 
            new("searchHist", name=package, type="package", mode="add", pos=pos, extra=NULL)
      ) } }
    })
  )
  # Shim attach
  
  trace(
    base::attach, at=1L, tracer=quote(.unitizer.search.path.init=search()), 
    exit=quote({
      if(identical(length(search()), length(.unitizer.search.path.init) + 1L)) {
        if(is.character(name) && length(name) == 1L) {
          unitizer.env <- asNamespace("unitizer")$pack.env        
          unitizer.env$history <- append(
            unitizer.env$history, 
            new("searchHist", name=name, type="object", mode="add", extra=NULL)
      ) } }
    })
  )
  # Shim detach
  
  if(!identical(as.list(body(base:detach)[[3]]), quote(packageName <- search()[[pos]]))
    stop("Logic Error: Unable to shim `base:detach`, contact package maintainer.")

  trace(
    base::detach, at=3L, tracer=quote({
      .unitizer.search.path.init=search()
      .unitizer.obj <- as.environment(packageName)
      .unitizer.type <- is.loaded_package(packageName)
    }), 
    exit=quote({
      if(identical(length(search()), length(.unitizer.search.path.init) - 1L)) {
        if(
          is.numeric(pos) && length(pos) == 1L && 
          pos >= min(seq_along(.unitizer.search.path.init)) && 
          pos <= max(seq_along(.unitizer.search.path.init))
        ) {
          unitizer.env <- asNamespace("unitizer")$pack.env        
          unitizer.env$history <- append(
            unitizer.env$history, 
            new("searchHist", name=packageName, 
              type=.unitizer.type, mode="remove",
              pos=pos, extra=.unitizer.obj
        ) ) } }
    })
  )
}
#' Reconstruct Search Path From History
#' 
#' This is an internal check to make sure the shims on \code{`library/require/attach/detach`}
#' worked correctly.
#' 
#' @param verbose whether to output details of failures, purely for internal debugging
#' @keywords internal 

search_path_check <- function(verbose=FALSE) {
  hist <- rev(pack.env$history)
  names <- vapply(as.list(hist), slot, "", "name")
  types <- vapply(as.list(hist), slot, "", "type")
  modes <- vapply(as.list(hist), slot, "", "mode")
  poss <- vapply(as.list(hist), slot, "", "pos")

  names <- ifelse(types == "package", paste0("package:", names), names

  search.init <- pack.env$search.init

  for(i in seq_along(hist)) {
    if(modes[[i]] == "add") {
      if(
        (types[[i]] == "package" && !names[[i]] %in% search.init) || 
        types[[i]] == "object"
      ) {
        search.init <- append(search.init, names[[i]], after=pos - 1L)
      } 
    } else if (modes[[i]] == "remove") {
      if(!identical(search.init[[poss[[i]]]], names[[i]])) {
        if(vebose) message("Object to detach `", names[[i]], "` not at expected position (", poss[[i]], ").")
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
#' @return NULL, this function is run purely for side effects and generates and
#'   calls \code{`stop`} on failure

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
    invisible(NULL)
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
    invisible(NULL)
  }
  packs.to.detach <- tail(head(search.path.pre, -detach.count), -1L)
  pack.env$base.packs <- head(search.path.pre, detach.count)
  if(length(pack.env$objects.detached))
    stop("Logic Error: there should not be any detached packages yet; contact maintainer")

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

    # For `unitizer`, only record the position since we're not actually detaching
    # it, but do need to be able to put it back in the same position in the 
    # search path

    if(!identical(pack, "package:unitizer")) {
      if(inherits(try(detach(pack, character.only=TRUE)), "try-error")) 
        stop("Logic Error: unable to detach `", pack, "`; contact package maintainer.")
    } else {
      pack.env$unitizer.pos <- i + 1L
    } 
  }
  # Find and return parent environment for tests

  search.path.post <- search()
  if(length(search.path.post) < 9L)
    stop("Logic Error: post-trim search path is less than 9 items long; contact maintainer.")
  zero.env.par.tmp <- try(as.environment(search.path.post[[2L]]))
  if(inherits(zero.env.par.tmp, "try-error"))
    stop(
      "Logic Error: targeted zero env parent cannot be converted to environment; ",
      "contact maintainer."
    )
  pack.env$zero.env.par <- zero.env.par.tmp
  on.exit(NULL)  # clear clean-up b/c we succeeded
  invisible(NULL)
}

#' Restore Search Path to State Before \code{`search_path_trim`}
#' 
#' Undoes \code{`search_path_trim`}
#' 
#' @seealso \code{`\link{search_path_trim}`}  \code{`\link{search}`}
#' @keywords internal
#' @return NULL, this function is run purely for side effects and generates and
#'   calls \code{`stop`} on failure

search_path_restore <- function() {
  
  # Make sure everything is as we expect before we actually do anything

  fail.msg <- paste0("We recommend you restart R to restore the search path ",
    "to a clean state.  Please contact maintainer to share this error.  In ",
    "the mean time, you can run `unitizer(clean.search.path=FALSE)` to disable ",
    "search path manipulation."
  )
  unshim <- try({  # this needs to go 
    untrace(library, where=.BaseNamespaceEnv)
    untrace(require, where=.BaseNamespaceEnv)
    untrace(attach, where=.BaseNamespaceEnv)
    untrace(detach, where=.BaseNamespaceEnv)
  })
  if(inherits(unshim, "try-error")) {
    stop(
      "Logic Error: failed trying to unshim library/require/attach/detach, ",
      "which means some of those functions are still modified for search path ",
      "manipulation by `unitizer`.  Additionally, `unitizer` will not be able ",
      "to restore the search path.  ", fail.msg
    )
  }
  if(!search_path_check()) {
    stop(
      "Logic Error: unexpected search path, this likely occurred because you ",
      "somehow bypassed in your test code  the shimmed versions of ",
      "`base::library/require/attach/detach` that `unitizer` overloads or ",
      "otherwise modified the search path in an unexpected manner.  We are ",
      "unable to restore the search path to its original form.  ", fail.msg
    )
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
          detach(pos=hist@pos)
        } else detach(pos=hist@pos, unload=TRUE)
      } else if(hist@mode == "remove") { # Need to add back
        if(hist@type == "package") {
          library(
            sub("^package:", "", hist@name), pos=hist@pos, quietly=TRUE, 
            lib.loc=dirname(attr(hist@extra, "path"))
          )
        } else if (hist@type == "object") {
          attach(hist@extra, pos=hist@pos, name=hist@name, warn.conflicts=FALSE)
        }
      }
    })
    if(inherits(res, "try-error")) {
      stop(
        "Logic Error: failed attempting to restore search path at step ", 
        hist@mode, "`", hist@name, "`.  ", length(pack.env$history) - i + 1L, 
        " items in search path where not restored.  ", fail.msg
      )
    }
  }
  invisible(NULL)
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