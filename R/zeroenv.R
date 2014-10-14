#' @include class_unions.R

NULL

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
pack.env$zero.env.par <- .GlobalEnv
pack.env$unitizer.pos <- 0L
pack.env$base.packs <- character()
pack.env$search <- character()
pack.env$lib.copy <- base::library
pack.env$history <- list()
pack.env$search.init <- character()        # Initial search path b4 any modifications
pack.env$search.base <- character()        # "Clean" search path

#' Class To Track History Changes

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
#' Shim Functions
#' 
#' @keywords internal 

shim_funs <- function() {
  
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
        list(name=package, type="package", mode="add", pos=pos, extra=NULL)
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
            list(name=package, type="package", mode="add", pos=pos, extra=NULL)
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
            list(name=name, type="object", mode="add", extra=NULL)
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
            list(
              name=packageName, type="object", mode="remove",
              pos=pos, extra=.unitizer.obj
        ) ) } }
    })
  )
}
#' Reconstruct Search Path From History
#' 
#' This is an internal check to make sure the shims on \code{`library/require/attach/detach`}
#' worked correctly.  Should probably use an object to do this, but a bit annoying 
#' to implement.
#' 
#' @keywords internal 

search_path_check <- function() {
  hist <- rev(pack.env$history)
  names <- vapply(hist, `[[`, "", "name")
  types <- vapply(hist, `[[`, "", "type")
  modes <- vapply(hist, `[[`, "", "mode")
  poss <- vapply(hist, `[[`, "", "pos")

  names <-

  search.init <- pack.env$search.init

  for(i in seq_along(hist)) {
    if(modes[[i]] == "add") {
      if(
        (types[[i]] == "package" && !names[[i]] %in% search.init) || 
        types[[i]] == "object"
      ) {
        search.init <- append(
          search.init, 
          if(types[[i]] == "package") paste0("package:", names[[i]]) else names[[i]]
          after=pos - 1L
        )
      } 
    } else if (modes[[i]] == "remove") {
      if(search.init[[poss[[i]]]] != names[[i]])

    } else stop("Logic Error: incorrect search tracking object structure; contact maintainer.")
    
  }


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

  on.exit({
    search.path.exit <- search()
    search.path.comp <- search.path.pre
    if(length(search.path.exit) < length(search.path.pre))
      search.path.comp <- head(search.path.pre, length(search.path.exit))
    if(!identical(search.path.exit, search.path.comp))
      stop(
        "Logic Error: unexpected search path status, failed attempting to unload ",
        "packages to create a clean environment for a unitizer run, and due to ",
        "unexpected status we cannot restore it to original status (sorry); we ",
        "recommend you restart your R session and reload the packages you want ",
        "on the search path, and contact package maintainer to let him know ",
        "of this error."
      )
    search_path_reset()
  })

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
  # remove added objects, but only unload namespace if they 

  if(!identical(pack.env$search, search())) {
    # We can manage this so long as there are no conflicts, i.e. someone doesn't
    # attach the same object five times, leaving us unable to know which one is
    # the one that matters

    stop(
      "Logic Error: unexpected search path, this likely occurred because you ",
      "used `base::library/require/attach/detach`, or your package functions are ",
      "using those functions.  `unitizer` overloads `library/require/attach/detach`, ",
      "but the overload is ineffective if you use these functions from the `base` ",
      "namespace directly.  We are unable to restore the search path to its original ",
      "form."
    )
  }

  for(i in rev(seq_along(pack.env$objects.attached))) {
    # Only detach if not in first 8/9 elements
    # Only unload namespace if not in removed

    curr.obj <- rev(names(pack.env$objects.attached))[[i]]
    

    pack.env$objects.attached


  }
  # re-attach objects; note different strategy depending on whether it is a
  # package or not; also, just a reminder that namespaces should still be
  # loaded

  fail <- character()
  for(i in rev(pack.env$objects.detached)) {
    if(i$is.pack) {
      pck.name.clean <- sub("package:(.*)", "\\1", i$name)
      res <- try(
        library(
          pck.name.clean, warn.conflicts=FALSE, 
          lib.loc=dirname(i$path), character.only=TRUE
      ) )
    } else {
      res <- try(attach(i$obj, name=i$name, warn.conflicts=FALSE))
    }
    if(inherits(res, "try-error")) fail <- append(fail, i$name)
  }
  pack.env$objects.detached <- list()

  # re-attach unitizer in correct position

  if(pack.env$unitizer.pos && is.loaded_package("package:unitizer")) {
    res <- try({
      lib.loc <- dirname(attr(unitizer.obj, "path"))
      detach("package:unitizer")
      library(
        "unitizer", pos=pack.env$unitizer.pos, 
        warn.conflicts=FALSE, lib.loc=lib.loc, character.only=TRUE
      )
    })    
    if(inherits(res, "try-error")) fail <- append(fail, "package:unitizer")
  }
  # Handle errors

  if(length(fail)) {
    stop(
      "Logic Error: unable to re-attach the following object", if(length(fail) > 1L) "s", 
      " to search path: ", paste0(fail, collapse=", "), ". ", 
      if(length(fail) > 1L) "These" else "This", " object", 
      if(length(fail) > 1L) "s were" else " was", " removed from search path in ",
      "an attempt to create a clean environment to run `unitizer` tests.  Please ",
      "review prior error messages carefully, and consider restarting your R ",
      "session to restore your search path fully.  Also, please forward this ",
      "error to the  `unitizer` package maintainer."
    )
  }
  invisible(NULL)
}
#' Special Unitizer Overloaded \code{`library`} and \code{`require`} Funs
#' 
#' \code{`make_req_lib`} is a generator function since these share almost the
#' same code except for the function to \code{`match.call`} against.
#' 
#' @keywords internal
#' @aliases unitizer_library unitizer_require
#' @param definition should be one of \code{`library`}

make_req_lib <- function(definition) {
  if(
    !identical(definition, quote(base::library)) &&
    !identical(definition, quote(base::require))
  )
    stop(
      "Logic Error: you may only use this function with library/require as ",
      "argument; contact maintainer"
    )
  
  function(...) {
    has.pack <- FALSE
    m.c <- match.call(definition)  # <---- NOTE: definition is pulled from enclosing frame
     
    # Reconstitute call into a character version of the call

    as.char <- FALSE
    if("character.only" %in% names(m.c)) {
      if(!inherits(try(as.logical(m.c$character.only), silent=TRUE), "try-error")) {
        as.char <- as.logical(m.c$character.only)[[1]]
    } }
    # Extract character version of package name

    if("package" %in% names(m.c)) {
      if(as.char) {
        pck.name <- try(eval(m.c$package, parent.frame()))
        if(inherits(pck.name, "try-error")) {
          stop(
            "Logic Error: failed attempting to get character name for use with ",
            "library/require; please contact `unitizer` package maintainer"
        ) }
      } else {
        pck.name <- as.character(m.c$package)
      }
      m.c$package <- pck.name
      m.c$character.only <- TRUE
    }
    m.c[[1]] <- definition
    search.pre <- search()
    lib.res <- eval(m.c, parent.frame())

    if(length(new.pack <- setdiff(search(), search.pre))) {
      if(length(new.pack) > 1L)
        stop("Logic Error: seem to have loaded more than one package; contact maintainer.")

      # Package loaded, register; if package loaded more than once in unitizer
      # code, this will just over-write existing entry; note order is important
      # as we will unload them in reverse order to try to avoid issues

      if(isTRUE(lib.res) || is.character(lib.res)) {  
        pck.name.full <- paste("package", pck.name, sep=":")
        if(
          pck.name.full %in% names(pack.env$objects.attached) && 
          !identical(pack.env$objects.attached[[pck.name.full]], "package")
        )
          stop(
            "Non-package with same name as package \"", pck.name.full, 
            "\" detected.  If there are no non-package objects on the search ",
            "path with this name, contact `unitizer` maintainer."
          )
        pack.env$objects.attached[[pck.name.full]] <- "package"
    } }
    lib.res
  }
}
unitizer_library <- make_req_lib(quote(base::library))
unitizer_require <- make_req_lib(quote(base::require))

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


#' Functions To Overload For Package Management
#' 
#' List will be loaded into our zero env

unitizer_lib_funs <- list(
  library=NULL,
  require=NULL,
  attach=NULL,
  detach=NULL
)
