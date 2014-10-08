
#' Objects used to Track What Environment to Use as Parent to Zero Env
#' 
#' Note \code{`"package:unitizer"`} is not actually detached but is included in
#' the list so that it is replaced in the same position on the search path
#' 
#' Packages are stored as character values, other objects are stored directly.
#' 
#' @aliases zero.env.par objects.attached
#' @keywords internal

objects.detached <- list()
objects.attached <- list()
zero.env.par <- .GlobalEnv

#' Functions To Manage Search Path
#' 
#' \code{`search_path_trimp`} attempts to recreate a clean environment by 
#' unloading all packages and objects that are not loaded by default in the 
#' default R  configuration. Will fail if a user loaded packages with a 
#' \code{`pos`} argument such that they package ends up later in the search list 
#' than \code{`package:stats`} (basically we're assuming that default load
#' includes everything up to \code{`package:stats`}).  Note this only detaches
#' packages and objects and does not unload namespaces.
#' 
#' \code{`search_path_reset`} attempts to restore the search list to what it
#' was previously.
#' 
#' @seealso \code{`\link{search}`}
#' @return environment to use as parent environment for \code{`search_path_trim`},
#'   NULL for \code{`search_path_reset`}

search_path_trim <- function() {
  # Make sure search path is compatible with what we're doing

  search.path <- search()
  base.path <- c(
    "package:stats", "package:graphics", "package:grDevices", "package:utils",  
    "package:datasets", "package:methods", "Autoloads", "package:base")
  )
  if(!identical(base.path, tail(search.path, 8L))) {
    warning(
      "Cannot use a clean environment as the parent environment for tests because ",
      "the last eight elements in the search path are not: ", 
      paste0(base.path, collapse=", "), "; using `.GlobalEnv` instead."
    )
    return(zero.env.par)
  }
  packs.to.detach <- tail(head(base.path, -8L), -1L)
  if(length(objects.detached))
    stop("Logic Error: there should not be any detached packages yet; contact maintainer")

  on.exit({stop("need to add search_path_reset here");})

  # detach each object, and record them for purposes of restoring them later

  for(i in seq_along(packs.to.detach)) {
    pack <- pack.to.detach[[i]]
    if(!is.character(pack) || length(pack) != 1L) {
      stop("Logic Error: search path object was not character(1L); contact maintainer")
    }
    if(inherits(try(obj <- as.environment(pack)), "try-error")) {
      stop(
        "Logic Error: unable to convert search path element to environment;",
        " contact maintainer"
      )
    }
    if(!identical(pack, "package:unitizer")) {
      if(inherits(try(detach(obj)), "try-error")) {
        stop("Logic Error: unable to detach `", obj, "`; contact package maintainer".)
      }      
    }
    objects.detached[[i]] <<- list(name=pack, obj=obj)
  }
  # Find and return parent environment for tests

  search.path <- search()
  if(length(search.path) < 9L)
    stop("Logic Error: post-trim search path is less than 9 items long; contact maintainer.")
  zero.env.par.tmp <- try(as.environment(search.path[[2L]]))
  if(inherits(zero.env.par.tmp, "try-error"))
    stop("Logic Error: targeted zero env parent cannot be converted to environment; contact maintainer.")
  zero.env.par <<- zero.env.par.tmp
  zero.env.par
}
search_path_reset <- function() {
  for(i in rev(objects.attached)) {
    stop("NEED TO ADD CODE HERE")
  }

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
    !identical(definition, base::library) &&
    !identical(definition, base::require)
  )
    stop("Logic Error: you may only use this function with library/require as argument; contact maintainer")
  
  function(...) {
    has.pack <- FALSE
    m.c <- match.call(definition)  # <---- NOTE: definition is pulled from enclosing frame
     
    # Reconstitute call into a character version of the call

    if("character.only" %in% names(m.c)) {
      if(!inherits(try(as.logical(m.c$character.only), silent=TRUE), "try-error")) {
        as.char <- as.logical(m.c$character.only)[[1]]
      } else {
        as.char <- FALSE
      }      
    }
    # Extract character version of package name

    if("package" %in% names(m.c)) {
      if(as.char) {
        pck.name <- try(eval(m.c$package, parent.frame()))
        if(inherits(pck.name, "try-error")) {
          stop(
            "Logic Error: failed attempting to get character name for use with "
            "library/require; please contact `unitizer` package maintainer"
        ) }
      } else {
        pck.name <- as.character(m.c$package)
      }
      m.c$package <- pck.name
      m.c$character.only <- TRUE
      has.pack <- TRUE
    }
    m.c[[1]] <- quote(base::library)
    lib.res <- eval(m.c, parent.frame())

    if(has.pack) {
      # Package loaded, register; if package loaded more than once in unitizer
      # code, this will just over-write existing entry; note order is important
      # as we will unload them in reverse order to try to avoid issues

      if(isTRUE(lib.res) || is.character(lib.res)) {  
        pck.name.full <- paste("package", pck.name, sep=":")
        if(
          pck.name.full %in% names(objects.attached) && 
          !identical(objects.attached[[pck.name.full]], "package")
        )
          stop(
            "Non-package with same name as package \"", pck.name.full, 
            "\" detected.  If there are no non-package objects on the search "
            "path with this name, contact `unitizer` maintainer."
          )
        objects.attached[[pck.name.full]] <<- "package"
    } }
    lib.res
  }
}
unitizer_library <- make_req_lib(library)
unitizer_require <- make_req_lib(require)

#' Functions To Overload For Package Management
#' 
#' List will be loaded into our zero env

unitizer_lib_funs <- list(
  library=NULL,
  require=NULL,
  attach=NULL,
  detach=NULL
)
