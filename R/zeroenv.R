
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
pack.env$objects.detached <- list()
pack.env$objects.attached <- character()
pack.env$zero.env.par <- .GlobalEnv
pack.env$unitizer.pos <- 0L

#' Restore Search Path to Bare Bones R Default
#' 
#' \code{`search_path_trimp`} attempts to recreate a clean environment by 
#' unloading all packages and objects that are not loaded by default in the 
#' default R  configuration. Will fail if a user loaded packages with a 
#' \code{`pos`} argument such that they package ends up later in the search list 
#' than \code{`package:stats`} (basically we're assuming that default load
#' includes everything up to \code{`package:stats`}).  Note this only detaches
#' packages and objects and does not unload namespaces.
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
      "Cannot use a clean environment as the parent environment for tests because ",
      "the last eight elements in the search path are not: ", 
      paste0(base.path, collapse=", "), ".  This may be happening because you ",
      "attached a package at a position before `package:stats` using the `pos` ",
      "argument.  We are using `.GlobalEnv` as the parent environment for our ",
      "tests instead."
    )
    return(NULL)
  }
  packs.to.detach <- tail(head(search.path.pre, -8L), -1L)
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
    # For `unitizer`, only record the position since we're not actually detaching
    # it, but do need to be able to put it back in the same position in the 
    # search path

    if(!identical(pack, "package:unitizer")) {
      if(inherits(try(detach(pack, character.only=TRUE)), "try-error")) {
        stop("Logic Error: unable to detach `", pack, "`; contact package maintainer.")
      }
      pack.env$objects.detached <- 
        append(pack.env$objects.detached, list(list(name=pack, obj=obj)))
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
  return(NULL)
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
  # re-attach objects

  fail <- character()
  for(i in rev(pack.env$objects.detached)) {
    res <- try(attach(i$obj, name=i$name, warn.conflicts=FALSE))
    if(inherits(res, "try-error")) fail <- append(fail, i$name)
  }
  # re-attach unitizer in correct position

  res <- try({
    unitizer.obj <- as.environment("package:unitizer")
    detach("package:unitizer")
    attach(
      unitizer.obj, pos=pack.env$unitizer.pos, 
      warn.conflicts=FALSE, name="package:unitizer")
  })
  pack.env$objects.detached <- list()

  # Handle errors

  if(inherits(res, "try-error")) fail <- append(fail, i$name)
  if(length(fail)) {
    stop(
      "Logic Error: unable to re-attach the following object", if(length(fail) > 1L) "s", 
      " to search path: ", paste0(fail, collapse=", "), ". ", 
      if(length(fail) > 1L) "These" else "This", " object", 
      if(length(fail) > 1L) "s"," was removed from search path in an attempt to ",
      "create a clean environment to run `unitizer` tests.  Please review prior ",
      "error messages carefully, and consider restarting your R session to restore ",
      "your search path fully.  Also, please forward this error to the  `unitizer` ",
      "package maintainer.",
    )
  }
  NULL
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
    m.c[[1]] <- quote(base::library)
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
