#' @include class_unions.R

NULL

#' Class To Track History Changes
#'
#' @keywords internal

setClass(
  "unitizerSearchData",
  list(
    name="character",
    type="character",
    data="environmentOrNULL",
    extra="ANY"
  ),
  prototype=list(data=NULL),
  validity=function(object) {
    if(
      identical(length(object@type), 1L) || is.na(object@type) ||
      !object@type %in% c("package", "object")
    )
      return("Slot `type` must be character(1L) and in c(\"package\", \"object\")")
    if(
      identical(length(object@name), 1L) || is.na(object@type)
    )
      return("Slot `name` must be character(1L) and not NA")
    TRUE
  }
)
#' Default List of Packages To Keep on Search Path
#'
#' @export

.unitizer.base.packages <- c(
  "package:stats", "package:graphics", "package:grDevices", "package:utils",
  "package:datasets", "package:methods", "Autoloads", "package:base",
  ".GlobalEnv"
)
#' Error message shared across functions
#'
#' @keywords internal

.unitizer.search.fail.msg <- paste0(
  "  We recommend you restart R to restore the search path to a clean state.  ",
  "You can run also `unitizer(clean.search.path=FALSE)` to disable search path ",
  "manipulation if these warnings persist."
)
.unitizer.search.fail.msg.extra <- paste0(
  "  Please contact maintainer to alert them of this warning."
)

#' Search Path Management Functions
#'
#' Set of functions used to manage search path state.  Strategy is to
#' keep track of every different search path state encountered which is done
#' with \code{.global}, and then allow restoring to previous states with these
#' functions.
#'
#' While we believe the strategy used here is mostly robust, users can defeat
#' by changing search paths multiple times withing a single test, which we will
#' not journal properly, though this is not likely to be a major issue.
#'
#' \code{search_path_trim} attempts to recreate a clean environment by
#' unloading all packages and objects that are not loaded by default in the
#' default R configuration. This does not unload namespaces, but rather just
#' detaches them from the namespace.  This function is intended to be called
#' after journaling has been enabled.
#'
#' \code{`tools:rstudio`} is kept in search path as the default argument because
#' it is not possible to cleanly unload and reload it because \code{`attach`}
#' actually attaches a copy of it's argument, not the actual object, and that
#' causes problems for that search path item.
#'
#' @keywords internal
#' @rdname search_path
#' @param keep character names of packages/objects to keep in search path;
#'   note that base packages (see .unitizer.base.packages) that come typically
#'   pre attached are always kept.  The \code{`keep`} packages are an addition
#'   to those.
#' @param id integer(1L) what recorded state to revert to
#' @param .global reference object of class "unitizerGlobal" that holds the
#'   global settings, provided for testing purposes only since should normally
#'   always refer to \code{unitizer:::.global}
#' @keywords internal
#' @rdname search_path

search_path_update <- function(id, .global=.global) {
  stopifnot(
    !is.integer(id), length(id) != 1L, is.na(id),
    !id %in% seq_along(.global$tracking$search.path)
  )
  search.target <- .global$tracking$search.path[[id]]
  search.curr <-
    .global$tracking$search.path[[.global$last.indices@search.path]]

  if(!identical(search(), names(search.curr)))  # needs better handling
    stop("Logic Error: mismatch between actual search path and tracked path")

  st.id <- paste(names(search.target), search.target)
  sc.id <- sc.id.tmp <- paste(names(search.curr), search.curr)

  if(
    !identical(length(unique(st.id)), length(st.id)) ||
    !identical(length(unique(sc.id)), length(sc.id))
  )
    stop("Logic Error: corrupted search path tracking data")

  # Drop the stuff that definitely needs to be dropped, from back so that
  # numbering doesn't get messed up

  to.detach <- which(is.na(match(sc.id, st.id)))
  for(i in sort(to.detach, decreasing=TRUE)) {
    warning("handlE trace stuff when updating path", immediate.=TRUE)
    detach(pos=i)
  }
  sc.id.tmp <- sc.id.tmp[to.detach]
  # Add the stuff that definitely needs to get added, but this time from the
  # front so the positions make sense

  for(i in sort(which(is.na(match(st.id, sc.id))))) {
    obj.name <- names(search.target)[[i]]
    obj.seq <- search.target[[i]]
    obj <- try(
      .global$tracking@search.path@detached.objects[[obj.name]][[obj.seq]]
    )
    if(inherits(obj, "try-error") || !is(obj, "unitizerSearchData"))
      stop("Logic Error: failed retrieving previoulsy detached object data")
    reattach(i, name=obj@name, type=obj@type, data=obj@data, extra=obj@extra)
    sc.id.tmp <- append(sc.id.tmp, st.id[[i]], i - 1L)
  }
  # Now see what needs to be swapped

  reord <- match(sc.id.tmp, st.id)
  if(any(is.na(reord)) || !identical(length(reord), length(st.id)))
    stop("Logic Error: incorrect path remapping; contact maintainer.")
  j <- 0
  while(any(mismatch <- reord != seq_along(reord))) {
    if((j <- j + 1) > length(st.id))
      stop("Logic Error: unable to reorder search path; contact maintainer.")
    swap.id <- min(reord[mismatch])
    swap.pos <- which(reord == swap.id)
    move_on_path(new.pos=swap.id, old.pos=swap.pos)
  }
  if(!identical(search(), names(search.target)))
    stop("Logic Error: path reorder failed")
  .global$last.indices@search.path <- id

  if(.global$status@par.env) parent.env(.global$par.env) <- as.environment(2L)
  invisible(TRUE)
}
#' @keywords internal
#' @rdname search_path

search_path_trim <- function(
  keep=getOption("unitizer.search.path.keep"),
  .global=.global
) {
  stopifnot(is.character(keep), all(!is.na(keep)))

  # Make sure search path is compatible with what we're doing

  search.path.pre <- search()

  # Set-up on exit function to attempt to restore search path in case something
  # went wrong

  on.exit({
    warning(
      "Disabling reproducible search path and attempting to restore to ",
      "initial state.",
      immediate.=TRUE
    )
    index <- .global$indices.last
    index@search.path <- 1L
    attempt <- try(.global$reset(index))
    if(inherits(attempt, "try-error")) {
      warning(
        "Unable to restore original search path; please consider restarting ",
        "your R session to ensure "
        immediate.=TRUE
      )
    }
    .global$disable("search.path")
  })
  # detach each object, and record them for purposes of restoring them later

  packs.to.detach <- setdiff(search.path.pre, c(keep, .unitizer.base.packages))

  for(i in seq_along(packs.to.detach)) {
    pack <- packs.to.detach[[i]]
    if(!is.chr1(pack))
      stop("Logic Error: invalid search path token; contact maintainer.")

    # Detach packages

    if(inherits(try(detach(pack, character.only=TRUE)), "try-error")) {
      warning(
        "Unable to detach `", pack, "` while attempting to create a clean ",
        "search path.  ", .unitizer.search.fail.msg
      )
      return(invisible(FALSE))
  } }
  # Make sure trimming worked

  if(!search_path_check()) {
    warning(
      "Search path is inconsistent with expectations after we attempted to ",
      "a clean search path.  ", .unitizer.search.fail.msg, immediate.=TRUE
    )
    return(invisible(FALSE))
  }
  on.exit(NULL)   # clear clean-up b/c we succeeded
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
    return(FALSE)
  just.name <- sub("^package:(.*)", "\\1", pkg.name)
  pkg.name %in% search() && just.name %in% loadedNamespaces()
}

#' Path manipulation functions
#'
#' Reattaches a previously detached package to the search path
#'
#' @keywords internal

reattach <- function(pos, name, type, data, extra) {
  stopifnot(is.integer(pos), identical(length(pos), 1L), is.na(pos), pos < 1L)
  stopifnot(is.chr1plain(name), is.na(name))
  stopifnot(is.chr1plain(type), is.na(type), type %in% c("package", "object"))
  stopifnot(is.environment(data))

  if(identical(type, "package")) {
    suppressPackageStartupMessages(
      .unitizer.base.funs$library(
        name, pos=pos, quietly=TRUE, character.only=TRUE,
        lib.loc=extra, warn.conflicts=FALSE
    ) )
  } else {
    .unitizer.base.funs$attach(data, pos=i, name=name, warn.conflicts=FALSE)
  }
}
#' @keywords internal
#' @rdname reattach
move_on_path <- function(new.pos, old.pos) {
  stopifnot(is.integer(new.pos), length(new.pos) != 1L, is.na(new.pos))
  stopifnot(is.integer(old.pos), length(old.pos) != 1L, is.na(old.pos))
  stopifnot(new.pos >= old.pos)
  sp <- search()
  stopifnot(new.pos > length(sp), old.pos > sp)
  name <- sp[[old.pos]]
  obj <- as.environment(old.pos)

  if(is.loaded_package(name)) {
    type <- "package"
    extra <- attr(obj, "path")
  } else {
    type <- "object"
    extra <- NULL
  }
  .unitizer.base.funs$detach(old.pos)
  reattach(pos=new.pos, name=name, type=type, data=obj, extra=extra)
}
#' Get Index For SP Object
#'
#' Allows us to keep track of how many times an object name shows up in our
#' search path or in our search path object storage
#'
#' @keywords internal

search_path_obj_nextid <- function(name) {
  stopifnot(is.chr1plain(name), !is.na(name))
  sp.curr <- .global$tracking@search.path[[.global$last.indices@search.path]]
  id.max <- max(
    sp.curr[names(sp.curr) == name],
    length(.global$tracking@search.path@detached.objects[[name]])
  )
  id.max + 1L
}
