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
#' Set of functions used to track and set search path state.  Strategy is to
#' keep track of every different search path state encountered, which is done by
#' shimming the library (and require by extension), detach, and attach
#' functions (see shim.R, global.R).
#'
#' Any time any of those functions is called, \code{search_path_track} updates
#' the journaling information kept in \code{unitizer:::.global} and makes sure
#' the parent to the \code{unitizer} environment is the one right below
#' \code{.GlobalEnv}.  Additionally, any time \code{detach} is called, data
#' about the detached object is kept for use when re-attaching.
#'
#' While we believe the strategy used here is mostly robust, users can defeat
#' it by messing with the tracing status of functions in tests or during
#' review.  Rather than attempt to have a completely robust solution we will
#' focus on detecting this type of thing happening and disable all search path
#' manipulation in that event.  The following are checked: \itemize{
#'   \item search path inconsistent with known recorded actions
#'   \item \code{\link{tracingState}} turned off
#'   \item functions \code{library}, \code{attach}, \code{detach} being changed
#'     from their traced versions
#' }
#' The checks themselves are not fully foolproof either as a user could turn
#' \code{tracingState} on and off within a single test expression, etc., but
#' that behavior should be extremely unlikely within the typical testing
#' environment.
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

search_path_track <- function(mode, pos=NA_integer_, , .global=.global) {
  res <- try(
    {
      stopifnot(
        is.chr1plain(mode), !is.na(mode),
        mode %in% c("library", "attach", "detach"),
        is.integer(pos), length(pos) == 1L
      )
      if(is.na(pos)) pos <- 2L

      # at most search path should have changed one element since last call

      a <- search()
      a.len <- length(a)
      track.len <- .global$tracking@search.path
      b.full <- .global$tracking@search.path[[track.len]]
      b <- names(b.full)
      b.len <- length(b)
      max.len <- max(a.len, b.len)

      # Make sure pos makes sense; note that infeasible `detach` `pos` values
      # cause detach to fail so we don't really need to worry about them

      if(!identical(mode, "detach")) {
        pos <- if(pos >= a.len) {
          a.len - 1L
        } else if (pos < 2L) 2L else pos
      } else {
        if(!pos %in% seq_along(b)[-c(1L, length(b))])
          stop("impossible pos value")
      }
      # Check things went okay

      res <- integer(0L)

      if(identical(a.len, b.len)) {
        if(!identical(a, b))
          stop("more than one item in search path changed")
      } else if (abs(a.len - b.len) != 1L) {
        stop("more than one item in search path changed")
      } else if (a.len < b.len && !identical(mode, "detach")) {
        stop("search path length may only decrease with detach")
      } else {
        # Try to figure out single change, though note this can be fooled if
        # user moves around multiple objects of same name, though this should be
        # a pretty rare occurrence; we could add a check for this by getting
        # environment name?

        sp <- b.full
        sp <- if(a.len > b.len) {   # attach
          if(!identical(a[-pos], b))
            stop("search path not as expected")
          res <- setNames(search_path_obj_nextid(a[[pos]]), a[[pos]])
          append(sp, res, pos - 1L)
        } else {              # detach
          if(!identical(a, b[-pos]))
            stop("search path not as expected")
          res <- sp[pos]
          sp[-pos]  # drop from search path
        }
        # Add an updated search path entry

        .global$tracking$search.path[[track.len + 1L]] <- sp
      }
      # Keep unitizer rooted just below globalenv; do we need to worry about
      # there only being one environment in the search path?

      if(.global$status@par.env) {
        parent.env(.global$par.env) <- as.environment(2L)
      }
      res
  } )
  if(inherits(res, "try-error")) {
    warning(
      "Search path tracking and manipulation failed so we are reverting to ",
      "vanilla mode: ", conditionMessage(attr(res, "cond"))
      immediate.=TRUE
    )
    .global$disable("search.path")
    return(FALSE)
  }
  res
}
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

search_path_trim <- function(keep=c("package:unitizer", "tools:rstudio")) {
  # Make sure search path is compatible with what we're doing

  search.path.pre <- search()
  # Set-up on exit function to attempt to restore search path in case something
  # went wrong

  on.exit({
    warning(
      "Unable to trim search path, so attempting to restore it.",
      immediate.=TRUE
    )
    search_path_restore()
  })

  # detach each object, and record them for purposes of restoring them later

  packs.to.detach <- setdiff(search.path.pre, c(keep, .unitizer.base.packages))

  for(i in seq_along(packs.to.detach)) {
    pack <- packs.to.detach[[i]]
    if(!is.character(pack) || length(pack) != 1L) {
      stop(
        "Logic Error: search path object was not character(1L); contact ",
        "maintainer"
      )
    }
    if(inherits(try(obj <- as.environment(pack)), "try-error")) {
      stop(
        "Logic Error: unable to convert search path element `", pack,
        "` to environment; contact maintainer."
      )
    }
    # Is it a package or an object?  Considered package if name starts with
    # "package:" and the package shows up as a namespace env

    is.pack <- is.loaded_package(pack)  # run before detaching

    # Detach all but `unitizer`

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
