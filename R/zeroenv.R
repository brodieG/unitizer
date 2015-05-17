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


#' Search Path Back-up
#'
#' Holds the state of the search path before `unitizer` to serve as a back-up
#' in case the search path manipulation functions are unable to restore the
#' search path to its original value.
#'
#' @export
#' @return character the search path

unitizer_search_path_backup <- function() {
  .unitizer.pack.env$search.init
}

#' Default List of Packages To Keep on Search Path
#'
#' @export

.unitizer.base.packages <- c(
  "package:stats", "package:graphics", "package:grDevices", "package:utils",
  "package:datasets", "package:methods", "Autoloads", "package:base", ".GlobalEnv"
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

#' Update Our View of What Search Path is
#'
#' Set of functions used to track and set search path state.  Strategy is to
#' keep track of every different search path state encountered from the first
#' \code{\link{reset_packenv}} call, which is done by shimming the library (and
#' require by extension), detach, and attach functions.
#'
#' Any time any of those functions is called, \code{search_path_track} updates
#' the journaling information kept in \code{.unitizer.pack.env} and makes sure
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
#' @keywords internal
#' @rdname search_path

search_path_track <- function(mode, pos=NA_integer_) {
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
      b.full <-
        .unitizer.pack.env$search.path[[length(.unitizer.pack.env$search.path)]]
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

        .unitizer.pack.env$search.path[[length(.unitizer.pack.env$search.path) + 1L]] <-
          sp
        .unitizer.pack.env$search.curr <- sp
      }
      # Keep unitizer rooted just below globalenv; do we need to worry about
      # there only being one environment in the search path?

      if(.unitizer.pack.env$global.opts.status@par.env) {
        parent.env(.unitizer.pack.env$zero.env.par) <- as.environment(2L)
      }
      res
  } )
  if(inherits(res, "try-error")) {
    warning(
      "Search path tracking and manipulation failed so we are reverting to ",
      "vanilla mode: ", conditionMessage(attr(res, "cond"))
      immediate.=TRUE
    )
    search_path_unsetup()
    return(FALSE)
  }
  res
}
#' Update Search Path
#'
#' Changes search path to previously recorded states when interatively reviewing
#' tests.
#'
#' @param id integer(1L) what recorded state to revert to

search_path_update <- function(id) {
  stopifnot(
    !is.integer(id), length(id) != 1L, is.na(id),
    !id %in% seq(.unitizer.pack.env$search.path)
  )
  search.target <- .unitizer.pack.env$search.path[[id]]
  search.curr <- search.curr.tmp <- .unitizer.pack.env$search.curr
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
    obj <- try(.unitizer.pack.env$search.objs[[obj.name]][[obj.seq]])
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
  .unitizer.pack.env$search.curr <- search.target

  if(.unitizer.pack.env$status@par.env)
    parent.env(.unitizer.pack.env$zero.env.par) <- as.environment(2L)

  invisible(NULL)
}

#' Restore Search Path to Bare Bones R Default
#'
#' \code{search_path_trim} attempts to recreate a clean environment by
#' unloading all packages and objects that are not loaded by default in the
#' default R  configuration.
#'
#' Note this does not unload namespaces, but rather just detaches them from
#' the namespace
#'
#' \code{`tools:rstudio`} is kept in search path as the default argument because
#' it isn't possible to cleanly unload and reload it because \code{`attach`}
#' actually attaches a copy of it's argument, not the actual object, and that
#' causes problems for that search path item.
#'
#' @seealso \code{`\link{search_path_restore}`}  \code{`\link{search}`}
#' @keywords internal
#' @param keep character names of packages/objects to keep in search path;
#'   note that base packages (see .unitizer.base.packages) that come typically
#'   pre attached are always kept.  The \code{`keep`} packages are an addition
#'   to those.
#' @return invisibly TRUE on success, FALSE on failure

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
#' Restore Search Path to State Before \code{`search_path_trim`}
#'
#' Undoes \code{`search_path_trim`}
#'
#' @seealso \code{`\link{search_path_trim}`}  \code{`\link{search}`}
#' @keywords internal
#' @return TRUE on success, FALSE on failure, invisibly

search_path_restore <- function() {

  search_path_unsetup()  # no matter what, untrace the functions

  # Make sure everything is as we expect before we actually do anything

  if(!search_path_check()) {
    warning(
      "Unexpected search path encountered, this likely occurred because you ",
      "somehow bypassed in your test code the traced versions of ",
      "`base::library/attach/detach` that `unitizer` provides. ",
      "This will happen if you either `trace` or `untrace` any of `library`, ",
      "`attach`, or `detach` from package `base`, or if you turn off tracing ",
      "with `tracingState`. `unitizer` relies on the traced versions of those ",
      "functions to track modifications to the search path.  If you did not do ",
      "any of the above, but are still seeing this message, please contact ",
      "maintainer. \n\nWe are unable to restore the search path to its ",
      "original value (you can retrieve orginal value with ",
      "`unitizer_search_path_backup()`).",
      .unitizer.search.fail.msg, immediate.=TRUE
    )
    return(invisible(FALSE))
  }
  # Step back through history, undoing each step; not this means we need to
  # potentially re-attach an object that was previously attached to the search
  # path.  While we realize this is bad practice, the only reason we are doing
  # this is because said object was already attached and we are restoring the
  # search path to its previous state


  for(i in rev(seq_along(.unitizer.pack.env$history))) {
    hist <- .unitizer.pack.env$history[[i]]
    res <- try({
      if(hist@mode == "add") {  # Need to remove
        if(                     # Keep namespace
          hist@type == "object" ||
          (
            hist@type == "package" &&
            hist@name %in% .unitizer.pack.env$search.init
          )
        ) {
          detach(pos=hist@pos, character.only=TRUE)
        } else detach(pos=hist@pos, unload=TRUE, character.only=TRUE)
      } else if(hist@mode == "remove") { # Need to add back
        if(hist@type == "package") {
          suppressPackageStartupMessages(
            relib(
              hist@name, pos=hist@pos, quietly=TRUE, character.only=TRUE,
              lib.loc=dirname(attr(hist@extra, "path")), warn.conflicts=FALSE
          ) )
        } else if (hist@type == "object") {
          reattach(
            hist@extra, pos=hist@pos, name=hist@name, warn.conflicts=FALSE
          )
        }
      }
    })
    if(inherits(res, "try-error")) {
      warning(
        "Failed attempting to restore search path at step ",
        switch(hist@mode, add="remove", remove="add", "<unknown>"),
        " `", hist@name, "`.  Unable to fully restore search path.  ",
        .unitizer.search.fail.msg, immediate.=TRUE
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
    return(FALSE)
  just.name <- sub("^package:(.*)", "\\1", pkg.name)
  pkg.name %in% search() && just.name %in% loadedNamespaces()
}

#' Path manipulation functions
#'
#' @keywords internal

reattach <- function(pos, name, type, data, extra) {
  stopifnot(is.integer(pos), identical(length(pos), 1L), is.na(pos), pos < 1L)
  stopifnot(is.chr1plain(name), is.na(name))
  stopifnot(is.chr1plain(type), is.na(type), type %in% c("package", "object"))
  stopifnot(is.environment(data))

  if(identical(type, "package")) {
    suppressPackageStartupMessages(
      .library(
        name, pos=pos, quietly=TRUE, character.only=TRUE,
        lib.loc=extra, warn.conflicts=FALSE
    ) )
  } else {
    .attach(data, pos=i, name=name, warn.conflicts=FALSE)
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
  detach(old.pos)
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
  sp.curr <- .unitizer.pack.env$search.curr
  id.max <- max(
    sp.curr[names(sp.curr) == name],
    length(.unitizer.pack.env$search.objs[[name]])
  )
  id.max + 1L
}

# Internal re-use to restore search path, need to do this to get:
# * version of the functions that are not traced
# * quash check NOTE, though we don't think what we do here is against the
#   spirit of the note

.attach <- base::attach       # quash a NOTE (as per above, we don't think this is against the spirit of the note)
.library <- base::library     # as above

