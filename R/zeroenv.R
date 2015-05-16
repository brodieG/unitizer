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
#' Objects used to Track Global Settings and the Like
#'
#' Note \code{`"package:unitizer"`} is not actually detached but is included in
#' the list so that it is replaced in the same position on the search path
#'
#' Packages are stored as character values, other objects are stored directly.
#'
#' Probably should be an RC object instead of an environment.
#'
#' @keywords internal

.unitizer.pack.env <- new.env()

#' Clears out Global State
#'
#' Should be called by `unitize` each time
#'
#' @return the package environment we use as a global variable
#' @keywords internal

reset_packenv <- function() {
  .unitizer.pack.env$zero.env.par <- new.env(parent=.GlobalEnv)
  .unitizer.pack.env$search.init <- character()   # Initial search path b4 any modifications

  .unitizer.pack.env$opts <- list()                 # options after helper
  .unitizer.pack.env$wds <- list()

  sp <- search()
  sp.full <- setNames(ave(integer(length(sp)) + 1L, sp, FUN=cumsum), sp)
  .unitizer.pack.env$search <- list(sp.full)
  .unitizer.pack.env$search.curr <- sp.full
  .unitizer.pack.env$search.objs <- list()        # populated by `detach`

  .unitizer.pack.env
}
#' Search Path Back-up
#'
#' Holds the state of the search path before `unitizer` to serve as a back-up
#' in case the search path manipulation functions are unable to restore the
#' search path to it's original value.
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
#' Tells us the difference prior to the last added search path, and as side
#' effects:
#' * updates the search path stack so that current is latest
#' * makes sure zeroenv points to the environment just below GlobalEnv
#'
#' This is supposed to be run \bold{after} the search path functions do their
#' thing.
#'
#' @keywords internal
#' @return the index of the difference if any (named integer where name is
#'   search path item and value is the sequence if item name is repeated), or
#'   integer(0L) if no difference

search_path_track <- function(mode) {
  stopifnot(
    !is.chr1plain(mode), is.na(mode),
    !mode %in% c("library", "require", "attach", "detach")
  )
  # at most search path should have changed one element since last call

  a <- search()
  b.full <-
    .unitizer.pack.env$search[[length(.unitizer.pack.env$search)]]
  b <- names(sp)

  a.len <- length(a)
  b.len <- length(b)

  res <- integer(0L)

  if(identical(a.len, b.len)) {
    if(!identical(a, b))
      stop("Logic Error: more than one item in search path changed")
  } else if (abs(a.len - b.len) != 1L) {
    stop("Logic Error: more than one item in search path changed")
  } else if (a.len < b.len && !identical(mode, "detach")) {
    stop("Logic Error: search path length may only decrease with detach")
  } else {
    first.diff <- head(which(a[-a.len] != b[-b.len]), 1L)
    if(length(first.diff)) {
      if(a.len > b.len) {
        x <- a[-first.diff]
        y <- b
      } else {
        x <- b[-first.diff]
        y <- a
      }
      if(!identical(x, y))
        stop("Logic Error: more than one item in search path changed")
      res <- if(a.len < b.len) {
        b[first.diff]
      } else  {
        setNames(max(b[b == a[first.diff]], 0L) + 1L, a[first.diff])
      }
      # Add an updated search path entry

      sp <- b.full
      sp <- if(a.len > b.len) {
        length(sp) <- length(sp + 1L)
        sp[(first.diff + 1L):length(sp)] <- sp[first.diff:(length(sp) - 1L)]
        sp[first.diff] <- res
        names(sp)[first.diff] <- names(res)
        sp
      } else {
        b.full[-first.diff]
      }
      .unitizer.pack.env$search[[length(.unitizer.pack.env$search) + 1L]] <- sp
      .unitizer.pack.env$search.curr <- sp
  } }
  # Keep unitizer rooted just below globalenv; do we need to worry about
  # there only being one environment in the search path?

  parent.env(unitizer.env$zero.env.par) <- as.environment(2L)
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
    !id %in% seq(.unitizer.pack.env$search)
  )
  search.target <- .unitizer.pack.env$search[[id]]
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
  invisible(NULL)
}

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
  if(is(base::attach, "functionWithTrace")) fail.shim <- c(fail.shim, "attach")
  if(is(base::detach, "functionWithTrace")) fail.shim <- c(fail.shim, "detach")

  if(length(fail.shim)) {
    warning(
      "Cannot trace ", paste0(fail.shim, collapse=", "), " because already traced.",
      immediate.=TRUE
    )
    return(FALSE)
  }
  # Suppress std.err because of "Tracing Function..." messages produced by trace

  std.err <- tempfile()
  std.err.con <- file(std.err, "w+b")
  on.exit({
    try(get_text_capture(std.err.con, std.err, type="message"))
    release_sinks()
    close(std.err.con)
    unlink(std.err)
    try(search_path_unsetup())
    stop(
      "Unexpectedly failed while attempting to setup search path.  You may ",
      "need to exit R to restore search path and to untrace ",
      "`library/attach/detach`; this should not happen so please report to ",
      "maintainer."
    )
  } )
  capt.con <- set_text_capture(std.err.con, "message")

  # Attempt to apply shims

  shimmed <- try({
    # Shim library, note we cannot use the `exit` param since `library` uses
    # on.exit

    library.shim <- quote({
      untz <- asNamespace("unitizer")
      if (!character.only) {
        package <- as.character(substitute(package))
        character.only <- TRUE
      }
      res <- try(
        untz$.library(
          package=package, help=help, pos = pos, lib.loc = lib.loc,
          character.only = character.only, logical.return = logical.return,
          warn.conflicts = warn.conflicts, quietly = quietly,
          verbose = verbose
      ) )
      if(inherits(res, "try-error")) {
        cond <- attr(res, "condition")
        stop(simpleError(conditionMessage(cond), sys.call()))
      }
      untz$search_track("library") # Update search path tracking
      return(res)
    })
    trace(
      base::library, library.shim, at=1L, where=.BaseNamespaceEnv, print=FALSE
    )
    # Shim require (actually, this is done indirectly by the library shim)

    NULL

    # Shim attach

    trace(
      base::attach, at=1L,
      exit=quote(asNamespace("unitizer")$search_track("attach")),
      where=.BaseNamespaceEnv, print=FALSE
    )
    # Shim detach; here need to make sure we save a copy of what we are
    # detaching so we can re-attach later if needed

    if(
      !identical(
        as.list(body(base::detach))[[3]],
        quote(packageName <- search()[[pos]])
      )
    )
      stop(
        "Logic Error: Unable to shim `base:detach` because the code is not ",
        "the same as it was when this package was developed; contact package ",
        "maintainer."
      )
    trace(
      base::detach, at=4L,
      tracer=quote({
        .unitizer.detach.obj <- as.environment(pos)
        .unitizer.detach.name <- search()[[pos]]
      }),
      exit=quote({
        untz <- asNamespace("unitizer")
        res <- untz$search_track("detach")
        if(!exists(".unitizer.detach.obj") && length(res))
          stop("Logic Error: search path shorter, but no detach object")

        if(is.null(untz$.unitizer.pack.env$search.objs[[names(res)]])) {
          untz$.unitizer.pack.env$search.objs[[names(res)]] <- list()
        }
        untz$.unitizer.pack.env$search.objs[[names(res)]][[res]] <- new(
          "unitizerSearchData",
          name=.unitizer.detach.name,
          type=if(is.loaded_package(.unitizer.detach.name))
            "package" else "object",
          data=.unitizer.detach.obj,
          extra=dirname(attr(.unitizer.detach.obj, "path"))
        )
      }),
      where=.BaseNamespaceEnv, print=FALSE
    )
  })
  # Process std.err to make sure nothing untoward happened

  shim.out <- get_text_capture(capt.con, std.err, "message")

  on.exit(NULL)
  close(std.err.con)
  unlink(std.err)
  if(
    !identical(
      gsub("\\s", "", paste0(shim.out, collapse="")),
      gsub("\\s", "",
        paste0(
          "Tracing function \"library\" in package \"namespace:base\"",
          "Tracing function \"attach\" in package \"base\"",
          "Tracing function \"detach\" in package \"base\""
      ) )
    ) || inherits(shimmed, "try-error")
  ) {
    cat(shim.out, file=stderr(), sep="\n")
  }
  if(inherits(shimmed, "try-error")) {
    warning(
      "Unable to shim all of library/require/attach/detach.  ",
      .unitizer.search.fail.msg.extra, immediate.=TRUE
    )
    search_path_unsetup()
    return(FALSE)
  }
  # Track initial values

  .unitizer.pack.env$search.init <- search()

  # Setup zero env parent

  parent.env(.unitizer.pack.env$zero.env.par) <- as.environment(2L)

  return(TRUE)
}
#' Search Path Unsetup
#'
#' Undoes all the shimming we applied
#'
#' @keywords internal

search_path_unsetup <- function() {
  # Suppress std.err because of "Untracing function..." messages produced by trace

  std.err <- tempfile()
  std.err.con <- file(std.err, "w+b")
  on.exit({
    try(get_text_capture(std.err.con, std.err, type="message"))
    release_sinks()
    close(std.err.con)
    unlink(std.err)
    stop(
      "Unexpectedly failed while attempting to restore search path.  You may ",
      "need to exit R to restore search path and to untrace ",
      "`library/attach/detach`; this should not happen so please report to ",
      "maintainer."
    )
  } )
  capt.con <- set_text_capture(std.err.con, "message")

  unshim <- try({  # this needs to go
    untrace(library, where=.BaseNamespaceEnv)
    untrace(attach, where=.BaseNamespaceEnv)
    untrace(detach, where=.BaseNamespaceEnv)
  })
  unshim.out <- get_text_capture(capt.con, std.err, "message")
  on.exit(NULL)
  close(std.err.con)
  unlink(std.err)

  if(
    !identical(
      gsub("\\s", "", paste0(unshim.out, collapse="")),
      gsub("\\s", "",
        paste0(
          "Untracing function \"library\" in package \"namespace:base\"",
          "Untracing function \"attach\" in package \"namespace:base\"",
          "Untracing function \"detach\" in package \"namespace:base\""
      ) )
    ) || inherits(unshim, "try-error")
  ) {
    cat(unshim.out, file=stderr(), sep="\n")
  }
  if(inherits(unshim, "try-error")) {
    warning(
      "Failed trying to unshim library/require/attach/detach, ",
      "which means some of those functions are still modified for search path ",
      "manipulation by `unitizer`.  Restarting R should restore the original ",
      "functions.", .unitizer.search.fail.msg.extra
    )
    return(invisible(FALSE))
  }
  invisible(TRUE)
}
#' Reconstruct Search Path From History
#'
#' This is an internal check to make sure the shims on \code{`library/require/attach/detach`}
#' worked correctly.
#'
#' @param verbose whether to output details of failures, purely for internal debugging
#' @keywords internal

search_path_check <- function(verbose=FALSE) {
  hist <- .unitizer.pack.env$history
  names <- vapply(as.list(hist), slot, "", "name")
  types <- vapply(as.list(hist), slot, "", "type")
  modes <- vapply(as.list(hist), slot, "", "mode")
  poss <- vapply(as.list(hist), slot, 1L, "pos")

  names <- ifelse(types == "package", paste0("package:", names), names)

  search.init <- .unitizer.pack.env$search.init

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
        if(verbose)
          warning(
            "Object to detach `", names[[i]], "` not at expected position (",
            poss[[i]], ").", immediate.=TRUE
          )
        return(FALSE)
      }
      search.init <- search.init[-poss[[i]]]
    }
  }
  if(!identical(search(), search.init)) {
    if(verbose) {
      warning(
        "Mismatches between expected search path and actual:\n - expected: ",
        deparse(search.init), "\n - actual: ", deparse(search()), immediate.=TRUE
      )
    }
    return(FALSE)
  }
  return(TRUE)
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

# Internal re-use to restore search path, need to do this to get:
# * version of the functions that are not traced
# * quash check NOTE, though we don't think what we do here is against the spirit of the note

.attach <- base::attach       # quash a NOTE (as per above, we don't think this is against the spirit of the note)
.library <- base::library     # as above

