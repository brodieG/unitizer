#' @include global.R
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
#' Additional Namespaces to Keep Loaded
#'
#' \itemize{
#'   \item data.table must be kept loaded due to issue
#'      \href{https://github.com/Rdatatable/data.table/issues/990}{#990}
#' }
#' @export

.unitizer.namespace.keep <- c("data.table")

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
#' default R configuration. This also unloads namespaces so that packages can
#' be reloaded with all corresponding hooks evaluating as with a normal load.
#' As documented in \code{?detach}, it is likely there may be some issues with
#' some packages with this approach.  This function is intended to be called
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

search_path_update <- function(id, global, force=FALSE) {
  stopifnot(
    is(global, "unitizerGlobal"),
    is.integer(id), length(id) == 1L, !is.na(id),
    id %in% seq_along(global$tracking@search.path),
    is.TF(force)
  )
  search.target <- global$tracking@search.path[[id]]
  search.curr <- global$tracking@search.path[[global$indices.last@search.path]]

  if(!identical(search_path_attrs(), search_path_attrs(search.curr))) {
    # not entirely sure this check is needed
    stop("Logic Error: mismatch between actual search path and tracked path")
  }
  # Get uniquely identified objects on search path; this isn't completely
  # perfect because some objects that are genuinely the same might be identified
  # as different because R copied them during attach / detach

  st.id <- search_path_unique_id(search.target)
  sc.id <- sc.id.tmp <- search_path_unique_id(search.curr)

  # Drop the stuff that definitely needs to be dropped, from back so that
  # numbering doesn't get messed up

  to.detach <- which(is.na(match(sc.id, st.id)))
  for(i in sort(to.detach, decreasing=TRUE)) detach(pos=i)
  if(length(to.detach)) sc.id.tmp <- sc.id.tmp[-to.detach]

  # Add the stuff that definitely needs to get added, but this time from the
  # front so the positions make sense

  for(i in sort(which(is.na(match(st.id, sc.id.tmp))))) {

    obj.name <- attr(search.target[[i]], "name")
    if(is.null(obj.name)) obj.name <- ""
    obj.type <- if(grepl("^package:.+", obj.name)) "package" else "object"
    obj.name.clean <- sub("^package:", "", obj.name)

    extra <- if(!is.null(attr(search.target[[i]], "path")))
      dirname(attr(search.target[[i]], "path"))

    reattach(
      i, name=obj.name.clean, type=obj.type, data=search.target[[i]],
      extra=extra
    )
    sc.id.tmp <- append(sc.id.tmp, st.id[[i]], i - 1L)
  }
  # Now see what needs to be swapped

  j <- 0
  repeat {
    reord <- match(sc.id.tmp, st.id)
    if(any(is.na(reord)) || !identical(length(reord), length(st.id)))
      stop("Logic Error: incorrect path remapping; contact maintainer.")

    if(!any(mismatch <- reord != seq_along(reord))) break

    if((j <- j + 1) > length(st.id) || length(which(mismatch)) < 2L)
      stop("Logic Error: unable to reorder search path; contact maintainer.")

    swap.id <- min(reord[mismatch])
    swap.pos <- which(reord == swap.id)
    move_on_path(new.pos=swap.id, old.pos=swap.pos)
    sc.id.tmp <- search_path_unique_id(search_as_envs())
  }
  if(!identical(search(), names(search.target)))
    stop("Logic Error: path reorder failed; contact maintainer.")

  # Replace all non packages with those in the target list since those may have
  # been changed

  tar.objs <- vapply(names(search.target), is.loaded_package, logical(1L))
  cur.objs <- vapply(names(search_as_envs()), is.loaded_package, logical(1L))

  if(!identical(tar.objs, cur.objs))
    stop("Logic Error: search path object type mismatch; contact maintainer.")

  if(!all(tar.objs)) {
    for(i in which(!tar.objs)) {
      if(i == 1L) next # global env doesn't count since
      detach(pos=i, character.only=TRUE)
      reattach(
        i, names(search.target)[[i]], type="object", data=search.target[[i]]
  ) } }

  if(
    !identical(
      sapply(search_as_envs(), attributes, simplify=FALSE),
      sapply(search.target, attributes, simplify=FALSE)
    )
  )
    stop("Logic Error: path reorder failed at last step; contact maintainer.")

  # Line up the namespaces

  if(!is.null(tar.lns.dat <- attr(search.target, "loadedNamespaces"))) {
    if(
      !is.list(tar.lns.dat) ||
      !all(vapply(tar.lns.dat, is, logical(1L), "unitizerNamespaceData"))
    )
      stop(
        "Logic Error: unable to line up namespaces across states because ",
        "namespace data in unexpected format; contact maintainer."
      )
    cur.lns <- loadedNamespaces()
    tar.lns.loc <- sapply(tar.lns.dat, slot, "lib.loc", simplify=FALSE)  # may contain nulls
    tar.lns <- names(tar.lns.loc)
    to.unload <- setdiff(cur.lns, tar.lns)
    to.keep <- global$unitizer.opts[["unitizer.namespace.keep"]]
    if(
      length(unload.conf <- which(to.unload %in% to.keep)) &&
      global$status@options && !force
    ) {
      many <- length(unload.conf) > 1L
      word_msg(
        "Incompatible `reproducible.state` settings: `unitizer` is trying to ",
        "auto-unload namespaces which are marked as un-unloadable while ",
        "`reproducible.state[[\"options\"]]` is greater than zero; either set ",
        "`reproducible.state[[\"options\"]]` to zero, or consult the ",
        "reproducible tests vignette (`vignette(\"vgn05reproducibletests\")`) ",
        "for other possible work-arounds.  The namespace",
        if(many) "s", " that caused this error ", if(many) "are" else "is",
        ": ", deparse(to.unload[unload.conf], width=500L), sep=""
      )
      stop("Unable to proceed")
    }
    to.keep.depends <- unlist(lapply(to.keep, getNamespaceImports))
    unload_namespaces(
      setdiff(
        cur.lns,
        c(tar.lns, to.keep, to.keep.depends)
    ) )
    to.load <- setdiff(tar.lns, loadedNamespaces())
    for(i in to.load) loadNamespace(i, lib.loc=dirname(tar.lns.loc[[i]]))
  }
  invisible(TRUE)
}
#' @keywords internal
#' @rdname search_path

search_path_trim <- function(
  keep=getOption("unitizer.search.path.keep")
) {
  if(!is.character(keep) && all(!is.na(keep))) {
    stop(
      "Global option `unitizer.search.path.keep` must be character ",
      "and not contain NAs; if you have not modified this option, contact ",
      "maintainer."
    )
  }
  # detach each object, but make sure we do so in an order that doesn't cause
  # issues with namespace dependencies

  search.path.pre <- search()

  to.detach <- setdiff(search.path.pre, c(keep, .unitizer.base.packages))
  to.detach.pkg <- vapply(to.detach, is.loaded_package, logical(1L))
  to.detach.pkg.names <- sub("^package:", "", to.detach[to.detach.pkg])

  to.keep <- intersect(c(keep, .unitizer.base.packages), search.path.pre)
  to.keep.pkg <- vapply(to.keep, is.loaded_package, logical(1L))
  to.keep.pkg.names <- sub("^package:", "", to.keep[to.keep.pkg])

  # start by detaching without unloading

  for(pack in to.detach) {
    if(!is.chr1(pack))
      stop("Logic Error: invalid search path token; contact maintainer.")
    detach(pack, character.only=TRUE)
  }
  # Now attempt to unload namespaces

  unload_namespaces(setdiff(loadedNamespaces(), to.keep.pkg.names))

  invisible(TRUE)
}
#' Unload Namespaces
#'
#' Attempts to unload namespaces in an order that avoids dependency issues
#' based on data from \code{getNamespaceImports}

unload_namespaces <- function(unload) {
  stopifnot(is.character(unload), all(!is.na(unload)))

  # Make sure search path is compatible with what we're doing

  search.path.pre <- search()
  search.path.pkg <- vapply(search.path.pre, is.loaded_package, logical(1L))
  search.path.pkg.names <-
    sub("^package:", "", search.path.pre[search.path.pkg])

  # Check that none of the keep namespaces reference namespaces other than the
  # keep namespaces, and warn otherwise since we won't be able to unload /
  # re-load that one

  lns.raw <- loadedNamespaces()
  if(!all(unload %in% lns.raw))
    stop(
      "Logic Error: attempting to unload namespaces that are not loaded; ",
      "contact maintainer."
    )
  lns.tmp <- sapply(
    unload, function(x) unique(names(getNamespaceImports(x))),
    simplify=FALSE
  )
  # Get lib locations to unload DLLs later (if applicable)

  lib.locs <- vapply(unload, find.package, character(1L))

  # order these in search path order if attached, as that will likely be easiest
  # order to detach in (though in most cases by the time we get here the
  # package should have been detached already - shouldn't be trying to unload)
  # the namespace of a still attached package

  lns.orig <- lns <- lns.tmp[
    order(
      match(names(lns.tmp), search.path.pkg.names, nomatch=0L)
  ) ]
  # Cycle through path attempting to unload namespaces until we cannot unload
  # any more.  This is not a particularly efficient algorithm, but should make
  # do for our purposes

  safety <- 0
  unloaded.success <- character(0L)
  repeat {
    if(safety <- safety + 1L > 1000)
      stop(
        "Logic Error: namespace unloading not complete after 1000 iterations"
      )
    lns.names <- names(lns)
    unloaded.try <- integer(0L)
    for(i in seq_along(lns)) {
      tar.ns <- names(lns)[[i]]
      if(!tar.ns %in% unlist(lns[-i])) {
        # No dependencies, so attempt to unload

        unloaded.try <- c(unloaded.try, i)
        attempt <- try(unloadNamespace(tar.ns))
        if(inherits(attempt, "try-error")) {
          warning(
            "Error while attempting to unload namespace `", tar.ns, "`",
            immediate.=TRUE
          )
        } else {
          unloaded.success <- c(unloaded.success, lns.names[i])
    } } }
    # Keep looping until length of remaining namespaces doesn't decrease anymore

    lns <- lns[-unloaded.try]
    if(!length(unloaded.try)) break
  }
  # Unload any dynamic libraries associated with the stuff we detached by
  # matching paths to what's in dynlibs

  dyn.lib.fnames <- vapply(.dynLibs(), `[[`, character(1L), "path")
  dls <- sub("/libs/[^/].*$", "", dyn.lib.fnames)
  lib.locs.ul <- lib.locs[unloaded.success]
  dls.to.ul <-  lib.locs.ul[lib.locs.ul %in% dls]

  for(i in names(dls.to.ul)) library.dynam.unload(i, dls.to.ul[i])

  # Warn if some namespaces could not be unloaded (likely due to dependency)

  if(length(lns)) {
    warning(
      "Unable to unload the following namespaces:\n", deparse(names(lns)),
      immediate.=TRUE
    )
  }
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

reattach <- function(pos, name, type, data, extra=NULL) {
  stopifnot(is.integer(pos), identical(length(pos), 1L), !is.na(pos), pos > 0L)
  stopifnot(is.chr1plain(name), !is.na(name))
  stopifnot(is.chr1plain(type), !is.na(type), type %in% c("package", "object"))
  stopifnot(is.environment(data))

  if(identical(type, "package")) {
    suppressPackageStartupMessages(
      library(
        name, pos=pos, quietly=TRUE, character.only=TRUE,
        lib.loc=extra, warn.conflicts=FALSE
    ) )
  } else {
    attach(data, pos=pos, name=name, warn.conflicts=FALSE)
  }
}
#' @keywords internal
#' @rdname reattach
move_on_path <- function(new.pos, old.pos) {
  stopifnot(is.integer(new.pos), length(new.pos) == 1L, !is.na(new.pos))
  stopifnot(is.integer(old.pos), length(old.pos) == 1L, !is.na(old.pos))
  stopifnot(old.pos > new.pos)  # otherwise detaching old.pos would mess path up
  stopifnot(new.pos > 1L)       # can't attach at 1L
  sp <- search()
  stopifnot(new.pos <= length(sp), old.pos <= length(sp))
  name <- sp[[old.pos]]
  obj <- as.environment(old.pos)

  if(is.loaded_package(name)) {
    type <- "package"
    extra <- dirname(attr(obj, "path"))
  } else {
    type <- "object"
    extra <- NULL
  }
  name.clean <- sub("package:", "", name)
  detach(pos=old.pos)
  reattach(pos=new.pos, name=name.clean, type=type, data=obj, extra=extra)
}
#' Make Unique IDs For Search Path Object
#'
#' adds ".1", ".2" etc if there are non-unique values, but first occurence is
#' not modified so we can match between a list with duplicates and one without.
#'
#' @keywords internal

search_path_unique_id <- function(search.path.objs) {
  stopifnot(
    is.list(search.path.objs),
    all(vapply(search.path.objs, is.environment, logical(1L)))
  )
  sp.id.base <- names(search.path.objs)
  ave(
    sp.id.base, sp.id.base,
    FUN=function(x) {
      if(length(x) == 1L) return(x)
      c(head(x, 1L), paste0(tail(x, -1L), ".", 1:(length(x) - 1L)))
    }
  )
}

#' Generate An Identifier out of SP objects
#'
#' Not perfect, by any means, but necessary because we can't directly compare
#' the search path environments as they change on detach / re-attach

search_path_attrs <- function(x=NULL) {
  if(is.null(x)) x <- search_as_envs()
  sapply(x, attributes, simplify=FALSE)
}
get_namespace_data <- function() {
  sapply(
    loadedNamespaces(),
    function(x) {
      loc <- try(getNamespace(x)[[".__NAMESPACE__."]][["path"]])
      ver <- try(getNamespaceVersion(x))
      new(
        "unitizerNamespaceData",
        name=x,
        lib.loc=if(!inherits(loc, "try-error")) loc else "",
        version=if(!inherits(ver, "try-error")) ver else ""
      )
    },
    simplify=FALSE
  )
}
get_package_data <- function() {
  sapply(
    search(),
    function(x) {
      loc <- try(package.path(x))
      ver <- try(getNamespaceVersion(x))
      new(
        "unitizerNamespaceData",
        name=x,
        lib.loc=if(!inherits(loc, "try-error")) loc else "",
        version=if(!inherits(ver, "try-error")) ver else ""
      )
    },
    simplify=FALSE
  )
}
setClass(
  "unitizerNamespaceData",
  slots=c(
    name="character", lib.loc="characterOrNULL", version="characterOrNULL"
) )
setClass(
  "unitizerPackageData",
  slots=c(
    name="character", lib.loc="characterOrNULL", version="characterOrNULL"
) )
