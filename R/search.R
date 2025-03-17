# Copyright (C) Brodie Gaslam
#
# This file is part of "unitizer - Interactive R Unit Tests"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' @include class_unions.R

NULL

# Class To Track History Changes

setClass(
  "unitizerNsListData", contains="unitizerList",
  validity=function(object) {
    if(!all(vapply(object@.items, is, logical(1L), "unitizerNamespaceData")))
      return("Object may only contain environments")
    TRUE
  }
)
# # Deprecated b/c way too slow
# setClass(
#   "unitizerSearchData", contains="unitizerList",
#   slots=c(ns.dat="unitizerNsListData"),
#   prototype=list(data=NULL),
#   validity=function(object) {
#     if(!all(vapply(object@.items, is.environment, logical(1L))))
#       return("Object may only contain environments")
#     TRUE
#   }
# )
# setGeneric(
#   # nocov start
#   "unitizerGetPaths", function(x, ...) StandardGeneric("unitizerGetPaths")
#   # nocov end
# )
# setMethod(
#   "unitizerGetPaths", "unitizerSearchData",
#   function(x, ...) lapply(as.list(x), attr, "path")
# )
# setGeneric(
#   # nocov start
#   "unitizerGetVersions", function(x, ...) StandardGeneric("unitizerGetVersions")
#   # nocov end
# )

#' @importFrom utils sessionInfo

get_package_versions <- function(x) {
  ns.loaded <- names(x$ns.dat)
  ns.version <- vapply(x$ns.dat, "[[", character(1L), "version")

  pkg.names <- names(x$search.path)
  are.pkg <- grepl("^package:.+", pkg.names)
  pkg.names <- sub("^package:(.*)", "\\1", pkg.names)

  # base packages should not show version (issue203)

  base.pkg <- sessionInfo()$basePkgs

  pkg.sub <- match(pkg.names, ns.loaded)
  pkg.ver <- ns.version[pkg.sub]
  pkg.ver[!are.pkg | pkg.names %in% base.pkg] <- NA_character_
  pkg.ver[pkg.names == "base"] <- as.character(getRversion())
  pkg.ver
}
# Used to be an S4 method for the search data objects
#
# Checks whether the search data objects are equivalent, note we check names b/c
# environments will usually not be equal

search_dat_equal <- function(target, current) {
  res <- character()
  tar <- target$search.path
  cur <- current$search.path
  if(!isTRUE(name.comp <- all.equal(names(tar), names(cur))))
    res <- c("Search Path Name Mismatch:", name.comp)
  if(
    !isTRUE(
      path.comp <- all.equal(
        lapply(tar, attr, "path"), lapply(cur, attr, "path")
  ) ) )
    res <- c("Search Path Object Path Mismatch:", path.comp)
  if(length(res)) res else TRUE
}
# setClass(
#   "unitizerNamespaceData",
#   slots=c(
#     name="character", lib.loc="characterOrNULL", version="characterOrNULL"
# ) )
# setClass(
#   "unitizerPackageData",
#   slots=c(
#     name="character", lib.loc="characterOrNULL", version="characterOrNULL"
# ) )

# Reduce search path info to pkg name and version

compress_search_data <- function(x) {
  res <- names(x$search.path)
  res.pkg <- grepl("^package:.+", res)
  ver <- get_package_versions(x)
  res[!is.na(ver)] <- paste0(res[!is.na(ver)],  " (v", ver[!is.na(ver)], ")")
  res
}
compress_ns_data <- function(x) {
  vapply(x, function(y) sprintf("%s (v%s", y$names, y$version), character(1L))
}
# Search Path Management Functions
#
# Set of functions used to manage search path state.  Strategy is to
# keep track of every different search path state encountered which is done
# with \code{.global}, and then allow restoring to previous states with these
# functions.
#
# While we believe the strategy used here is mostly robust, users can defeat
# by changing search paths multiple times withing a single test, which we will
# not journal properly, though this is not likely to be a major issue.
#
# \code{search_path_trim} attempts to recreate a clean environment by
# unloading all packages and objects that are not loaded by default in the
# default R configuration.
#
# This function is intended to be called after journaling has been enabled.
#
# \code{tools:rstudio} is kept in search path as the default argument because
# it is not possible to cleanly unload and reload it because \code{attach}
# actually attaches a copy of it's argument, not the actual object, and that
# causes problems for that search path item.
#
# @keywords internal
# @rdname search_path
# @param keep character names of packages/objects to keep in search path;
#   note that base packages (see .unitizer.base.packages) that come typically
#   pre attached are always kept.  The \code{keep} packages are an addition
#   to those.
# @param id integer(1L) what recorded state to revert to
# @param global reference object of class "unitizerGlobal" that holds the
#   global settings

search_path_update <- function(id, global) {
  stopifnot(is(global, "unitizerGlobal"), is.int.pos.1L(id))
  if(!id %in% seq_along(global$tracking@search.path)) {
    stop( # nocov start
      "Internal Error: attempt to reset state to unknown index; contact ",
      "maintainer"
    )     # nocov end
  }
  search.target <- global$tracking@search.path[[id]]
  search.curr <- global$tracking@search.path[[global$indices.last@search.path]]

  curr.env.check <- search_as_envs()

  if(!isTRUE(search_dat_equal(curr.env.check, search.curr))) {
    # not entirely sure this check is needed, or might be too stringent
    # new version of comparing entire object not tested
    # nocov start
    stop("Internal Error: mismatch between actual search path and tracked path")
    # nocov end
  }
  # If we exit pre-maturely we'll be in a weird state so we need to mark this
  # state so that next time we get here during the overall `on.exit` we can
  # proceed

  on.exit(global$state())

  # Get uniquely identified objects on search path; this isn't completely
  # perfect because some objects that are genuinely the same might be identified
  # as different because R copied them during attach / detach

  st.id <- unitizerUniqueNames(search.target)
  sc.id <- sc.id.tmp <- unitizerUniqueNames(search.curr)

  # Drop the stuff that definitely needs to be dropped, from back so that
  # numbering doesn't get messed up

  to.detach <- which(is.na(match(sc.id, st.id)))
  for(i in sort(to.detach, decreasing=TRUE)) detach(pos=i)
  if(length(to.detach)) sc.id.tmp <- sc.id.tmp[-to.detach]

  # Add the stuff that definitely needs to get added, but this time from the
  # front so the positions make sense

  for(i in sort(which(is.na(match(st.id, sc.id.tmp))))) {

    obj.name <- names(search.target$search.path)[[i]]
    if(is.null(obj.name)) obj.name <- ""
    obj.type <- if(grepl("^package:.+", obj.name)) "package" else "object"
    obj.name.clean <- sub("^package:", "", obj.name)

    extra <- if(!is.null(attr(search.target$search.path[[i]], "path")))
      dirname(attr(search.target$search.path[[i]], "path"))

    reattach(
      i, name=obj.name.clean, type=obj.type,
      data=search.target$search.path[[i]], extra=extra, global=global
    )
    sc.id.tmp <- append(sc.id.tmp, st.id[[i]], i - 1L)
  }
  # Now see what needs to be swapped; make sure not to detach environments that
  # are not package environments that should be kept on the search path as doing
  # so leads to them getting copied

  search.keep <- keep_sp_default(global$unitizer.opts)
  j <- 0
  repeat {
    reord <- match(sc.id.tmp, st.id)
    if(any(is.na(reord)) || !identical(length(reord), length(st.id)))
      # nocov start
      stop("Internal Error: incorrect path remapping; contact maintainer.")
      # nocov end

    if(!any(mismatch <- reord != seq_along(reord))) break

    if((j <- j + 1) > length(st.id) || length(which(mismatch)) < 2L)
      # nocov start
      stop("Internal Error: unable to reorder search path; contact maintainer.")
      # nocov end

    swap.valid <- mismatch & (
      grepl("package:.+", sc.id.tmp) | !sc.id.tmp %in% search.keep
    )
    if(!any(swap.valid))
      # nocov start
      stop(
        "Internal Error: unable to reorder search path because of ",
        "'unitizer.search.path.keep' limitations. If you added objects ",
        "to that option, make sure you're not also attaching/detaching ",
        "them in your tests.  If you are not doing those things, contact ",
        "maintainer."
      )
      # nocov end
    swap.id <- min(reord[swap.valid])
    swap.pos <- which(reord == swap.id)
    move_on_path(new.pos=swap.id, old.pos=swap.pos, global=global)
    sc.id.tmp <- unitizerUniqueNames(search_as_envs())
  }
  search.new <- search()
  sp.check <- match(search.new, names(search.target$search.path))
  if(any(is.na(sp.check)) || any(diff(sp.check) < 1L))
    # nocov start
    stop("Internal Error: path reorder failed; contact maintainer.")
    # nocov end
  search.target$search.path <- search.target$search.path[search.new]

  # Replace all non packages with those in the target list since those may have
  # been changed (note, using search.new as it is possible we failed to fully
  # restore path (e.g. if a package is removed but not dettached/unloaded))

  tar.objs <- vapply(search.new, is.loaded_package, logical(1L))
  cur.objs <- vapply(
    names(search_as_envs()$search.path), is.loaded_package, logical(1L)
  )
  if(!identical(tar.objs, cur.objs))
    # nocov start
    stop("Internal Error: search path object type mismatch; contact maintainer.")
    # nocov end

  if(!all(tar.objs)) {
    for(
      i in which(!tar.objs & !(search.new %in% search.keep))
    ) {
      # Don't replace identical elements; this is meant to avoid re-attaching
      # environments since doing so actually leads to a copy of the
      # environment being made

      if(identical(as.environment(i), search.target$search.path[[i]])) next

      detach(pos=i, character.only=TRUE)
      reattach(
        i, names(search.target$search.path)[[i]], type="object",
        data=search.target$search.path[[i]],
        global=global
  ) } }
  # Updated comparison method (might be too stringent)

  if(!isTRUE(search_dat_equal(search_as_envs(), search.target)))
    # nocov start
    stop("Internal Error: path reorder failed at last step; contact maintainer.")
    # nocov end

  on.exit(NULL)
  invisible(TRUE)
}
# This also unloads namespaces so that packages can
# be reloaded with all corresponding hooks evaluating as with a normal load.
# As documented in \code{?detach}, it is likely there may be some issues with
# some packages with this approach.
#
# This function is intended to be called after journaling has been enabled.
#
# @param id integer(1L) what recorded state to revert to
# @param global reference object of class "unitizerGlobal" that holds the
#   global settings

namespace_update <- function(id, global) {
  stopifnot(is(global, "unitizerGlobal"), is.int.pos.1L(id))
  if(!id %in% seq_along(global$tracking@namespaces))
    # nocov start
    stop(
      "Internal Error: attempt to reset namespaces to unknown index; contact ",
      "maintainer"
    )
    # nocov end
  ns.target <- global$tracking@namespaces[[id]]
  # should this be get_namespace_data()?
  ns.curr <- global$tracking@namespaces[[global$indices.last@namespaces]]

  ns.in.common <- intersect(names(ns.target), names(ns.curr))
  ns.extra <- setdiff(names(ns.curr), ns.in.common)

  # Line up the namespaces

  cur.lns <- loadedNamespaces()

  # may contain nulls

  tar.lns.loc <- sapply(as.list(ns.target), "[[", "lib.loc", simplify=FALSE)
  tar.lns <- names(ns.target)
  to.unload <- setdiff(cur.lns, tar.lns)

  unload_namespaces(
    to.unload, global=global,
    keep.ns=global$unitizer.opts[["unitizer.namespace.keep"]]
  )
  to.load <- setdiff(tar.lns, loadedNamespaces())
  for(i in to.load) {
    try.ln <- try(loadNamespace(i, lib.loc=dirname(tar.lns.loc[[i]])))
    if(inherits(try.ln, "try-error"))
      warning(
        "Unable to fully restore previously loaded namespaces.",
        immediate.=TRUE
      )
  }
  invisible(TRUE)
}
search_path_trim <- function(
  keep.path=keep_sp_default(options()),
  global
) {
  stopifnot(
    is.character(keep.path) && !any(is.na(keep.path)),
    is(global, "unitizerGlobal")
  )
  on.exit(global$state())

  # detach each object, but make sure we do so in an order that doesn't cause
  # issues with namespace dependencies

  search.path.pre <- search()

  to.detach <- setdiff(search.path.pre, c(keep.path, .unitizer.base.packages))
  to.detach.pkg <- vapply(to.detach, is.loaded_package, logical(1L))
  to.detach.pkg.names <- sub("^package:", "", to.detach[to.detach.pkg])

  to.keep <- intersect(c(keep.path, .unitizer.base.packages), search.path.pre)
  to.keep.pkg <- vapply(to.keep, is.loaded_package, logical(1L))
  to.keep.pkg.names <- sub("^package:", "", to.keep[to.keep.pkg])

  # start by detaching without unloading

  for(pack in to.detach) {
    if(!is.chr1(pack))
      # nocov start
      stop("Internal Error: invalid search path token; contact maintainer.")
      # nocov end
    detach(pack, character.only=TRUE)
  }
  invisible(TRUE)
}
namespace_trim <- function(
  keep.ns=keep_ns_default(options()),
  global
) {
  stopifnot(
    is.character(keep.ns) && !any(is.na(keep.ns)), is(global, "unitizerGlobal")
  )
  on.exit(global$state())
  unload_namespaces(loadedNamespaces(), global=global, keep.ns=keep.ns)
  invisible(TRUE)
}
# Unload Namespaces
#
# Attempts to unload namespaces in an order that avoids dependency issues
# based on data from \code{getNamespaceImports}
#
# Needs to be thought through a bit more in terms of how integrated it should
# be with the functions that use it.

unload_namespaces <- function(
  unload=loadedNamespaces(), global, keep.ns=union(
    getOption("unitizer.namespace.keep.base"),
    getOption("unitizer.namespace.keep")
  )
) {
  stopifnot(
    is.character(unload), all(!is.na(unload)),
    is.character(keep.ns) && !any(is.na(keep.ns)),
    is(global, "unitizerGlobal")
  )
  # We can't unload any namespaces associated with packages; packages must be
  # unloaded first

  search.path.pre <- search()
  search.path.pkg <- vapply(search.path.pre, is.loaded_package, logical(1L))
  search.path.pkg.names <-
    sub("^package:", "", search.path.pre[search.path.pkg])

  sp.depends <- unique(
    c(
      search.path.pkg.names,
      unlist(
        lapply(search.path.pkg.names, function(x) names(getNamespaceImports(x)))
  ) ) )
  unload <- setdiff(unload, sp.depends)

  # Check that none of the keep namespaces reference namespaces other than the
  # keep namespaces, and warn otherwise since we won't be able to unload /
  # re-load that one

  to.keep.depends <- unlist(
    lapply(
      keep.ns[keep.ns %in% loadedNamespaces()],
      function(x) names(getNamespaceImports(x))
  ) )
  # Since `getNamespaceImports` is supposed to be a bit experimental, make sure
  # that all namespaces we got are actually loaded, which they really should be

  if(!all(union(sp.depends, to.keep.depends) %in% loadedNamespaces()))
    # nocov start
    stop(
      "Internal Error: loaded namespace dependency calculation produced ",
      "non-loaded namespaces; this should not happen; contact maintainer."
    )
    # nocov end
  # Stuff left to unload

  unload.net <- setdiff(unload, c(keep.ns, to.keep.depends))

  # Now unload namespaces

  lns.raw <- loadedNamespaces()
  if(!all(unload.net %in% lns.raw))
    # nocov start
    stop(
      "Internal Error: attempting to unload namespaces that are not loaded; ",
      "contact maintainer."
    )
    # nocov end
  lns.tmp <- sapply(
    unload.net, function(x) unique(names(getNamespaceImports(x))),
    simplify=FALSE
  )
  # Get lib locations to unload DLLs later (if applicable)

  lib.locs <- vapply(
    unload.net,
    function(x) {
      if(inherits(loc <- try(find.package(x), silent=TRUE), "try-error")) {
        # nocov start
        warning(
          "Internal Warning: Unloading namespace \"", x, "\", but it does ",
          "not appear to have a corresponding installed package; if this",
          "warning persists please contact maintainer about it.",
          immediate.=TRUE
        )
        ""
        # nocov end
      } else loc
    },
    character(1L)
  )
  # Cycle through path attempting to unload namespaces until we cannot unload
  # any more.  This is not a particularly efficient algorithm, but should make
  # do for our purposes.  Really should create a dependency matrix, decrementing
  # dependencies as we unload them, and look for rows (or cols) with zero values
  # to find namespaces to unload

  lns.orig <- lns <- lns.tmp
  safety <- 0
  unloaded.success <- character(0L)
  repeat {
    if(safety <- safety + 1L > 1000)
      # nocov start
      stop(
        "Internal Error: namespace unloading not complete after 1000 iterations"
      )
      # nocov end
    lns.names <- names(lns)
    unloaded.try <- integer(0L)
    for(i in seq_along(lns)) {

      tar.ns <- names(lns)[[i]]
      if(!tar.ns %in% unlist(lns[-i])) {
        # No dependencies, so attempt to unload

        unloaded.try <- c(unloaded.try, i)
        attempt <- try(unloadNamespace(tar.ns))
        if(inherits(attempt, "try-error")) {
          # nocov start
          # no good way to test this
          warning(
            "Error while attempting to unload namespace `", tar.ns, "`",
            immediate.=TRUE
          )
          # nocov end
        } else {
          unloaded.success <- c(unloaded.success, tar.ns)
        }
      }
    }
    # Keep looping until length of remaining namespaces doesn't decrease anymore

    lns <- lns[-unloaded.try]
    if(!length(unloaded.try)) break
  }
  # Unload any dynamic libraries associated with the stuff we detached by
  # matching paths to what's in dynlibs

  dyn.lib.fnames <- vapply(.dynLibs(), "[[", character(1L), "path")
  dls <- sub("/libs/[^/].*$", "", dyn.lib.fnames)
  lib.locs.ul <- lib.locs[unloaded.success]
  dls.to.ul <-  lib.locs.ul[lib.locs.ul %in% dls]

  for(i in names(dls.to.ul)) library.dynam.unload(i, dls.to.ul[i])

  if(length(lns)) {
    # nocov start
    # no good way to test this
    warning(
      "Unable to unload the following namespaces: ",
      char_to_eng(sort(names(lns)), "", ""), immediate.=TRUE
    )
    # nocov end
  }
  # Warn if some namespaces could not be unloaded (likely due to dependency),
  # and register the conflict if we're tracking options

  if(
    (
      length(unload.conf <- which(unload %in% keep.ns)) ||
      length(lns)
    ) && global$status@options
  ) {
    global$ns.opt.conflict@conflict <- TRUE
    global$ns.opt.conflict@namespaces <- c(unload[unload.conf], names(lns))
    global$status@options <- 0L
    global$disabled@options <- TRUE
    global$state()  # mark state if we're not able to completely clean it up
  }
  NULL
}
# Check Whether a Package Is Loaded
#
# A package is considered loaded if it is in the search path and there is a
# namespace loaded with the same name as the package
#
# @keywords internal
# @param pkg.name character(1L) must be in format "package:pkgname"
# @return TRUE if it is a loaded package

is.loaded_package <- function(pkg.name) {
  if(!is.character(pkg.name) || length(pkg.name) != 1L)
    stop("Argument `pkg.name` must be character 1L")
  if(!isTRUE(grepl("^package:", pkg.name)))
    return(FALSE)
  just.name <- sub("^package:(.*)", "\\1", pkg.name)
  pkg.name %in% search() && just.name %in% loadedNamespaces()
}

# Path manipulation functions
#
# Reattaches a previously detached package to the search path

reattach <- function(pos, name, type, data, extra=NULL, global) {
  stopifnot(
    is.integer(pos), identical(length(pos), 1L), !is.na(pos), pos > 0L,
    is.chr1plain(name), !is.na(name),
    is.chr1plain(type), !is.na(type), type %in% c("package", "object"),
    is.environment(data),
    is(global, "unitizerGlobal")
  )
  if(identical(type, "package")) {
    suppressPackageStartupMessages(
      lib.try <- try(library(
        name, pos=pos, quietly=TRUE, character.only=TRUE,
        lib.loc=extra, warn.conflicts=FALSE
    ) ) )
    if(inherits(lib.try, "try-error")) {
      # nocov start
      warning(
        "Warning: unable to fully restore search path; see prior ",
        "error, and consult `?unitizerState`, searching for \"Caveats\". ",
        "Contact maintainer if your problem is not covered by documentation.",
        immediate.=TRUE
      )
      global$state()
      # nocov end
    }
  } else {
    base::attach(data, pos=pos, name=name, warn.conflicts=FALSE)
  }
}
# @keywords internal
# @rdname reattach
move_on_path <- function(new.pos, old.pos, global) {
  stopifnot(
    is.integer(new.pos), length(new.pos) == 1L, !is.na(new.pos),
    is.integer(old.pos), length(old.pos) == 1L, !is.na(old.pos),
    old.pos > new.pos,   # otherwise detaching old.pos would mess path up
    new.pos > 1L,        # can't attach at 1L
    is(global, "unitizerGlobal")
  )
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
  reattach(
    pos=new.pos, name=name.clean, type=type, data=obj, extra=extra,
    global=global
  )
}
# Make Unique IDs For Search Path Object
#
# adds ".1", ".2" etc if there are non-unique values, but first occurence is
# not modified so we can match between a list with duplicates and one without.
#
# This use to be for unitizerSearchData objects but that was way too slow to use
# in general search path tracking so had to switch to list.

setGeneric(
  # nocov start
  "unitizerUniqueNames", function(x, ...)
  StandardGeneric("unitizerUniqueNames")
  # nocov end
)
setMethod(
  "unitizerUniqueNames", "list",
  function(x, ...) {
    sp.id.base <- names(x$search.path)
    ave(
      sp.id.base, sp.id.base,
      FUN=function(x) {
        if(length(x) == 1L) return(x)
        c(head(x, 1L), paste0(tail(x, -1L), ".", 1:(length(x) - 1L)))
      }
    )
  }
)
## Generate An Identifier out of SP objects
##
## If this needs to be optimized look at `get_namespace_data` that is very
## similar but runs on `loadedNamespaces`.
##
## Not perfect, by any means, but necessary because we can't directly compare
## the search path environments as they change on detach / re-attach

get_package_data <- function() {
  sapply(
    search(),
    function(x) {
      loc <- if(grepl("^package:.+", x))
        try(path.package(sub("^package:(.*)", "\\1", x))) else ""
      ver <- try(getNamespaceVersion(x), silent=TRUE)
      list(
        names=x,
        lib.loc=if(!inherits(loc, "try-error")) loc else "",
        version=if(!inherits(ver, "try-error")) ver else ""
      )
    },
    simplify=FALSE
  )
}
# Helper function for loading options
#
# @param opts a list of options to look in for the relevant options

keep_ns_default <- function(opts=options()) {
  ns.opts <- c("unitizer.namespace.keep.base", "unitizer.namespace.keep")
  valid_sp_np_default(opts, ns.opts)
  keep.sp <- keep_sp_default(opts)
  keep.sp.ns <- sub("^package:", "", grep("^package:.+", keep.sp, value=TRUE))
  unique(c(unlist(opts[ns.opts]), keep.sp.ns))
}
keep_sp_default <- function(opts=options()) {
  sp.opts <- c("unitizer.search.path.keep.base", "unitizer.search.path.keep")
  valid_sp_np_default(opts, sp.opts)
  unique(unlist(opts[sp.opts]))
}
# Validation function shared by ns and sp funs

valid_sp_np_default <- function(opts, valid.names) {
  stopifnot(
    is.list(opts),
    is.character(valid.names) && !any(is.na(valid.names)),
    all(valid.names %in% names(opts)),
    all(
      vapply(
        opts[valid.names], function(x) is.character(x) && !any(is.na(x)),
        logical(1L)
  ) ) )
  invisible(TRUE)
}
