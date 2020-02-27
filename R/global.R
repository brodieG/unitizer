# Copyright (C) 2020  Brodie Gaslam
# 
# This file is part of "unitizer"
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

#' @include is.R
#' @include capture.R

NULL

.loaded <- FALSE   # gets set by .onLoad

.unitizer.global.settings.names <-
  c("search.path", "options", "working.directory", "random.seed", "namespaces")

#' Structures For Tracking Global Options
#'
#' Immplemented as S4 classes just so we can ensure everything is guaranteed
#' to have the right slots.  This is done by defining a virtual class that has
#' a validity function that checks the required slots exist.
#'
#' Not we don't use "ANY" slots here because that would allow partially
#' specified sub classes (i.e. classes with slots that are "ANY"), which we
#' do not want to allow.
#'
#' \code{unitizerGlobalTrackingStore} is used to keep "compressed" versions of
#' \code{unitizerGlobal$tracking}.  The compressed versions obviously lose some
#' information.  In particular, environments or things that have environments
#' as parents, or large objects, are not stored and instead a reference to
#' a \code{unitizerDummy} object is stored.  This object unambiguously
#' identifies a non-stored object since no user or system code should
#' normally creating a \code{unitizerDummy} object.
#'
#' \code{unitizerGlobalState} tracks a single state which is just one value from
#' each of the slots of \code{unitizerGlobalTrackingStore}
#'
#' When comparing state between new and reference tests, only explicitly stored
#' items are compared (though any extra or missing items may be brought up as
#' possible mismatches).
#'
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalBase", contains="VIRTUAL",
  validity=function(object)
    if(!identical(slotNames(object), .unitizer.global.settings.names)) {
      paste0(
        "Invalid global object, slots must be ",
        deparse(.unitizer.global.settings.names, width.cutoff=500L)
      )
    TRUE
} )
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalStatus", contains="unitizerGlobalBase",
  slots=c(
    search.path="integer",
    options="integer",
    working.directory="integer",
    random.seed="integer",
    namespaces="integer"
  ),
  prototype=list(
    search.path=0L, working.directory=0L, options=0L,
    random.seed=0L, namespaces=0L
  ),
  validity=function(object) {
    for(i in slotNames(object))
      if(!is.int.1L(slot(object, i)) || !slot(object, i) %in% 0L:2L)
        return(paste0("slot `", i, "` must be integer(1L) and in 0:2"))
    TRUE
  }
)
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalDisabled", contains="unitizerGlobalBase",
  slots=c(
    search.path="logical",
    options="logical",
    working.directory="logical",
    random.seed="logical",
    namespaces="logical"
  ),
  prototype=list(
    search.path=FALSE, working.directory=FALSE, options=FALSE,
    random.seed=FALSE, namespaces=FALSE
  ),
  validity=function(object) {
    for(i in slotNames(object)) if(!is.TF(slot(object, i)))
      return(paste0("slot `", i, "` must be TRUE or FALSE"))
    TRUE
  }
)
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalTracking", contains="unitizerGlobalBase",
  slots=c(
    search.path="list",
    options="list",
    working.directory="list",
    random.seed="list",
    namespaces="list"
  )
)
#' @rdname global_structures
#' @keywords internal

setClass("unitizerDummy", slots=c(.="NULL"))

#' @rdname unitizer_s4method_doc

setMethod(
  "show", "unitizerDummy", function(object) cat("<object not recorded>\n")
)
setClassUnion("listOrNULLOrDummy", c("list", "NULL", "unitizerDummy"))
setClassUnion("characterOrNULLOrDummy", c("character", "NULL", "unitizerDummy"))
setClassUnion("integerOrNULLOrDummy", c("integer", "NULL", "unitizerDummy"))

#' @rdname global_structures
#' @keywords internal

setClass("unitizerGlobalTrackingStore", contains="unitizerGlobalTracking")

#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalState", contains="unitizerGlobalBase",
  slots=c(
    search.path="characterOrNULLOrDummy",
    options="listOrNULLOrDummy",
    working.directory="characterOrNULLOrDummy",
    random.seed="integerOrNULLOrDummy",
    namespaces="characterOrNULLOrDummy"
  )
)
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalIndices",
  slots=c(
    search.path="integer",
    options="integer",
    working.directory="integer",
    random.seed="integer",
    namespaces="integer"
  ),
  prototype=list(
    search.path=0L, options=0L, working.directory=0L, random.seed=0L,
    namespaces=0L
  ),
  validity=function(object){
    for(i in slotNames(object))
      if(length(slot(object, i)) != 1L || slot(object, i) < 0L)
        return(paste0("slot `", i, "` must be integer(1L) and positive"))
    TRUE
  }
)
#' @rdname unitizer_s4method_doc

setMethod(
  "as.integer", "unitizerGlobalIndices",
  function(x, ...) {
    s.n <- slotNames(x)
    res <- setNames(unlist(lapply(s.n, slot, object=x)), s.n)
    if(!is.integer(res)) {
      # nocov start
      stop(
        "Internal Error: unable to convert `unitizerGlobalIndices` object to ",
        "integer; contact maintainer."
      )
      # nocov end
    }
    res
} )
# Create a `unitizerGlobalIndices` object that points to the last stored states;
# used primarily so we can then add more states to the ends and can compute what
# the indices for the added states should be; used by `mergeStates`

setGeneric(
  "unitizerStateMaxIndices",
  function(x, ...) standardGeneric("unitizerStateMaxIndices")
)
setMethod("unitizerStateMaxIndices", c("unitizerGlobalTrackingStore"),
  function(x, ...) {
    last.ids <- Map(function(y) length(slot(x, y)), slotNames(x))
    do.call("new", c(list("unitizerGlobalIndices"), last.ids))
} )
# Pull out a single state from a tracking object

setGeneric(
  "unitizerGlobalStateExtract",
  function(x, y, ...) standardGeneric("unitizerGlobalStateExtract")
)
setMethod(
  "unitizerGlobalStateExtract",
  c("unitizerGlobalTrackingStore", "unitizerGlobalIndices"),
  function(x, y, ...) {
    vals <- Map(
      function(x, y) unlist(x[y], recursive=FALSE),
      sapply(.unitizer.global.settings.names, slot, object=x, simplify=FALSE),
      lapply(.unitizer.global.settings.names, slot, object=y)
    )
    do.call("new", c("unitizerGlobalState", vals))
} )
# Reduce size of a tracking object for storage purposes

setGeneric(
  "unitizerCompressTracking",
  function(x, ...) standardGeneric("unitizerCompressTracking")
)
setMethod(
  "unitizerCompressTracking", "unitizerGlobalTracking",
  function(x, opts.ignore, ...) {
    stopifnot(is.character(opts.ignore))
    res <- new("unitizerGlobalTrackingStore")
    res@search.path <- lapply(x@search.path, compress_search_data)
    res@namespaces <- lapply(x@namespaces, compress_ns_data)

    # Don't store stuff with environments or stuff that is too big
    # (size cut-off should be an option?), or stuff that is part of the base or
    # as.is options

    res@options <- Map(
      x@options,
      f=function(i) {
        Map(
          setdiff(names(i), opts.ignore),
          f=function(j)
            if(
              !is.null(environment(i[[j]])) || is.environment(i[[j]]) ||
              object.size(i[[j]]) > 1000
            ) new("unitizerDummy") else i[[j]]
    ) } )
    res@working.directory <- x@working.directory  # not sure whether this is something that we want to be comparing
    res@random.seed <- lapply(  # this could be big!!!
      x@random.seed,
      function(y) if(length(y) > 10L) new("unitizerDummy") else y
    )
  res
} )
# Get Current Search Path And Namespace Data
#
# These have to be in this file, and not in R/search.R for the setClass for the
# state funs object.  Note there are some weird dependency circularities,
# and we're relying on this function not being called until once the full
# package is loaded.
#
# Also, while we track namespaces separately, we need to store them here as well
# as they contain the the package version info we ultimately want to retain;
# this could possibly be improved by just attaching package version, but too
# much work to retrofit existing behavior that blended namespaces and
# search path

search_as_envs <- function() {
  sp <- search()
  res <- setNames(lapply(seq_along(sp), as.environment), sp)

  list(search.path=res, ns.dat=get_namespace_data())
}
# Accessing namespace info in not really documented manner, but muuuch faster
# than using getNamespaceInfo

get_namespace_data <- function() {
  sapply(
    loadedNamespaces(),
    function(x) {
      ns <- getNamespace(x)
      loc <- ns[[".__NAMESPACE__."]][["path"]]
      ver <- ns[[".__NAMESPACE__."]][["spec"]]["version"]
      if(is.null(ver)) ver <- ""
      if(is.null(loc)) loc <- ""
      list(names=x, lib.loc=loc, version=ver)
    },
    simplify=FALSE
  )
}
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalStateFuns", contains="unitizerGlobalBase",
  slots=c(
    search.path="function", options="function", working.directory="function",
    random.seed="function", namespaces="function"
  ),
  prototype=list(
    search.path=search_as_envs,
    options=options,
    working.directory=getwd,
    random.seed=function()
      mget(
        ".Random.seed", envir=.GlobalEnv, inherits=FALSE, ifnotfound=list(NULL)
      )[[1L]],
    namespaces=get_namespace_data
  )
)
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalNsOptConflict",
  slots=c(conflict="logical", namespaces="character", file="character"),
  prototype=list(conflict=FALSE),
  validity=function(object) {
    if(!is.TF(conflict)) return("Slot `conflict` must be TRUE or FALSE")
    if(!is.chr1(file)) return("Slot `file` must be character(1L) and not NA")
    if(any(is.na(namespaces))) return("Slot `namespaces` may not contain NAs")
    TRUE
  }
)
# Objects / Methods used to Track Global Settings and the Like
#
# Implemented as Reference Class

unitizerGlobal <- setRefClass(
  "unitizerGlobal",
  fields=list(
    par.env="environment",

    status="unitizerGlobalStatus",
    disabled="unitizerGlobalDisabled",
    tracking="unitizerGlobalTracking",

    # Implement global object as locked so that it doesn't get overwritten

    locked="logical",
    set.global="logical",

    # Allow us to remember if an error happened on state reset

    ns.opt.conflict="unitizerGlobalNsOptConflict",
    cons="unitizerCaptConsOrNULL",   # Connections for stdout and stderr capture

    # store original unitizer options before they get zeroed out

    unitizer.opts="list",

    state.funs="unitizerGlobalStateFuns",
    shim.funs="list",

    indices.init="unitizerGlobalIndices",
    indices.last="unitizerGlobalIndices"
  ),
  methods=list(
    initialize=function(
      ..., disabled=FALSE, enable.which=integer(0L),
      par.env=new.env(parent=baseenv()),
      unitizer.opts=options()[grep("^unitizer\\.", names(options()))],
      set.global=FALSE
    ) {
      obj <- callSuper(
        ..., par.env=par.env, unitizer.opts=unitizer.opts,
        locked=FALSE
      )
      enable(enable.which)
      state()
      ns.opt.conflict@conflict <<- FALSE

      # top level copy for access from other namespaces

      if(isTRUE(.global$global$locked)) {
        # nocov start
        stop(
          "Internal Error: global tracking object already exists; this should ",
          "never happen; contact maintainer"
        )
        # nocov end
      } else if(set.global) {
        # no longer 100% sure, but I believe the .global is used solely so that
        # we can have access to the special unitizer parent environment from the
        # traced functions (i.e. so we can change the parent when the search
        # path changes)
        .global$global <- .self
        locked <<- TRUE
      } else if(.loaded) {
        warning(
          "Instantiated global object without global namespace registry; ",
          "you should only see this warning you are using ",
          "`repair_environments`.", immediate.=TRUE
        )
      }
      obj
    },
    enable=function(
      which=setNames(
        rep(2L, length(.unitizer.global.settings.names)),
        .unitizer.global.settings.names
      )
    ) {
      '
      Turn on global environment tracking, shouldnt be needed since usually
      called during initialization
      '
      if(!length(which)) return(status)
      stopifnot(
        is.integer(which), !any(is.na(which)),
        !is.null(names(which)) && !any(is.na(names(which))),
        all(names(which) %in% .unitizer.global.settings.names),
        length(which) == length(unique(names(which)))
      )
      for(i in names(which)) {
        if(slot(disabled, i)) {
          warning(
            "State setting for `", i, "` has already been disabled and ",
            "cannot be re-enabled", immediate.=TRUE
          )
          next
        }
        slot(status, i) <<- which[[i]]
      }
      status
    },
    disable=function(which=.unitizer.global.settings.names) {
      '
      Turn off global settings; for `par.env` also unshims the functions used
      to enable tracking of topmost environment.

      Currently not truly needed but left in for future use since the enable/
      disable paradigm is not as important now that `par.env` is being handled
      separately
      '
      stopifnot(
        is.character(which), all(!is.na(which)),
        all(which %in% .unitizer.global.settings.names)
      )
      for(i in which) {
        slot(status, i) <<- 0L
        slot(disabled, i) <<- TRUE
      }
      status
    },
    state=function(mode="normal") {
      '
      Record state for each of the globals we are tracking; one question here is
      whether we want more sophisticated checking against existing settings to
      avoid repeatedly storing the same thing.  For now we just check against
      the last one
      '
      stopifnot(is.chr1(mode), mode %in% c("normal", "init"))

      for(i in slotNames(tracking)) {
        # Don't record statuses that aren't being tracked
        if(!slot(status, i)) next
        # Get state with pre-defined function
        new.obj <- slot(state.funs, i)()
        ref.obj <- if(slot(indices.last, i)) {
          slot(tracking, i)[[slot(indices.last, i)]]
        } else {
          new.env()  # guaranteed unique
        }
        if(!identical(new.obj, ref.obj)) {
          slot(tracking, i) <<- append(slot(tracking, i), list(new.obj))
          slot(indices.last, i) <<- length(slot(tracking, i))
        }
        if(identical(mode, "init")) {
          slot(indices.init, i) <<- length(slot(tracking, i))
          slot(indices.last, i) <<- length(slot(tracking, i))
        }
      }
      indices.last
    },
    reset=function(to) {
      '
      Reset global settings to a prior State
      '
      stopifnot(is(to, "unitizerGlobalIndices"))

      if(status@search.path && to@search.path){
        search_path_update(to@search.path, .self)
      }
      if(status@namespaces && to@namespaces)
        namespace_update(to@namespaces, .self)
      if(status@options && to@options)
        options_update(tracking@options[[to@options]])
      if(status@working.directory && to@working.directory)
        setwd(
          tracking@working.directory[[to@working.directory]]
        )
      if(status@random.seed && to@random.seed) {
        if(is.null(tracking@random.seed[[to@random.seed]])) {
          if(exists(".Random.seed", .GlobalEnv, inherits=FALSE))
            rm(".Random.seed", envir=.GlobalEnv)
        } else {
          assign(
            ".Random.seed", tracking@random.seed[[to@random.seed]], .GlobalEnv
      ) } }
      indices.last <<- to
      indices.last
    },
    resetInit=function() {
      '
      Reset global settings to what they were right after initialization scripts
      '
      reset(indices.init)

    },
    resetFull=function() {
      '
      Reset global settings to what they were on first load; par.env doesnt
      matter since only meaningful in context of unitizer
      '
      reset(
        new(
          "unitizerGlobalIndices", search.path=1L, options=1L,
          working.directory=1L, random.seed=1L, namespaces=1L
        )
      )
    },
    release=function() {
      '
      Blow away the global tracking object so that we can re-use for other
      sessions
      '
      locked <<- FALSE
    }
) )
# used purely for traced functions that need access to global object; in most
# cases should be just our traced functions, note that we just create this
# object here for test; any time a `unitizer` is instantiated.  UPDATE: seems
# this is just used to get access to the special `unitizer` parent env, and to
# implement the non-interactive prompt responses for testing.

.global <- new.env()

# not necessary since any use of `shimFuns` can only occur if a unitizerGlobal
# object is instantiated, so $global will have been created by initialize method

# .global$global <- unitizerGlobal$new()

setClassUnion("unitizerGlobalOrNULL", c("unitizerGlobal", "NULL"))
