#' @include is.R

NULL

.unitizer.global.settings.names <-
  c("search.path", "options", "working.directory", "random.seed")

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
#' \code{unitizerGlobal}.  The compressed versions obviously lose some
#' information.  In particular, environments or things that have environments
#' as parents, or large objects, are not stored and instead a reference to
#' a dummy environment is stored.  This environment is used to unambiguously
#' indicate a non-stored state component.
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
        deparse(.unitizer.global.settings.names, width=500)
      )
    TRUE
} )

setClass(
  "unitizerGlobalStatus", contains="unitizerGlobalBase",
  slots=c(
    search.path="logical",
    options="logical",
    working.directory="logical",
    random.seed="logical"
  ),
  prototype=list(
    search.path=FALSE, working.directory=FALSE, options=FALSE,
    random.seed=FALSE
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
    random.seed="list"
  )
)
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalTrackingStore", contains="unitizerGlobalTracking",
  slots=c(dummy="environment")
)
setGeneric(
  "unitizerCompressTracking",
  function(x, ...) standardGeneric("unitizerCompressTracking")
)
setMethod(
  "unitizerCompressTracking", "unitizerGlobalTracking",
  function(x, ...) {
    res <- new("unitizerGlobalTrackingStore")
    res@search.path <- lapply(x@search.path, names)
    res@options <- lapply(
      x@options,
      function(y) {
        if(
          !is.null(environment(x)) || is.environment(x) ||
          object.size(y) > 1000
        )
          res@dummy else y
      }
    )
    res@working.directory <- x@working.directory  # not sure whether this is something that we want to be comparing
    res@random.seed <- lapply(  # this could be big!!!
      x@random.seed,
      function(y) if(length(y) > 10L) res@dummy else y
    )
  res
} )
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalIndices",
  slots=c(
    search.path="integer",
    options="integer",
    working.directory="integer",
    random.seed="integer"
  ),
  prototype=list(
    search.path=0L, options=0L, working.directory=0L, random.seed=0L
  ),
  validity=function(object){
    for(i in slotNames(object))
      if(length(slot(object, i)) != 1L || slot(object, i) < 0L)
        return(paste0("slot `", i, "` must be integer(1L) and positive"))
    TRUE
  }
)

#' Get Current Search Path as List of Environments
#'
#' Internal utility function.  Loaded namespaces attached as an attribute.
#' Probably should be an S4 class.
#'
#' This has to be in this file, and not in R/search.R for teh setClass for the
#' state funs object.
#'
#' @keywords internal

search_as_envs <- function() {
  sp <- search()
  res <- setNames(lapply(seq_along(sp), as.environment), sp)
  attr(res, "loadedNamespaces") <- get_namespace_data()
  res
}

#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalStateFuns", contains="unitizerGlobalBase",
  slots=c(
    search.path="function", options="function", working.directory="function",
    random.seed="function"
  ),
  prototype=list(
    search.path=search_as_envs,
    options=options,
    working.directory=getwd,
    random.seed=function()
      mget(
        ".Random.seed", envir=.GlobalEnv, inherits=FALSE, ifnotfound=list(NULL)
      )[[1L]]
  )
)
#' Objects / Methods used to Track Global Settings and the Like
#'
#' Implemented as Reference Class
#'
#' @keywords internal

unitizerGlobal <- setRefClass(
  "unitizerGlobal",
  fields=list(
    par.env="environment",

    status="unitizerGlobalStatus",
    disabled="unitizerGlobalStatus",
    tracking="unitizerGlobalTracking",

    unitizer.opts="list",   # store original unitizer options before they get zeroed out

    state.funs="unitizerGlobalStateFuns",
    shim.funs="list",

    indices.init="unitizerGlobalIndices",
    indices.last="unitizerGlobalIndices",

    namespaces.loaded="logical"  # track which of the keep namespaces started off loaded
  ),
  methods=list(
    initialize=function(
      ..., disabled=FALSE, enable.which=.unitizer.global.settings.names,
      par.env=new.env(parent=.GlobalEnv)
    ) {
      obj <- callSuper(..., par.env=par.env)
      enable(enable.which)
      state()
      .global$global <- .self  # top level copy for access from other namespaces
      namespaces.loaded <<-
        unitizer.opts[["unitizer.namespace.keep"]] %in% loadedNamespaces()
      obj
    },
    enable=function(which=.unitizer.global.settings.names) {
      '
      Turn on global environment tracking, shouldnt be needed since usually
      called during initialization
      '
      stopifnot(
        is.character(which), all(which %in% .unitizer.global.settings.names)
      )
      for(i in which) {
        if(slot(disabled, i)) {
          warning(
            "Reproducible setting for `", i, "` has already been disabled and ",
            "cannot be re-enabled", immediate.=TRUE
          )
          next
        }
        slot(status, i) <<- TRUE
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
        slot(status, i) <<- FALSE
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
        if(!slot(status, i)) next             # Don't record statuses that aren't being tracked
        new.obj <- slot(state.funs, i)()      # Get state with pre-defined function
        ref.obj <- if(slot(indices.last, i))
          slot(tracking, i)[[slot(indices.last, i)]] else
            new.env()  # this can't possibly be identical to anything other than itself
        if(!identical(new.obj, ref.obj))
          slot(tracking, i) <<- append(slot(tracking, i), list(new.obj))
        if(identical(mode, "init"))
          slot(indices.init, i) <<- length(slot(tracking, i))
        slot(indices.last, i) <<- length(slot(tracking, i))
      }
      indices.last
    },
    reset=function(to, force=FALSE) {
      '
      Reset global settings to a prior State, `force` is typically used When
      attempting to do a best effort reset with an on.exit reset b/c there
      was a failure or some such elsewhere, particularly within an actual
      non full reset attempt.
      '
      stopifnot(is(to, "unitizerGlobalIndices"))

      if(status@search.path && to@search.path)
        search_path_update(to@search.path, .self, force=force)
      if(status@options && to@options)
        options(tracking@options[[to@options]])
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
          working.directory=1L, random.seed=1L
        ), force=TRUE
      )
    }
) )

.global <- new.env()
# .global$global <- unitizerGlobal$new() # used purely for traced functions that need access to global object

setClassUnion("unitizerGlobalOrNULL", c("unitizerGlobal", "NULL"))
