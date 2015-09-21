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
        deparse(.unitizer.global.settings.names, width=500)
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
    random.seed="integer"
  ),
  prototype=list(
    search.path=0L, working.directory=0L, options=0L,
    random.seed=0L
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

setClass("unitizerDummy", slots=c(.="NULL"))
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
    random.seed="integerOrNULLOrDummy"
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
    res@search.path <- lapply(x@search.path, unitizerCompressTracking)

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
# Get Current Search Path as List of Environments
#
# This has to be in this file, and not in R/search.R for the setClass for the
# state funs object.  Note there are some weird dependency circularities,
# and we're relying on this function not being called until once the full
# package is loaded.

search_as_envs <- function() {
  sp <- search()
  res <- setNames(lapply(seq_along(sp), as.environment), sp)

  new(
    "unitizerSearchData",
    .items=res,
    ns.dat=new("unitizerNsListData", .items=get_namespace_data())
) }
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
    ns.opt.conflict="unitizerGlobalNsOptConflict",    # Allow us to remember if an error happened on state reset

    unitizer.opts="list",   # store original unitizer options before they get zeroed out

    state.funs="unitizerGlobalStateFuns",
    shim.funs="list",

    indices.init="unitizerGlobalIndices",
    indices.last="unitizerGlobalIndices"
  ),
  methods=list(
    initialize=function(
      ..., disabled=FALSE, enable.which=integer(0L),
      par.env=new.env(parent=baseenv()),
      unitizer.opts=options()[grep("^unitizer\\.", names(options()))]
    ) {
      obj <- callSuper(..., par.env=par.env, unitizer.opts=unitizer.opts)
      enable(enable.which)
      state()
      ns.opt.conflict@conflict <<- FALSE
      .global$global <- .self  # top level copy for access from other namespaces
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
    reset=function(to) {
      '
      Reset global settings to a prior State
      '
      stopifnot(is(to, "unitizerGlobalIndices"))

      if(status@search.path && to@search.path)
        search_path_update(to@search.path, .self)
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
          working.directory=1L, random.seed=1L
        )
      )
    }
) )
 # used purely for traced functions that need access to global object; in most
 # cases should be just our traced functions, note that we just create this
 # object here for test; any time a `unitizer` is instantiated

.global <- new.env()

# not necessary since any use of `shimFuns` can only occur if a unitizerGlobal
# object is instantiated, so $global will have been created by initialize method

# .global$global <- unitizerGlobal$new()

setClassUnion("unitizerGlobalOrNULL", c("unitizerGlobal", "NULL"))
