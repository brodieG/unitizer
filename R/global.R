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

setClass("unitizerDummy", slots=c(.="NULL"))

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
  ),
  prototype=list(.dummy=new.env(parent=baseenv()))
)
#' Compare State Between Reference and New Tests
#'
#' Designed to compare the state snapshots as stored and recorded in a
#' \code{unitizer}.  These approximate various aspects of global state.
#' The approximation is because state objects that are too large are not kept,
#' so we are not always certain whether some parts of state changed or not.
#'
#' We report several levels of differences between state values:
#' \itemize{
#'    \item affirmative difference, when we know with certainty the state values
#'      are different
#'    \item probable difference, when we have one known value and one unknown
#'      value, typically because one was too large to store
#'    \item possible difference, when both values are unknown
#' }
#' Additionally, it is possible that the state tracking settings change between
#' \code{unitizer} runs.  If that happens, then:
#' \itemize{
#'   \item if state type is not tracked in new (current), then we assume state
#'     is unchanged
#'   \item if state is tracked in new, but was not in reference (target), then
#'     we report a probable difference
#' }
#' @export

setMethod(
  "all.equal",
  c("unitizerGlobalState", "unitizerGlobalState"),
  function(target, current, verbose=TRUE, strict=FALSE, ...) {
    stopifnot(is.TF(verbose), is.TF(strict))
    valid.diff <- 0L:3L
    err.msgs <- paste0(c("possible ", "likely ", "known "), "differences")
    deltas <- setNames(
      vector("list", length(.unitizer.global.settings.names)),
      .unitizer.global.settings.names
    )
    for(i in .unitizer.global.settings.names) {
      tar <- slot(target, i)
      cur <- slot(current, i)
      msg.header <- sprintf("`%s` state mismatch:", i)

      deltas[[i]] <- if(is.null(tar) && !is.null(cur)) {
        paste(msg.header, "reference state not recorded")
      } else if(is.null(cur)) {
        NULL
      } else {
        if(identical(i, "options")) {
          # Compare common options with state_item_compare, all others are
          # assumed to be different; note that system and asis options should
          # not be part of these lists

          common.opts <- intersect(names(tar), names(cur))
          deltas.opts <- unlist(
            Map(
              state_item_compare, tar[common.opts], cur[common.opts]
          ) )
          mismatch.opts <- c(
            setdiff(names(tar), names(cur)), setdiff(names(cur), names(tar))
          )
          deltas.opts <- c(
            deltas.opts, setNames(rep(3L, length(mismatch.opts)), mismatch.opts)
          )
          if(any(deltas.opts > !strict)) {
            deltas.split <- split(names(deltas.opts), deltas.opts)
            deltas.by.type <- unlist(  # unlist drops any NULL values so we get rid of "0"
              lapply(
                sort(names(deltas.split), decreasing=TRUE),
                function(x) {
                  if(x %in% c("0", if(!strict) "1")) return(NULL)
                  paste0(
                    err.msgs[[as.integer(x)]], " for option",
                    if(length(deltas.split[[x]]) > 1L) "s", " ",
                    paste0(sprintf("\"%s\"", deltas.split[[x]]), collapse=", ")
            ) } ) )
            # Bulleted list of more than one type of option delta

            if(length(deltas.by.type) > 1L) {
              c(msg.header, as.character(UL(deltas.by.type)))
            } else paste(msg.header, deltas.by.type)
          }
        } else {
          # States other than options that don't need to be compared element
          # for element

          state.diff <- state_item_compare(tar, cur)
          if(state.diff)
            paste(
              msg.header,
              if(identical(state.diff, 3L)) {
                aq <- all.equal(tar, cur)
                if(isTRUE(aq) || !is.character(aq)) {
                  err.msgs[[state.diff]]
                } else {
                  if(length(aq) > 1L) paste0(aq[[1L]], "...") else aq
                }
              } else err.msgs[[state.diff]]
            )
    } } }
    # Finalize

    if(length(deltas)) {
      res <- vapply(deltas, paste0, character(1L), collapse="\n")
      if(verbose) word_cat(res, sep="\n")
      invisible(res)
    } else TRUE
} )
# Helper function for all.equal

state_item_compare <- function(tar, cur) {
  tar.dum <- is(tar, "unitizerDummy")
  cur.dum <- is(cur, "unitizerDummy")
  if(tar.dum && cur.dum) 1L else if (tar.dum || cur.dum) 2L else {
    if(identical(tar, cur)) 0L else 3L
} }
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
      function(x, y) unlist(x[y]),
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
    res@search.path <- lapply(x@search.path, names)

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
        ), force=TRUE
      )
    }
) )

.global <- new.env()
# .global$global <- unitizerGlobal$new() # used purely for traced functions that need access to global object

setClassUnion("unitizerGlobalOrNULL", c("unitizerGlobal", "NULL"))
