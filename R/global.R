#' @include is.R

NULL

.unitizer.global.settings.names <-
  c("search.path", "options", "working.directory", "par.env")

.unitizer.base.funs <- list(
  library=base::library,
  attach=base::attach,
  detach=base::detach,
  options=base::options,
  search=base::search,
  getwd=base::getwd,
  setwd=base::setwd
)
.unitizer.base.funs.to.shim <- c("library", "attach", "detach")


#' Get Current Search Path as List of Environments
#'
#' Internal utility function
#'
#' @keywords internal

search_as_envs <- function() {
  sp <- search()
  setNames(lapply(seq_along(sp), as.environment), sp)
}

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
    par.env="logical"
  ),
  prototype=list(
    search.path=FALSE, working.directory=FALSE, options=FALSE, par.env=FALSE
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
    par.env="list"               # not used, but present for code simplicity
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
    par.env="integer"
  ),
  prototype=list(search.path=0L, options=0L, working.directory=0L, par.env=0L),
  validity=function(object){
    for(i in slotNames(object))
      if(length(slot(object, i)) != 1L || slot(object, i) < 0L)
        return(paste0("slot `", i, "` must be integer(1L) and positive"))
    TRUE
  }
)
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalStateFuns", contains="unitizerGlobalBase",
  slots=c(
    search.path="function", options="function", working.directory="function",
    par.env="function"
  ),
  prototype=list(
    search.path=search_as_envs,
    options=options,
    working.directory=getwd,
    par.env=function() NULL
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

    state.funs="unitizerGlobalStateFuns",
    shim.funs="list",

    indices.init="unitizerGlobalIndices",
    indices.last="unitizerGlobalIndices"
  ),
  methods=list(
    initialize=function(
      ..., disabled=FALSE,
      par.env=new.env(parent=.GlobalEnv)
    ) {
      obj <- callSuper(..., par.env=par.env)
      state()
      obj
    },
    enable=function(which=.unitizer.global.settings.names) {
      '
      Turn on global environment tracking
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
        if(identical(i, "par.env")) parent.env(par.env) <<- as.environment(2L)
      }
      status
    },
    disable=function(which=.unitizer.global.settings.names) {
      '
      Turn off global settings; for `par.env` also unshims the functions used
      to enable tracking of topmost environment
      '
      stopifnot(
        is.character(which), all(!is.na(which)),
        all(which %in% .unitizer.global.settings.names)
      )
      for(i in which) {
        slot(status, i) <<- FALSE
        slot(disabled, i) <<- TRUE
        if(identical(i, "par.env")) {
          unshimFuns()
          parent.env(par.env) <<- .GlobalEnv
      } }
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
        ref.obj <- if(slot(indices.last, i)) slot(tracking, i)[[slot(indices.last, i)]]
        if(!identical(new.obj, ref.obj)) {
          slot(tracking, i) <<- append(slot(tracking, i), list(new.obj))
          if(identical(mode, "init")) {
            slot(indices.init, i) <<- length(slot(tracking, i))
          } else {
            slot(indices.last, i) <<- length(slot(tracking, i))
      } } }
      if(identical(mode, "init")) inidices.init else indices.last
    },
    reset=function(to) {
      '
      Reset global settings to a prior State
      '
      stopifnot(is(to, "unitizerGlobalIndices"))

      if(status@search.path && to@search.path)
        search_path_update(to@search.path)
      if(status@options && to@options)
        options(tracking@options[[to@options]])
      if(status@working.directory && to@working.directory)
        setwd(
          tracking@working.directory[[to@working.directory]]
        )
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
      Reset global settings to what they were on first load
      '
      reset(
        new(
          "unitizerGlobalIndices", search.path=1L, options=1L,
          working.directory=1L
      ) )
    }
) )

.global <- new.env()
# .global$global <- unitizerGlobal$new() # used purely for traced functions that need access to global object

setClassUnion("unitizerGlobalOrNULL", c("unitizerGlobal", "NULL"))
