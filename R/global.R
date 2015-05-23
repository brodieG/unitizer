#' @include shims.R

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
  setwd=bsae::setwd
)
#' Structures For Tracking Global Options
#'
#' Immplemented as S4 classes just so we can ensure everything is guaranteed
#' to have the right slots.  Almost certainly no the best way to do this
#'
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalStatus",
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
    sn <- slotNames(object)
    if(!identical(sn, .unitizer.global.settings.names))
      stop(
        "Logic Error: all slots must be in `.unitizer.global.settings.names`; ",
        "contact maintainer"
      )
    for(i in slotNames(object))
      if(!is.TF(slot(object, i)))
        return(paste0("slot `", i, "` must be TRUE or FALSE"))
    TRUE
  }
)
setMethod("any", "unitizerGlobalStatus",
  function(..., na.rm=TRUE) {
    x <- list(...)
    if(!length(x)) stop("Must specify at least one `...` argument")
    if(length(x) > 1L) warning("Only first `...` argument is checked")
    vals <- vapply(slotNames(x), slot, logical(1L), object=x)
    any(vals, na.rm)
} )
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalSearch",
  contains="unitizerList",
  slots=c(detached.objects="list", search.curr="integer"),
  prototype=list(search.curr=0L),
  validity=function(object) {
    sc <- object@search.curr
    if(
      !length(sc) == 1L || is.na(sc) ||
      !(sc >= 0L && sc <= length(object)
    )
      return(
        paste0(
          "slot `search.curr` must be a scalar positive integer no larger than",
          " the number of items in `.items`"
      ) )
    TRUE
  }
)
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalTracking",
  slots=c(
    search.path="unitizerGlobalSearch",
    options="list",
    working.directory="list",
    par.env="list"               # not used, but present for code simplicity
  ),
  validity=function(object) {
    sn <- slotNames(object)
    if(!identical(sn, .unitizer.global.settings.names))
      stop(
        "Logic Error: all slots must be in `.unitizer.global.settings.names`; ",
        "contact maintainer"
      )
    TRUE
  }
)
setGeneric(
  "getTrackingValue",
  function(x, index, ...) standardGeneric("getTrackingValue")
)
#' Pull out Tracking Value
#'
#' Necessary mostly because the search path tracking value is not exactly what
#' you get by using \code{search}

setMethod(
  "getTrackingValue", c("list", "integer"), function(x, index, ...) x[[index]]
)
setMethod(
  "getTrackingValue", c("unitizerGlobalSearch", "integer"),
  function(x, index, ...) names(x[[index]])
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
    sn <- slotNames(object)
    if(!identical(sn, .unitizer.global.settings.names))
      stop(
        "Logic Error: all slots must be in `.unitizer.global.settings.names`; ",
        "contact maintainer"
      )
    TRUE
  }
)
#' @rdname global_structures
#' @keywords internal

setClass(
  "unitizerGlobalStatusCheckFuns",
  slots=c(
    search.path="function", options="function", working.directory="function",
    par.env="function"
  ),
  prototype=list(search.path=)
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

    shims="unitizerShims",
    status="unitizerGlobalStatus",
    tracking="unitizerGlobalTracking",

    indices.post.init="unitizerGlobalIndices",
    indices.last="unitizerGlobalIndices"
  ),
  methods(
    initialize=function(
      ...,
      par.env=new.env(parent=.GlobalEnv),
      options=list(options()),
      working.directory=list(getwd()),
      search.path=list()
    ) {
      if(!length(search.path)) {
        sp <- search()
        search.path <- list(
          setNames(ave(integer(length(sp)) + 1L, sp, FUN=cumsum), sp)
        )
      }
      callSuper(
        ..., par.env=par.env, options=options,
        working.directory=working.directory, search.path=search.path
    ) },
    enable=function(which=.unitizer.global.settings.names) {
      stopifnot(
        is.character(which), all(which %in% .unitizer.global.settings.names)
      )
      if(
        any(
          traced <- vapply(
            .unitizer.base.funs, inherits, logical(1L), "functionWithTrace"
        ) )
      ) {
        warning(
          "Cannot enable reproducible global mode because ",
          paste0("`", names(.unitizer.base.funs)[traced], "`", collapse=", "),
          " functions are already traced."
          immediate.=TRUE
        )
        return(invisible(FALSE))
      }
      one.shim <- FALSE # Track that at least one shim is set
      for(i in which) {
        res <- shim_funs(slot(shims, i))
        if(is(res, "unitizerShimFunList")) {
          slot(shims, i) <<- res
          slot(status, i) <<- TRUE
          one.shim <- TRUE
        }
      }
      invisible(TRUE)
    },
    disable=function(which=.unitizer.global.settings.names) {
      '
      Turn off global environment tracking
      '
      stopifnot(
        is.character(which), all(!is.na(which)),
        all(which %in% .unitizer.global.settings.names)
      )
        for(i in which) {
          if(slot(status, i)) unshim_funs(slot(shims, i))
          slot(status, i) <<- FALSE
          if(identical(i, "search.path")) {
             status@par.env <<- FALSE
             parent.env(par.env) <<- .GlobalEnv
          }
        }
        status
      }
    },
    checkState=function(which=.unitizer.global.settings.names) {
      '
      Verify that state actually is what we think it is
      '
      stopifnot(is.chr1(which), all(which %in% .unitizer.global.settings.names))
      for(i in slotNames(status)) {
        if(
          !identical(
            getTrackingValue(slot(tracking, i)),
            slot(state.funs, i)()
          )

      }
    },
    checkShims=function() {
      '
      Verify that shimming is still working, and if not disable tracking
      '
      if(!any(vapply(slotNames(status), slot(status, i), logical(1L))))
        return(TRUE)
      if(!tracingState()) {
        warning(
          "Tracing state off, so disabling reproducible modes", immediate.=TRUE
        )
        disable()
      } else {
        for(i in slotNames(shims)) {
          if(slot(status, i)) {
            shim.list <- slot(shims, i)
            shim.status <- vapply(
              as.list(shim.list),
              function(funDat) identical(getFun(funDat@name), funDat@fun.ref),
              logical(1L)
            )
            if(!all(shim.status)) {
              warning(
                "Shims compromised for ", shim.list@description, " so we are ",
                "disabling reproducible mode for it"
              )
              unshim_funs(shim.list)
              slot(status, i) <<- FALSE
              if(identical(i, "search.path") && status@par.env) {
                warning("Parent env reverted to .GlobalEnv", immediate.)
                status@par.env <<- FALSE
                  parent.env(.global$par.env) <- .GlobalEnv
      } } } } }
      return(invisible(TRUE))
    },
    reset=function(to) {
      '
      Reset global settings to a prior State, use 0L to set to the state just
      following pre-loads
      '
      stopifnot(is(to, "unitizerGlobalIndices"))

      if(status@search.path && to@search.path)
        search_path_update(to@search.path)
      if(status@options && to@options)
        .unitizer.base.funs$options(tracking@options[[to@options]])
      if(status@working.directory && to@working.directory)
        .unitizer.base.funs$setwd(
          tracking@working.directory[[to@working.directory]]
        )
      indices.last <<- to
      return(invisible(TRUE))
    },
    resetPostInit=function() {
      '
      Reset global settings to what they were right after initialization scripts
      '
      reset(indices.post.init)

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
    },
    bookmark=function() {
      '
      Mark current state as being interesting so we can easily restore to that
      state
      '
      bookmark <- new("unitizerGlobalIndices")
      for(i in slotNames(tracking))
        slot(bookmark, i) <- length(slot(tracking, i))
      indices.last <<- bookmark
      bookmark
    },
    bookmarkInit=function() {
      indices.post.init <<- bookmark()
      indices.post.init
    },
    state=function(what) {
      stopifnot(is.chr1(what), what %in% .unitizer.global.settings.names)

    }
) )
.global <- unitizerGlobal$new() # symbol used to host the `unitizerGlobal` object
