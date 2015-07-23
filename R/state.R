#' @include global.R

NULL

.unitizer.valid.state.abbr <-  c("pristine", "noopt", "basic", "off")

#' Interface For State Control
#'
#' You can use these classes for detailed control of how \code{unitizer} tracks
#' and modifies state during test evaluation and review.  These can be passed
#' as the \code{state} argument to \code{\link{unitize}} and
#' \code{\link{unitize_dir}}.
#'
#' There are four classes defined, though their only purpose is to act as
#' presets since they are identical except for their prototype values:
#' \itemize{
#'   \item \code{unitizerStatePristine} is the default class and has implements
#'     the highest level of state tracking and control
#'   \item \code{unitizerStateNoOpt} turns off options tracking, which makes it
#'     a good choice if you have a lot of packages that are added to
#'     \code{getOption("unitizer.namespace.keep")} so that you do not need to
#'     also add all their options to \code{getOption("unitizer.opts.asis")}
#'   \item \code{unitizerStateBasic} keeps all tracking, but at a less aggressive
#'     level; state is reset between each test file to the state before
#'     you started \code{unitize}ing so that no single test file affects
#'     another, but the state of your workspace, search path, etc. will affect
#'     all the tests
#'   \item \code{unitizerStateOff} state tracking is turned off
#' }
#' In addition to the preset classes, you can set any of the slots to any valid
#' setting (see examples).
#'
#' @note \code{\link{unitize_dir}} and \code{\link{unitize}} can accept
#'   character values instead of the classes here; these are just translated to
#'   the corresponding class defined here
#' @examples
#' \dontrun{
#' ## use a custom environment as parent env
#' my.env <- new.env()
#' unitize(..., state=new("unitizerStatePrisitine", par.env=my.env))
#' ## Basic, but do not track options
#' unitize(..., state=new("unitizerStateBasic", options=0))
#' }
#'
#' @rdname state

setClass(
  "unitizerState",
  slots=c(
    search.path="integer",
    options="integer",
    working.directory="integer",
    random.seed="integer",
    par.env="environmentOrNULL"
  ),
  prototype=list(
    search.path=0L, options=0L, working.directory=0L, random.seed=0L
  ),
  validity=function(object) {
    # seemingly superflous used to make sure this object is in concordance with
    # the various others that are similar
    if(
      !identical(
        slotNames(object),
        c(.unitizer.global.settings.names, "par.env")
      )
    )
      paste0(
        "Invalid state object, slots must be ",
        deparse(.unitizer.global.settings.names, width=500)
      )
    for(i in .unitizer.global.settings.names) {
      slot.val <- slot(object, i)
      if(
        !is.integer(slot.val) || !length(slot.val) == 1L || is.na(slot.val) ||
        !slot.val %in% 0L:2L
      )
        return(paste0("Slot `", i, "` must be integer(1L) and in 0:2"))
    }
    if(
      identical(object@options, 2L) &&
      !identical(object@search.path, 2L)
    )
      return(
        paste0(
          "Argument `reproducible.state` has an invalid state: 'options' is set ",
          "to 2, but 'search.path' is not"
      ) )
    if(identical(object@random.seed, 2L)) {
      prev.seed <- mget(
        ".Random.seed", envir=.GlobalEnv, ifnotfound=list(NULL)
      )[[1L]]
      seed.dat <- getOption("unitizer.seed")
      msg <- ""
      if(inherits(try(do.call(set.seed, seed.dat)), "try-error")) {
        msg <- paste0(
          "Unable to set random seed; make sure `getOption('unitizer.seed')` ",
          "is a list of possible arguments to `set.seed`, or set `seed` slot ",
          "to be less than 2L."
      ) }
      if(is.null(prev.seed) && exists(".Random.seed", envir=.GlobalEnv))
        rm(".Random.seed", envir=.GlobalEnv) else
          assign(".Random.seed", prev.seed, envir=.GlobalEnv)
      if(nchar(msg)) return(msg)
    }
    TRUE
  }
)
setMethod("initialize", "unitizerState",
  function(.Object, ...) {
    dots <- list(...)
    dots.base <- dots[!names(dots) %in% "par.env"]
    for(i in names(dots.base))
      if(is.numeric(dots.base[[i]])) dots[[i]] <- as.integer(dots.base[[i]])
    do.call(callNextMethod, c(.Object, dots))
} )

setClass(
  "unitizerStatePristine", contains="unitizerState",
  prototype=list(
    search.path=2L, options=2L, working.directory=2L, random.seed=2L,
    par.env=NULL
  )
)
#' @rdname state

setClass(
  "unitizerStateNoOpt", contains="unitizerState",
  prototype=list(
    search.path=2L, options=0L, working.directory=2L, random.seed=2L,
    par.env=.GlobalEnv
  )
)
#' @rdname state

setClass(
  "unitizerStateBasic", contains="unitizerState",
  prototype=list(
    search.path=1L, options=1L, working.directory=1L, random.seed=1L,
    par.env=NULL
  )
)
#' @rdname state

setClass(
  "unitizerStateOff", contains="unitizerState",
  prototype=list(
    search.path=0L, options=0L, working.directory=0L, random.seed=0L,
    par.env=.GlobalEnv
  )
)

