# Copyright (C) 2022 Brodie Gaslam
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

#' @include global.R

NULL

.unitizer.valid.state.abbr <-
  c("pristine", "recommended", "suggested", "basic", "off", "safe")

#' Tests and Session State
#'
#' While R generally adheres to a "functional" programming style, there are
#' several aspects of session state that can affect the results of code
#' evaluation (e.g. global environment, search path).  \code{unitizer} provides
#' functionality to increase test reproducibility by controlling session state
#' so that it is the same every time a test is run.  This functionality is
#' turned off by default to comply with CRAN requirements, and also because
#' there are inherent limitations in R that may prevent it from fully working in
#' some circumstances.  You can permanently enable the suggested state tracking
#' level by adding \code{options(unitizer.state='suggested')} in your
#' \code{.Rprofile}, although if you intend to do this be sure to read the
#' \dQuote{CRAN non-compliance} section.
#'
#' @section CRAN non-compliance:
#'
#' In the default state management mode, this package fully complies with CRAN
#' policies.  In order to implement advanced state management features we must
#' lightly trace some \code{base} functions to alert \code{unitizer} each time
#' the search path is changed by a test expression.  The traced function
#' behavior is completely unchanged other than for the side effect of notifying
#' \code{unitizer} each time they are called.  Additionally, the functions are
#' only traced during \code{unitize} evaluation and are untraced on exit.
#' Unfortunately this tracing is against CRAN policies, which is why it is
#' disabled by default.
#'
#' For more details see the reproducible tests vignette with:
#' \code{vignette(package='unitizer', 'unitizer_reproducible_tests')}
#'
#' @section Overview:
#'
#' You can control how \code{unitizer} manages state via
#' the state argument to \code{unitize} or by setting the
#' \dQuote{unitizer.state} option.  This help file discusses state
#' management with \code{unitizer}, and also documents two functions that, in
#' conjunction with \code{\link{unitize}} or \code{\link{unitize_dir}} allow you
#' to control state management.
#'
#' \bold{Note}: most of what is written in this page about \code{unitize}
#' applies equally to \code{unitize_dir}.
#'
#' \code{unitizer} provides functionality to insulate test code from variability
#' in the following.  Note the \dQuote{can be} wording because by default
#' these elements of state are not managed:
#'
#' \itemize{
#'   \item Workspace / Parent Environment: all tests can be
#'      evaluated in environments that are children of a special environment
#'      that does not inherit from \code{.GlobalEnv}.  This prevents objects
#'      that are lying around in your workspace from interfering with your
#'      tests.
#'   \item Random Seed: can be set to a specific value at the
#'     beginning of each test file so that tests using random values get the
#'     same value at every test iteration. This only sets the seed at the
#'     beginning of each test file, so changes in order or number of functions
#'     that generate random numbers in your test file will affect subsequent
#'     tests.  The advantage of doing this over just setting the seed directly
#'     in the test files is that \code{unitizer} tracks the value of the seed
#'     and will tell you the seed changed for any given test (e.g. because you
#'     added a test in the middle of the file that uses the random seed).
#'   \item Working Directory: can be set to the tests directory
#'     inside the package directory if the test files appear to be inside the
#'     folder structure of a package, and the test file does not appear to be
#'     run as part of a check run (e.g. R CMD check,
#'     `tools::testInstalledPakage`).  If test files are not inside a package
#'     directory structure then can be set to the test files' directory.
#'   \item Search Path: can be set to what you would
#'     typically find in a freshly loaded vanilla R session.  This means any non
#'     default packages that are loaded when you run your tests are unloaded
#'     prior to running your tests.  If you want to use the same libraries
#'     across multiple tests you can load them with the \code{pre} argument to
#'     \code{\link{unitize}} or \code{\link{unitize_dir}}.  Due to limitations
#'     of R this is only an approximation to actually restarting R into a fresh
#'     session.
#'   \item Options: same as search path, but see "Namespaces" next.
#'   \item Namespaces: same as search path; this
#'     option is only made available to support options since many namespaces
#'     set options \code{onLoad}, and as such it is necessary to unload and
#'     re-load them to ensure default options are set.  See the "Namespaces and
#'     Options" section.
#' }
#'
#' In the \dQuote{suggested} state tracking mode (previously known as
#' \dQuote{recommended}), parent environment, random seed, working directory,
#' and search path are all managed to level 2, which approximates what you would
#' find in a fresh session (see "Custom Control" section below).  For example,
#' with the search path managed, each test file will start evaluation with the
#' search path set to the tests folder of your package.  All these settings are
#' returned to their original values when \code{unitizer} exits.
#'
#' To manage the search path \code{unitizer} detaches
#' and re-attaches packages.  This is not always the same as loading a package
#' into a fresh R session as detaching a package does not necessarily undo every
#' action that a package takes when it is loaded.  See \code{\link{detach}} for
#' potential pitfalls of enabling this setting.
#'
#' You can modify what aspects of state are managed by using the \code{state}
#' parameter to \code{\link{unitize}}.  If you are satisfied with basic default
#' settings you can just use the presets described in the next section.  If you
#' want more control you can use the return values of the \code{state} and
#' \code{in_pkg} functions as the values for the \code{state} parameter for
#' \code{unitize}.
#'
#' State is reset after running each test file when running multiple test
#' files with \code{unitize_dir}, which means state changes in one test file
#' will not affect the next one.
#'
#' @section State Presets:
#'
#' For convenience \code{unitizer} provides several state management presets
#' that you can specify via the \code{state} parameter to \code{\link{unitize}}.
#' The simplest method is to specify the preset name as a character value:
#'
#' \itemize{
#'   \item "suggested": \itemize{
#'       \item Use special (non \code{.GlobalEnv}) parent environemnt
#'       \item Manage search path
#'       \item Manage random seed (and set it to be of type "Wichmann-Hill"
#'         for space considerations).
#'       \item Manage workign directory
#'       \item Leave namespace and options untouched
#'     }
#'   \item "safe" like suggested, but turns off tracking for search path in
#'     addition to namespaces and options.  These settings, particularly the
#'     last two, are the most likely to cause compatibility problems.
#'   \item "pristine" implements the highest level of state tracking and control
#'   \item "basic" keeps all tracking, but at a less aggressive level; state is
#'     reset between each test file to the state before you started
#'     \code{unitize}ing so that no single test file affects another, but the
#'     state of your workspace, search path, etc. when you launch
#'     \code{unitizer} will affect all the tests (see the Custom Control)
#'     section.
#'   \item "off" (default) state tracking is turned off
#' }
#'
#' @section Custom Control:
#'
#' If you want to customize each aspect of state control you can pass a
#' \code{unitizerState} object as the \code{state} argument.  The simplest way
#' to do this is by using the \code{\link{state}} constructor function.  Look
#' at the examples for how to do this.
#'
#' For convenience \code{unitize} allows you to directly specify a parent
#' environment if all you want to change is the parent evaluation environment
#' but are otherwise satisfied with the defaults.  You can even use the
#' \code{\link{in_pkg}} function to tell \code{unitizer} to use the namespace
#' associated with your current project, assuming it is an R package.  See
#' examples for details.
#'
#' If you do chose to modify specific aspects of state control here is a guide
#' to what the various parameter values for \code{state} do:
#' \itemize{
#'   \item For \code{par.env}: any of the following:
#'     \itemize{
#'       \item \code{NULL} to use the special \code{unitizer} parent
#'         environment as the parent environment; this environment has for
#'         parent the parent of \code{.GlobalEnv}, so any tests evaluated
#'         therein will not be affected by objects in \code{.GlobalEnv}
#'         see (\code{vignette("unitizer_reproducible_state")}).
#'       \item an environment to use as the parent evaluation environment
#'       \item the name of a package to use that package's namespace
#'         environment as the parent environment
#'       \item the return value of \code{in_pkg}; used primarily to autodetect
#'         what package namespace to use based on package directory structure
#'     }
#'   \item For all other slots, the settings are in \code{0:2} and mean:
#'     \itemize{
#'       \item 0 turn off state tracking
#'       \item 1 track, but start with state as it was when \code{unitize} was
#'         called.
#'       \item 2 track and set state to what you would typically find in a clean
#'         R session, with the exception of \code{random.seed}, which is
#'         set to \code{getOption("unitizer.seed")} (of kind "Wichmann-Hill"
#'         as that seed is substantially smaller than the R default seed).
#' } }
#'
#' If you chose to use level \code{1} for the random seed you should consider
#' picking a random seed type before you start unitizer that is small like
#' "Wichman-Hill" as the seed will be recorded each time it changes.
#'
#' @section Permanently Setting State Tracking:
#'
#' You can permanently change the default state by setting the
#' \dQuote{unitizer.state} option to the name of the state presets above or to a
#' or to a state settings option object generated with \code{state} as described
#' in the previous section.
#'
#' @section Avoiding \code{.GlobalEnv}:
#'
#' For the most part avoiding \code{.GlobalEnv} leads to more robust and
#' reproducible tests since the tests are not influenced by objects in the
#' workspace that may well be changing from test to test.  There are some
#' potential issues when dealing with functions that expect \code{.GlobalEnv} to
#' be on the search path.  For example, \code{setClass} uses \code{topenv} to
#' find a default environment to assign S4 classes to.  Typically this will be
#' the package environment, or \code{.GlobalEnv}.  However, when you are in
#' \code{unitizer} this becomes the next environment on the search path, which
#' is typically locked, which will cause \code{setClass} to fail.  For those
#' types of functions you should specify them with an environment directly, e.g.
#' \code{setClass("test", slots=c(a="integer"), where=environment())}.
#'
#' @section Namespaces and Options:
#'
#' Options and namespace state management require the ability to fully unload
#' any non-default packages and namespaces, and there are some packages that
#' cannot be unloaded, or should not be unloaded (e.g.
#' \href{https://github.com/Rdatatable/data.table/issues/990}{data.table}).  I
#' some systems it may even be impossible to fully unload any compiled code
#' packages (see \code{\link{detach}}. If you know the packages you typically
#' load in your sessions can be unloaded, you can turn this functionality on by
#' setting \code{options(unitizer.state="pristine")} either in your session, in
#' your \code{.Rprofile} file, or using \code{state="prisitine"} in each call to
#' \code{unitize} or \code{unitize_dir}.  If you have packages that cannot be
#' unloaded, but you still want to enable these features, see the "Search Path
#' and Namespace State Options" section of \code{\link{unitizer.opts}} docs.
#'
#' If you run \code{unitizer} with options and namespace tracking and you run
#' into a namespace that cannot be unloaded, or should not be unloaded because
#' it is listed in \code{getOption("unitizer.namespace.keep")}, \code{unitizer}
#' will turn off \code{options} state tracking from that point onwards.
#'
#' Additionally, note that \code{warn} and \code{error} options are always set
#' to \code{1} and \code{NULL} respectively during test evaluation, irrespective
#' of what option state tracking level you select.
#'
#' @section Known Untracked State Elements:
#'
#' \itemize{
#'   \item system time: tests involving functions such as \code{\link{date}}
#'     will inevitably fail
#'   \item locale: is not tracked because it so specific to the system and so
#'     unlikely be be changed by user action; if you have tests that depend on
#'     locale be sure to set the locale via the \code{pre} argument to
#'     \code{\link{unitize}}, and also to reset it to the original value in
#'     \code{post}.
#' }
#' @param search.path one of \code{0:2}, uses the default value corresponding to
#'   \code{getOption(unitizer.state)}, which is 0 in the default unitizer state
#'   of \dQuote{off}.  See "Custom Control" section for details.
#' @param options same as \code{search.path}
#' @param working.directory same as \code{search.path}
#' @param random.seed same as \code{search.path}
#' @param namespaces same as \code{search.path}
#' @param par.env \code{NULL} to use the special \code{unitizer} parent
#'   environment, or an environment to use as the parent environment, or
#'   the name of a package as a character string to use that packages'
#'   namespace as the parent environment, or a \code{unitizerInPkg} object
#'   as produced by \code{\link{in_pkg}}, assumes .GlobalEnv if unspecified
#' @param package character(1L) or NULL; if NULL will tell \code{unitize}
#'   to attempt to identify if the test file is inside an R package folder
#'   structure and if so run tests in that package's namespace.  This should
#'   work with R CMD check tests as well as in normal usage.  If character will
#'   take the value to be the name of the package to use the namespace of as
#'   the parent environment.  Note that \code{in_pkg} does not retrieve the
#'   environment, it just tells \code{unitize} to do so.
#' @return for \code{state} a \code{unitizerStateRaw} object, for \code{in_pkg}
#'   a \code{unitizerInPkg} object, both of which are suitable as values for
#'   the \code{state} parameter for \code{\link{unitize}} or as values for the
#'   \dQuote{unitizer.state} global option.
#'
#' @aliases state, in_pkg
#' @rdname unitizerState
#' @export state
#' @name unitizerState
#' @seealso \code{\link{unitize}}, \code{\link{unitizer.opts}}
#' @examples
#' \dontrun{
#' ## In this examples we use `...` to denote other arguments to `unitize` that
#' ## you should specify.  All examples here apply equally to `unitize_dir`
#'
#' ## Run with suggested state tracking settings
#' unitize(..., state="suggested")
#' ## Manage as much of state as possible
#' unitize(..., state="pristine")
#'
#' ## No state management, but evaluate with custom env as parent env
#' my.env <- new.env()
#' unitize(..., state=my.env)
#' ## use custom environment, and turn on search.path tracking
#' ## here we must use the `state` function to construct a state object
#' unitize(..., state=state(par.env=my.env, search.path=2))
#'
#' ## Specify a namespace to run in by name
#' unitize(..., state="stats")
#' unitize(..., state=state(par.env="stats")) # equivalent to previous
#'
#' ## Let `unitizer` figure out the namespace from the test file location;
#' ## assumes test file is inside package folder structure
#' unitize("mytests.R", state=in_pkg()) # assuming mytests.R is part of a pkg
#' unitize("mytests.R", state=in_pkg("mypkg")) # also works
#' }

state <- function(
  par.env, search.path, options, working.directory, random.seed, namespaces
) {
  if(!identical(c("par.env", .unitizer.global.settings.names), names(formals())))
    stop(
      "Internal error: state element mismatch; this should not happen, ",
      "contact maintainer."
    )

  supplied.args <- tail(names(match.call()), -1L)
  state.def <- try(as.state_raw(getOption('unitizer.state')))
  if(inherits(state.def, "try-error"))
    stop(
      "Unable to generate state object from `getOption('unitizer.state')`, ",
      "see prior error messages."
    )
  for(i in supplied.args) {
    i.val <- get(i, inherits=FALSE)
    if(i != 'par.env') {
      if(!is.int.1L(i.val) || !i.val %in% 0:2)
        stop("Argument `", i, "` must be integer(1L)  in 0:2")
      i.val <- as.integer(i.val)
    }
    slot(state.def, i) <- i.val
  }

  if(!isTRUE(val.err <- validObject(state.def, test=TRUE))) {
    stop(
      "Unable to create valid `unitizerStateRaw` object: ",
      val.err
    )
  }
  state.def
}
unitizerInPkg <- setClass(
  "unitizerInPkg",
  slots=c(package="character"),
  validity=function(object) {
    if(!is.chr1(object@package))
     return("Slot `package` must be character(1L) and not NA")
  },
  prototype=list(package="")
)
#' @rdname unitizer_s4method_doc
setMethod("as.character", "unitizerInPkg",
  function(x, ...) {
    sprintf(
      "<in: %s>",
      if(nchar(x@package))
        sprintf("package:%s", x@package) else "auto-detect-pkg"
) } )
#' @rdname unitizer_s4method_doc
setMethod(
  "show", "unitizerInPkg",
  function(object) word_cat(as.character(object), sep="\n")
)
setClassUnion(
  "environmentOrNULLOrCharacterUnitizerInPkg",
  c("environment", "NULL", "character", "unitizerInPkg")
)
# unitizerState is an abstract class and is not meant to be instantiated.  It
# defines basic structure for unitizerStateRaw and unitizerStateProcessed.
# `as.state` will process a unitizerStateRaw class into a unitizerStateProcessed`
# objects.  Note that `unitizerState` does not have a `par.env` slot as that is
# added by the child classes

unitizerState <- setClass(
  "unitizerState",
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
  contains="VIRTUAL",
  validity=function(object) {
    # seemingly superflous used to make sure this object is in concordance with
    # the various others that are similar
    if(
      !identical(
        setdiff(slotNames(object), 'par.env'), .unitizer.global.settings.names
      )
    ) {
      return(
        paste0(
          "Invalid state object, slots must be ",
          deparse(.unitizer.global.settings.names, width.cutoff=500L)
      ) )
    }
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
      (
        !identical(object@namespaces, 2L) ||
        !identical(object@search.path, 2L)
      )
    )
      return(
        paste0(
          "Argument `state` is an invalid state: 'options' is set ",
          "to 2, but 'search.path' and 'namespaces' are not"
      ) )
    if(
      identical(object@namespaces, 2L) && !identical(object@search.path, 2L)
    )
      return(
        paste0(
          "Argument `state` is an invalid state: 'namespaces' is set ",
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
# The main advantage of unitizerStateRaw is that it allow us to store some
# parent environments in the form of "promises", specifically, if we use an
# "unitizerInPkg" object without specifying a package, we can let unitizer try
# to infer the package from the test directory under the presumption that the
# test directory is contained inside a package directory).  So we can set the
# "promise" at a time independent of unitizer evaluation, and the environment
# gets resolved at evaluation time.  This is helpful if we want to store this as
# an option, etc.

unitizerStateRaw <- setClass(
  "unitizerStateRaw",
  slots=c(par.env="environmentOrNULLOrCharacterUnitizerInPkg"),
  contains="unitizerState",
  prototype=list(par.env=.GlobalEnv),
  validity=function(object){
    if(is.character(object@par.env) && !is.chr1(object@par.env))
     return("Slot `par.env` must be 1 long and not NA if it is character")
    TRUE
  }
)
# Note that an instantiation of `unitizerStateProcessed` basically should be the
# default state object

unitizerStateProcessed <- setClass(
  "unitizerStateProcessed",
  slots=c(par.env="environmentOrNULL"),
  contains="unitizerState",
  prototype=list(par.env=.GlobalEnv)
)
setMethod("initialize", "unitizerState",
  function(.Object, ...) {
    dots <- list(...)
    for(i in names(dots))
      if(is.numeric(dots[[i]])) dots[[i]] <- as.integer(dots[[i]])
    do.call(callNextMethod, c(.Object, dots))
} )
unitizerStateSuggested <- setClass(
  "unitizerStateSuggested",
  contains="unitizerStateRaw",
  prototype=list(
    search.path=2L, options=0L, working.directory=2L, random.seed=2L,
    namespaces=0L, par.env=NULL
  )
)
unitizerStatePristine <- setClass(
  "unitizerStatePristine", contains="unitizerStateRaw",
  prototype=list(
    search.path=2L, options=2L, working.directory=2L, random.seed=2L,
    namespaces=2L, par.env=NULL
  )
)
unitizerStateSafe <- setClass(
  "unitizerStateSafe", contains="unitizerStateRaw",
  prototype=list(
    search.path=0L, options=0L, working.directory=2L, random.seed=2L,
    namespaces=0L, par.env=NULL
  )
)
unitizerStateBasic <- setClass(
  "unitizerStateBasic", contains="unitizerStateRaw",
  prototype=list(
    search.path=1L, options=1L, working.directory=1L, random.seed=1L,
    par.env=NULL
  )
)
unitizerStateOff <- setClass(
  "unitizerStateOff", contains="unitizerStateRaw",
  prototype=list(
    search.path=0L, options=0L, working.directory=0L, random.seed=0L,
    namespaces=0L, par.env=.GlobalEnv
  )
)
#' @rdname unitizerState
#' @export

in_pkg <- function(package=NULL) {
  if(!is.null(package) && !is.chr1(package))
    stop("Argument `package` must be character(1L) and not NA, or NULL")
  if(is.character(package) && !nchar(package))
    stop("Argument `package` may not be an empty string")
  unitizerInPkg(package=if(is.null(package)) "" else package)
}
in_pkg_to_env <- function(inPkg, test.files) {
  stopifnot(
    is(inPkg, "unitizerInPkg"),
    is.character(test.files) || is.null(test.files)
  )
  pkg <- if(nchar(inPkg@package)) {
    inPkg@package
  } else {
    if(is.null(test.files) || !length(pkg.tmp <- get_package_dir(test.files))){
      stop(
        word_wrap(collapse="\n",
          cc(
            "Unable to detect package to use namespace of as parent ",
            "environment; see `?unitizerState` for how to specify ",
            "a package namespace explicitly as a parent environment."
      ) ) )
    }
    pkg.name <- try(get_package_name(pkg.tmp))
    if(inherits(pkg.name, "try-error"))
      stop("Unable to extract package name from DESCRIPTION.")
    pkg.name
  }
  pkg.env <- try(getNamespace(pkg))
  if(inherits(pkg.env, "try-error"))
    stop(
      word_wrap(collapse="\n",
        cc(
          "Unable to load \"", pkg, "\" namespace to use as parent ",
          "environment; see `?unitizerState` for instructions on how to ",
          "specify a package namespace as a parent environment for tests."
    ) ) )
  pkg.env
}
# This method is a bit odd because it has logic for dealing with slots that are
# not actually defined in the class but are in the child classes that are
# expected to kick of the method
#' @rdname unitizer_s4method_doc

setMethod(
  "show", "unitizerState",
  function(object) {
    sn <- slotNames(object)
    sv <- sapply(sn, slot, object=object, simplify=FALSE)
    sv.env <- sv$par.env
    sv.extra <- ""
    sv[["par.env"]] <- if(is.null(sv.env)) {
      sv.extra <-  "<auto>: use special unitizer environment as 'par.env'"
      "<auto>"
    } else if (is(sv.env, "unitizerInPkg")) {
      sv.extra <- "<in: ???>: run with specified package namespace as parent"
      as.character(sv.env)
    } else if (is.environment(sv.env)) {
      env_name(sv.env)
    } else
      # nocov start
      stop("Internal Error: unexpected `par.env` slot type; contact maintainer")
      # nocov end
    res <- data.frame(Settings=sn, Values=unlist(sv))
    rownames(res) <- NULL
    print(res)
    word_cat(
      "-----", "0: off", "1: track starting with initial state",
      "2: track starting with clean state",
      if(nchar(sv.extra)) sv.extra,
      "See `?unitizerState` for more details.",
      sep="\n"
    )
  }
)
setGeneric(
  "as.unitizerStateProcessed",
  function(x, ...) standardGeneric("as.unitizerStateProcessed")
)
setMethod("as.unitizerStateProcessed", "unitizerStateRaw",
  function(x, ...) {
    x.proc <- new("unitizerStateProcessed")
    for(i in slotNames(x))
      slot(x.proc, i) <- slot(x, i)

    x.proc
} )
# Valid State Settings
#
# @keywords internal
# @param x objet to test
# @return a \code{unitizerState} object

as.state <- function(x, test.files=NULL) {
  stopifnot(is.character(test.files) || is.null(test.files))
  x.raw <- as.state_raw(x)

  x.fin <- if(is(x.raw, "unitizerStateRaw")) {
    par.env <- if(is.character(x.raw@par.env)) {
      try(getNamespace(x.raw@par.env))
    } else if(is(x.raw@par.env, "unitizerInPkg")) {
      try(in_pkg_to_env(x.raw@par.env, test.files))
    } else x.raw@par.env

    if(inherits(par.env, "try-error"))
      stop("Unable to convert `par.env` value to a namespace environment")

    x.raw@par.env <- par.env
    as.unitizerStateProcessed(x.raw)
  } else x.raw

  # Final sanity checks

  if(x.fin@options > x.fin@namespaces) {
    stop(
      word_wrap(collapse="\n",
        cc(
          "Options state tracking (", x.fin@options, ") must be less than ",
          "namespace state tracking (", x.fin@namespaces, ")."
    ) ) )
  }
  if(x.fin@namespaces > x.fin@search.path) {
    stop(
      word_wrap(collapse="\n",
        cc(
          "Namespace state tracking (", x.fin@namespaces, ") must be less ",
          "than or equal to search path state tracking (", x.fin@search.path,
          ")."
    ) ) )
  }
  # Last ditch check

  if(!isTRUE(test <- validObject(x.fin, test=TRUE)))
    stop(
      "Internal Error: failed processing raw state object, contact ",
      "maintainer. (", test, ")"
    )

  return(x.fin)
}
char_or_null_as_state <- function(x) {
  if(is.null(x)) x <- "off"  # default state
  if(!is.character(x) || !x %in% .unitizer.valid.state.abbr)
    stop(
      "Internal error, `x` must be char and match a known unitizer state ",
      "setting to convert to state, contact maintainer."
    )
  # note these are all raw objects

  switch(
    x, recommended=,suggested=new("unitizerStateSuggested"),
    pristine=new("unitizerStatePristine"),
    basic=new("unitizerStateBasic"),
    off=new("unitizerStateOff"),
    safe=new("unitizerStateSafe")
  )
}
setGeneric(
  "as.unitizerStateRaw",
  function(x, ...) standardGeneric("as.unitizerStateRaw")
)
# Raw states can contain pretty much everything processed ones can

setMethod("as.unitizerStateRaw", "unitizerState",
  function(x, ...) {
    state <- new("unitizerStateRaw")
    for(i in slotNames(state)) slot(state, i) <- slot(x, i)
    validObject(state)
    state
  }
)
## Generate a state raw object
##
## "unitizerStateProcessed" objects are returned as is.  The `x` argument is one
## of the things that can be converted to a state object.  This function is a
## little misleading because it will return "unitizerStateProcessed" objects if
## it can, and only if not "unitizerStateRaw" objects (e.g. if we're dealing
## with "in_pkg")

as.state_raw <- function(x) {
  err.msg <- cc(
    "%s must be character(1L) %%in%% ",
    deparse(.unitizer.valid.state.abbr), ", NULL, an environment, or ",
    "must inherit from S4 classes `unitizerStateRaw`, ",
    "`unitizerStateProcessed` or `unitizerInPkg` ",
    "in order to be interpreted as a unitizer state object."
  )
  if(
    !is(x, "unitizerState") &&
    !(is.chr1(x) && x %in% .unitizer.valid.state.abbr) &&
    !is.environment(x) &&
    !is(x, "unitizerInPkg") &&
    !is.null(x)
  )
    stop(word_wrap(collapse="\n", sprintf(err.msg, "Argument `x`")))

  x.raw <- if(!is(x, "unitizerState")) {
    if(is.null(x) || is.character(x)) {
      char_or_null_as_state(x)
    } else {
      # x is either an environment or inPkg, so need to create a state object to
      # inject that in

      state.opt <- getOption("unitizer.state")
      state.tpl <- if(is(state.opt, "unitizerState")) {
        as.unitizerStateRaw(state.opt)
      } else if (is(state.opt, "unitizerStateRaw")) {
        state.opt
      } else if (is.null(state.opt) || is.character(state.opt)) {
        char_or_null_as_state(state.opt)
      } else if (is.environment(state.opt) || is(state.opt, "unitizerInPkg")) {
        stop(
          "Value for `getOption('unitizer.state')` is incompatible with ",
          "using an environment or an 'unitizerInPkg' object as the value ",
          "for the `state` argument because it also is an environment ",
          "or a 'unitizerInPkg' object; you must change the option ",
          "or the `state` argument to be compatible."
        )
      } else {
        stop(
          word_wrap(
            collapse="\n",
            sprintf(err.msg, "`getOption('unitizer.state')`")
          )
        )
      }
      state.tpl@par.env <- x
      state.tpl
    }
  } else x

  x.raw
}
