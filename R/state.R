#' @include global.R

NULL

.unitizer.valid.state.abbr <-  c("pristine", "default", "basic", "off", "safe")

#' Tests and Session State
#'
#' While R generally adheres to a "functional" programming style, there are
#' several aspects of session state that can affect the results of code
#' evaluation.  \code{unitizer} attempts to make tests as reproducible as
#' possible by controlling session state so that it is the same every time a
#' test is run.  You can control how \code{unitizer} manages state via the
#' state argument to \code{unitize}.  This help file discusses state management
#' with \code{unitizer}, and also documents two functions that, in conjunction
#' with \code{\link{unitize}} or \code{\link{unitize_dir}} allow you to control
#' state management.
#'
#' @section Overview:
#'
#' \bold{Note}: most of what is written in this page about \code{unitize}
#' applies equally to \code{unitize_dir}
#'
#' \code{unitizer} provides functionality to insulate test code from variability
#' in the following:
#'
#' \itemize{
#'   \item Workspace / Parent Environment (enabled by default): all tests are
#'      evaluated in environments that are children of a special environment
#'      that does not inherit from \code{.GlobalEnv}.  This prevents objects
#'      that are lying around in your workspace from interfering with your
#'      tests.
#'   \item Random Seed (enabled by default): is set to a specific value at the
#'     beginning of each test file so that tests using random values get the
#'     same value at every test iteration. If you change the order of  your
#'     tests, or add a test that uses a random sampling before the end of
#'     the file, that will still affect the random seed.
#'   \item Working Directory (enabled by default): is set to the tests directory
#'     inside the package directory provided all test files are in the same
#'     sub-directory of a package.
#'   \item Search Path (enabled by default): is set to what you would
#'     typically find in a freshly loaded vanilla R session.  This means any non
#'     default packages that are loaded when you run your tests are unloaded
#'     prior to running your tests.  If you want to use the same libraries
#'     across multiple tests you can load them with the \code{pre} argument to
#'     \code{\link{unitize}} or \code{\link{unitize_dir}}.
#'   \item Options (\bold{disabled} by default): same as search path
#'   \item Namespaces (\bold{disabled} by default): same as search path; this
#'     option is only made available to support options since many namespaces
#'     set options \code{onLoad}, and as such it is necessary to unload and
#'     re-load them to ensure default options are set.
#' }
#' State is reset after running each test file when running multiple test
#' files with \code{unitize_dir}, which means state changes in one test file
#' will not affect the next one.
#'
#' You can modify what aspects of state are managed by using the \code{state}
#' parameter to \code{\link{unitize}}.  If you are satisfied with basic default
#' settings you can just use the presets described in the next section.  If you
#' want more control you can use the return values of the \code{state} and
#' \code{in_pkg} functions as the values for the \code{state} parameter for
#' \code{unitize}.
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
#' @section State Presets:
#'
#' For convenience \code{unitizer} provides several state management presets
#' that you can specify via the \code{state} parameter to \code{\link{unitize}}.
#' The simplest method is to specify the preset name as a character value:
#'
#' \itemize{
#'   \item "default" turns off options and namespace tracking,
#'     but otherwise enables all other state tracking.  This is the default
#'     behavior.
#'   \item "safe" turns off tracking for search path, namespaces and options.
#'     These settings, particularly the last two, are the most likely to cause
#'     compatibility problems.
#'   \item "pristine" implements the highest level of state tracking and control
#'   \item "basic" keeps all tracking, but at a less aggressive level; state is
#'     reset between each test file to the state before you started
#'     \code{unitize}ing so that no single test file affects another, but the
#'     state of your workspace, search path, etc. when you launch
#'     \code{unitizer} will affect all the tests (see the Custom Control)
#'     section.
#'   \item "off" state tracking is turned off
#' }
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
#'         called
#'       \item 2 track and set state to what you would typically find in a clean
#'         R session, with the exception of \code{random.seed}, which is
#'         set to \code{getOption("unitizer.seed")} (of kind "Wichmann-Hill"
#'         as that seed is substantially smaller than the R default seed).
#' } }
#' @section Namespaces and Options:
#'
#' Options and namespace state management are turned off by default
#' because in order to work they require the ability to fully unload any
#' non-default packages and namespaces, and there are some packages that cannot
#' be unloaded, or should not be unloaded (e.g.
#' \href{https://github.com/Rdatatable/data.table/issues/990}{data.table}). If
#' you know the packages you typically load in your sessions can be unloaded,
#' you can turn this functionality on by setting
#' \code{options(unitizer.state="pristine")} either in your session, in your
#' \code{.Rprofile} file, or using \code{state="prisitine"} in each call to
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
#' @param search.path one of \code{0:2}
#' @param options one of \code{0:2}
#' @param working.directory one of \code{0:2}
#' @param random.seed one of \code{0:2}
#' @param namespaces one of \code{0:2}
#' @param par.env \code{NULL} to use the special \code{unitizer} parent
#'   environment, or an environment to use as the parent environment, or
#'   the name of a package as a character string to use that packages'
#'   namespace as the parent environment, or a \code{unitizerInPkg} object
#'   as produced by \code{\link{in_pkg}}
#' @param package character(1L) or NULL; if NULL will tell \code{unitize}
#'   to attempt to identify if the test file is inside an R package folder
#'   structure and if so run tests in that package's namespace.  This should
#'   work with R CMD check tests as well as in normal usage.  If character will
#'   take the value to be the name of the package to use the namespace of as
#'   the parent environment.  Note that \code{in_pkg} does not retrieve the
#'   environment, it just tells \code{unitize} to do so.
#' @return for \code{state} a \code{unitizerStateRaw} object, for \code{in_pkg}
#'   a \code{unitizerInPkg} object, both of which are suitable as values for
#'   the \code{state} paramter for \code{\link{unitize}}.
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
#' ## Turn off state tracking completely
#' unitize(..., state="off")
#' ## Manage as much of state as possible
#' unitize(..., state="pristine")
#'
#' ## Use default state management, but evaluate in a custom environment
#' my.env <- new.env()
#' unitize(..., state=my.env)
#' ## use custom environment, and turn off search.path tracking
#' ## here we must use the `state` function to construct a state object
#' unitize(..., state=state(par.env=my.env, search.path=0))
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
  par.env=NULL, search.path=2L, options=0L, working.directory=2L,
  random.seed=2L, namespaces=0L
) {
  if(!identical(c("par.env", .unitizer.global.settings.names), names(formals())))
    stop(
      "Logic Error: state element mismatch; this should not happen, contact ",
      "maintainer"
    )
  args <- as.list(environment())
  if(
    !is.null(par.env) && !is.chr1(par.env) && !is.environment(par.env) &&
    !is(par.env, "unitizerInPkg")
  )
    stop(
      "Argument `par.env` must be NULL, character(1L), an environment, or ",
      "a `unitizerInPkg` object"
    )
  if(is.chr1(par.env)) {
    par.env <- try(getNamespace(par.env))
    if(inherits(par.env, "try-error"))
      stop(
        "Unable to retrieve namespace for `", par.env, "`; make sure the ",
        "value actually refers to an installed package."
      )
    args[["par.env"]] <- par.env
  }
  state <- try(do.call(unitizerStateRaw, args))
  if(inherits(state, "try-error"))
    stop("Unable to create state object; see prior errors.")
  state
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
    search.path=2L, options=0L, working.directory=2L, random.seed=2L,
    namespaces=0L
  ),
  contains="VIRTUAL",
  validity=function(object) {
    # seemingly superflous used to make sure this object is in concordance with
    # the various others that are similar
    if(!identical(slotNames(object), .unitizer.global.settings.names))
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
      !identical(object@namespaces, 2L)  &&
      !identical(object@search.path, 2L)
    )
      return(
        paste0(
          "Argument `reproducible.state` has an invalid state: 'options' is set ",
          "to 2, but 'search.path' and 'namespaces' are not"
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
unitizerStateRaw <- setClass(
  "unitizerStateRaw",
  slots=c(par.env="environmentOrNULLOrCharacterUnitizerInPkg"),
  contains="unitizerState",
  prototype=list(par.env=NULL),
  validity=function(object){
    if(is.character(object@par.env) && !is.chr1(object@par.env))
     return("Slot `par.env` must be 1 long and not NA if it is character")
    TRUE
  }
)
unitizerStateProcessed <- setClass(
  "unitizerStateProcessed",
  slots=c(par.env="environmentOrNULL"),
  contains="unitizerState",
  prototype=list(par.env=NULL)
)
setMethod("initialize", "unitizerState",
  function(.Object, ...) {
    dots <- list(...)
    for(i in names(dots))
      if(is.numeric(dots[[i]])) dots[[i]] <- as.integer(dots[[i]])
    do.call(callNextMethod, c(.Object, dots))
} )
unitizerStateDefault <- setClass(
  "unitizerStateDefault",
  contains="unitizerStateProcessed",
  prototype=list(
    search.path=2L, options=0L, working.directory=2L, random.seed=2L,
    namespaces=0L, par.env=NULL
  )
)
unitizerStatePristine <- setClass(
  "unitizerStatePristine", contains="unitizerStateProcessed",
  prototype=list(
    search.path=2L, options=2L, working.directory=2L, random.seed=2L,
    namespaces=2L, par.env=NULL
  )
)
unitizerStateSafe <- setClass(
  "unitizerStateSafe", contains="unitizerStateProcessed",
  prototype=list(
    search.path=0L, options=0L, working.directory=2L, random.seed=2L,
    namespaces=0L, par.env=NULL
  )
)
unitizerStateBasic <- setClass(
  "unitizerStateBasic", contains="unitizerStateProcessed",
  prototype=list(
    search.path=1L, options=1L, working.directory=1L, random.seed=1L,
    par.env=NULL
  )
)
unitizerStateOff <- setClass(
  "unitizerStateOff", contains="unitizerStateProcessed",
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
      stop("Logic Error: unexpected `par.env` slot type; contact maintainer")
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
# Valid State Settings
#
# @keywords internal
# @param x objet to test
# @return a \code{unitizerState} object

as.state <- function(x, test.files=NULL) {
  if(
    !is(x, "unitizerStateRaw") &&
    !(is.chr1(x) && x %in% .unitizer.valid.state.abbr) &&
    !is.environment(x) &&
    !is(x, "unitizerInPkg") &&
    !is.null(x)
  ) {
    stop(
      word_wrap(collapse="\n",
        cc(
          "Argument must be character(1L) %in% ",
          deparse(.unitizer.valid.state.abbr), ", NULL, an environment, or ",
          "must inherit from S4 classes `unitizerStateRaw` or `unitizerInPkg` ",
          "in order to be interpreted as a unitizer state object."
    ) ) )
  }
  stopifnot(
    is.character(test.files) || is.null(test.files),
    !is(x, "unitizerInPkg") || is.character(test.files) ||
    !!nchar(x@package)
  )
  if(is.null(x)) x <- "off"  # default state
  x <- if(is.character(x)){
    switch(
      x, default=new("unitizerStateDefault"),
      pristine=new("unitizerStatePristine"),
      basic=new("unitizerStateBasic"), off=new("unitizerStateOff"),
      safe=new("unitizerStateSafe")
    )
  } else if(is(x, "unitizerStateRaw")) {
     par.env <- if(is.character(x@par.env)) {
       try(getNamespace(x@par.env))
     } else if(is(x@par.env, "unitizerInPkg")) {
       try(in_pkg_to_env(x@par.env, test.files))
    }
    if(inherits(par.env, "try-error"))
      stop("Unable to convert `par.env` value to a namespace environment")
    x.fin <- unitizerStateDefault(par.env=par.env)
    for(i in .unitizer.global.settings.names) slot(x.fin, i) <- slot(x, i)
    x.fin
  } else if(is(x, "unitizerInPkg")){
    unitizerStateDefault(par.env=in_pkg_to_env(x, test.files))
  } else if(is.environment(x)){
    unitizerStateDefault(par.env=x)
  }
  if(x@options > x@namespaces) {
    stop(
      word_wrap(collapse="\n",
        cc(
          "Options state tracking (", x@options, ") must be less than ",
          "namespace state tracking (", x@namespaces, ")."
    ) ) )
  }
  if(x@namespaces > x@search.path) {
    stop(
      word_wrap(collapse="\n",
        cc(
          "Namespace state tracking (", x@namespaces, ") must be less than ",
          "or equal to search path state tracking (", x@search.path, ")."
    ) ) )
  }
  return(x)
}
