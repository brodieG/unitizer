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

# - Required Code -------------------------------------------------------------

#' @include conditions.R
#' @include class_unions.R
#' @include text.R
#' @include deparse.R
#' @include list.R
#' @include item.R

NULL

unitizerItemDataSlots <- slotNames("unitizerItemData")  # cache this for validity

# Virtual Class To Enforce Slots on Subclasses
#
# @keywords internal

setClass("unitizerItemTests", contains="VIRTUAL",
  validity=function(object) {
    if(!all(slotNames(getClass(object)) == unitizerItemDataSlots))
      return("slots must be defined as ", deparse(unitizerItemDataSlots))
    TRUE
  }
)
# Validates Functions for Use in New vs. Reference Object Comparison
# @keywords internal

setClass(
  "unitizerItemTestFun",
  slots=c(fun="function", fun.name="character"),
  prototype=list(fun=all.equal),
  validity=function(object) {
    if(!isTRUE(err <- is.two_arg_fun(object@fun)))
      return(
        cc(
         "Slot `@fun` must be a function with the first two parameters ",
         "non-optional and all others optional (", err, ")."
      ) )
    TRUE
  }
)
setMethod("initialize", "unitizerItemTestFun", function(.Object, ...) {
  dots <- list(...)
  if(!("fun" %in% names(dots))) stop("Argument `fun` required.")
  if(!("fun.name" %in% names(dots))) {
    fun.name <- deparse_fun(substitute(list(...))[["fun"]])
    return(callNextMethod(.Object, fun.name=fun.name, ...))
  }
  return(callNextMethod())
} )

# Stores Errors from Evaluating New vs. Ref Comparisons
#
# There various nested objects involved:
# \itemize{
#   \item \code{`unitizerItemTestError`} contains the error produced from a
#     comparison
#   \item \code{`unitizerItemTestsErrors`} aggregates the errors for each slot
#     of an item
#   \item \code{`unitizerItemsTestsErrors`} aggregates all the errors for the
#     \code{`\link{unitizer-class}`} object
# }
#
# @aliases unitizerItemsTestsErrors-class, unitizerItemTestsErrors-class
# @keywords internal

setClass("unitizerItemTestError",
  representation(
    value="characterOrNULL",
    compare.err="logical",
    .new="ANY",
    .ref="ANY"
  ),
  prototype(value=NULL, compare.err=FALSE, .new=NULL, .ref=NULL),
  validity=function(object) {
    if(!identical(length(object@compare.err), 1L))
      return("slot `@compare.err` must be a 1 length logical")
} )
setClass("unitizerItemTestsErrors",
  representation(
    value="unitizerItemTestError",
    conditions="unitizerItemTestError",
    output="unitizerItemTestError",
    message="unitizerItemTestError",
    aborted="unitizerItemTestError",
    .fail.context="numericOrNULL", # for passing around options for
    .use.diff="logical"
  ),
  prototype(.use.diff=TRUE)
)
unitizerItemTestsErrorsSlots <-
  grep("^[^.]", slotNames("unitizerItemTestsErrors"), value=TRUE)

# used to do this with virtual class, but slow
if(!identical(unitizerItemDataSlots, unitizerItemTestsErrorsSlots)) {
  stop(
    "Install error: `unitizerItemData` and `unitizerItemTestsErrors` slots ",
    "not identical; contact maintainer."
  )
}
unitizerItemTestErrorObj <- new("unitizerItemTestError")
setMethod("initialize", "unitizerItemTestsErrors",
  function(.Object, ...) {
    dots <- list(...)
    if(!all((s.n <- names(dots)) %in% unitizerItemTestsErrorsSlots))
      stop("Unused arguments ", paste0(deparse(names(dots)[!s.n])))
    for(i in seq_along(dots))
      slot(.Object, s.n[[i]]) <- if(is.null(dots[[i]])) {
        unitizerItemTestErrorObj
      } else dots[[i]]
    .Object
} )
## Track Diff And Comparison Error Text
##
## Store whether to show the diff or not in `show.diff`, and the alternate
## text to show in that circumstances in `diff.alt`.  `txt` and `txt.alt` are
## the "headers" shown ahead of `diff` or `diff.alt`

setClass("unitizerItemTestsErrorsDiff",
  slots=c(
    diff="DiffOrNULL",
    diff.alt="character",
    txt="character",
    txt.alt="character",
    err="logical",
    show.diff="logical",  # whether to display the diff
    use.diff="logical"    # whether to use diff or all.equal
  ),
  prototype=list(show.diff=TRUE, use.diff=TRUE, diff.alt=character())
)
setClassUnion(
  "unitizerItemTestsErrorsDiffOrNULL", c("unitizerItemTestsErrorsDiff", "NULL")
)
# Hold diffs for display; only used when a test actually fails and is queued up
# for review by user

setClass("unitizerItemTestsErrorsDiffs",
  slots=c(
    value="unitizerItemTestsErrorsDiffOrNULL",
    conditions="unitizerItemTestsErrorsDiffOrNULL",
    output="unitizerItemTestsErrorsDiffOrNULL",
    message="unitizerItemTestsErrorsDiffOrNULL",
    aborted="unitizerItemTestsErrorsDiffOrNULL",
    state="unitizerItemTestsErrorsDiffOrNULL"
) )
if("state" %in% unitizerItemDataSlots)
  stop(
    "Install error: `unitizerItemData` may not contain a \"state\" slot; ",
    "contact maintainer."
  )
if(
  !identical(
    c(unitizerItemDataSlots, "state"),
    slotNames("unitizerItemTestsErrorsDiffs")
  )
){
  stop(
    "Install error: `unitizerItemData` and `unitizerItemTestsErrorsDiffs` ",
    "slots not identical; contact maintainer."
  )
}
#' Subsetting Methods for unitizerItemTestsErrorsDiffs objects
#'
#' @rdname extract-unitizerItemTestsErrorsDiffs-method
#' @keywords internal

setMethod("$", "unitizerItemTestsErrorsDiffs",
  function(x, name) {
    what <- substitute(name)
    what <- if(is.symbol(what)) as.character(what) else name
    x[[what]]
} )
#' @rdname extract-unitizerItemTestsErrorsDiffs-method
#' @keywords internal

setMethod("[[",  "unitizerItemTestsErrorsDiffs",
  function(x, i, j, ..., exact=TRUE) {
    if(!is.chr1plain(i))
      stop("Argument `i` must be character(1L) and not NA")
    sn <- slotNames(x)
    if(!i %in% sn)
      stop(
        "Argument `i` must be one of ",
        paste0(deparse(sn, width.cutoff=500L), collapse="")
      )
    slot(x, i)
})

setClass(
  "unitizerItemsTestsErrors", contains="unitizerList"
  # ,validity=function(object) { # commented out for computation cost
  #   tests <- vapply(
  #     as.list(object), is, logical(1L), class2="unitizerItemTestsErrors"
  #   )
  #   if(!all(tests))
  #      return(
  #        paste0(
  #          "\"unitizerItemsTestsErrors\" may only contain objects of class ",
  #          "\"unitizerItemTestsErrors\""
  #        )
  #   TRUE
  # }
)
setClassUnion(
  "unitizerItemsTestsErrorsOrLogical",
  c("unitizerItemsTestsErrors", "logical")
)

setGeneric("as.Diffs", function(x, ...) StandardGeneric("as.Diff")) # nocov
setMethod("as.Diffs", "unitizerItemTestsErrors",
  function(x, width=getOption("width"), ...) {
    slots <- grep("^[^.]", slotNames(x), value=TRUE)
    slot.errs <- vapply(
      slots, function(y) !is.null(slot(x, y)@value), logical(1L)
    )
    diffs <- vector("list", length(slots))
    names(diffs) <- slots

    for(i in slots[slot.errs]) {
      curr.err <- slot(x, i)
      mismatch <- if(curr.err@compare.err) {
        paste0("Unable to compare ", i, ": ")
      } else {
        paste0(cap_first(i), " mismatch: ")
      }
      out <- if(length(curr.err@value) < 2L) {
        paste0(mismatch, decap_first(curr.err@value))
      } else {
        c(mismatch, as.character(UL(decap_first(curr.err@value)), width=width))
      }
      make_cont <- function(x) {
        res <- if(identical(i, "value")) {
          as.name(x)
        } else call("$", as.name(toupper(x)), as.name(i))
        call("quote", res)
      }
      diff <- if(x@.use.diff) try(
        diffObj(
          curr.err@.ref, curr.err@.new, tar.banner=make_cont(".ref"),
          cur.banner=make_cont(".new")
        )
      )
      diffs[[i]] <- if(inherits(diff, "try-error")) {
        new(
          "unitizerItemTestsErrorsDiff",
          diff=NULL,
          txt=sprintf("%s: <diff failed>", cap_first(i)),
          txt.alt=sprintf("%s: <diff failed>", cap_first(i)),
          err=curr.err@compare.err,
          show.diff=FALSE,
          use.diff=FALSE
        )
      } else if(is.null(diff)) {
        new(
          "unitizerItemTestsErrorsDiff", diff=diff,
          txt=out,
          txt.alt=out,
          err=curr.err@compare.err,
          diff.alt=capture.output(all.equal(curr.err@.ref, curr.err@.new)),
          use.diff=FALSE, show.diff=TRUE
        )
      } else {
        new(
          "unitizerItemTestsErrorsDiff", diff=diff, txt=out,
          err=curr.err@compare.err, use.diff=x@.use.diff, show.diff=TRUE
        )
      }
    }
    invisible(do.call("new", c(list("unitizerItemTestsErrorsDiffs"), diffs)))
  }
)
#' Show Method for unitizerItemTestsErrorsDiffs objects
#'
#' @rdname show-unitizerItemTestsErrorsDiffs-method
#' @keywords internal

setMethod("show", "unitizerItemTestsErrorsDiffs",
  function(object) {
    sn <- slotNames(object)
    null.slots <- vapply(sn, function(x) is.null(slot(object, x)), logical(1L))
    if(!all(null.slots)) {
      for(i in sn[!null.slots]) show(slot(object, i))
    }
    invisible(object)
} )
#' Show Method for unitizerItemTestsErrorsDiff objects
#'
#' @rdname show-unitizerItemTestsErrorsDiff-method
#' @keywords internal

setMethod("show", "unitizerItemTestsErrorsDiff",
  function(object) {
    cat_fun <- if(object@err) meta_word_msg else meta_word_cat
    cat_fun(if(object@show.diff) object@txt else object@txt.alt)
    if(object@show.diff) {
      if(object@use.diff) show(object@diff)
      else cat(object@diff.alt, sep='\n')
      cat("\n")
    }
    invisible(NULL)
} )

#' Like all.equal but Returns Empty String If Not all.equal
#'
#' Used as the default value comparison function since when values mismatch
#' we use \code{\link{diffObj}} which would make the text output from
#' \code{\link{all.equal}} somewhat redundant.
#'
#' @export
#' @param target R object
#' @param current other R object to be compared to \code{target}
#' @param ... arguments to pass to \code{\link{all.equal}}
#' @return TRUE if \code{all.equal} returns TRUE, "" otherwise
#' all_eq(1, 1L)
#' all_eq(1, 2)
#' isTRUE(all_eq(1, 2))

all_eq <- function(target, current, ...)
  if(isTRUE(all.equal(target, current, ...))) TRUE else ""

#' Store Functions for New vs. Reference Test Comparisons
#'
#' \code{testFuns} contains the functions used to compare the results and side
#' effects of running test expressions.  \dQuote{testFuns} objects can be used
#' as the \code{compare} argument for \code{\link{unitizer_sect}}, thereby
#' allowing you to specify different comparison functions for different aspects
#' of test evaluation.
#'
#' The default comparison functions are as follows:
#' \itemize{
#'   \item value: \code{\link{all_eq}}
#'   \item conditions: \code{\link{all_eq}}
#'   \item output: \code{function(x, y) TRUE}, i.e. not compared
#'   \item message: \code{function(x, y) TRUE}, i.e. not compared as conditions
#'     should be capturing warnings/errors
#'   \item aborted: \code{function(x, y) TRUE}, i.e. not compared as conditions
#'     should also be capturing this implicitly
#' }
#' @seealso \code{\link{unitizer_sect}} for more relevant usage examples,
#'    \code{\link{all_eq}}
#' @rdname testFuns
#' @name testFuns
#' @export testFuns
#' @examples
#' # use `identical` instead of `all.equal` to compare values
#' testFuns(value=identical)

testFuns <- setClass(
  "testFuns", contains="unitizerItemTests",
  representation(
    value="unitizerItemTestFun",
    conditions="unitizerItemTestFun",
    output="unitizerItemTestFun",
    message="unitizerItemTestFun",
    aborted="unitizerItemTestFun"
  ),
  prototype(
    value=new("unitizerItemTestFun", fun=all_eq),
    # note this will dispatch all.equal.condition_list
    conditions=new("unitizerItemTestFun", fun=all_eq),
    output=new("unitizerItemTestFun", fun=function(target, current) TRUE),
    message=new("unitizerItemTestFun", fun=function(target, current) TRUE),
    aborted=new("unitizerItemTestFun", fun=function(target, current) TRUE)
) )

# Ensures Functions are In Correct Format
#
# Also, allows the user to specify functions directly instead of having
# to instantiate \code{`\link{unitizerItemTestFun-class}`} for each function.
# Finally, recovers the deparsed passed function name.
# @keywords internal

setMethod("initialize", "testFuns", function(.Object, ...) {
  dots <- list(...)
  if(!all(err.slots <- names(dots) %in% slotNames(getClass(.Object))))
    stop("Can't initialize invalid slots ", deparse(names(dots)[!err.slots]))
  fun.names <- vapply(substitute(list(...))[-1L], deparse_fun, character(1L))
  if(!all(names(fun.names) %in% names(dots)))
    stop("Internal Error: contact package maintainer.") # nocov
  for(i in names(dots)) {
    slot(.Object, i) <- if(is(dots[[i]], "unitizerItemTestFun")) {
      dots[[i]]
    } else {
      new("unitizerItemTestFun", fun=dots[[i]], fun.name=fun.names[[i]])
  } }
  .Object
} )
