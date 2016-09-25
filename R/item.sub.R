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
  representation(fun="function", fun.name="character"),
  validity=function(object) {
    frms <- formals(object@fun)
    frms <- frms[!(names(frms) %in% "...")]
    if(
      length(frms) < 2L |
      any(
        vapply(
          head(frms, 2L),
          function(frm)
            !(is.symbol(frm) && nchar(as.character(frm)) == 0L),
          logical(1L))
       ) |
      any(
        vapply(
          tail(frms, -2L),
          function(frm) (is.symbol(frm) && nchar(as.character(frm)) == 0L),
          logical(1L)
      ) )
    ) {
      return(
        cc(
         "slot `@fun` must be a function with the first two parameters ",
         "non-optional and all others optional."
      ) )
    }
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
    if(!identical(length(object@compare.err), 1L)) return("slot `@compare.err` must be a 1 length logical")
} )
setClass("unitizerItemTestsErrors",
  representation(
    value="unitizerItemTestError",
    conditions="unitizerItemTestError",
    output="unitizerItemTestError",
    message="unitizerItemTestError",
    aborted="unitizerItemTestError",
    .fail.context="numericOrNULL" # for passing around options for
) )
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
## text to show in that circumstances in `txt.alt`.

setClass("unitizerItemTestsErrorsDiff",
  slots=c(
    diff="DiffOrNULL",
    txt="character",
    txt.alt="character",
    err="logical",
    show.diff="logical"
  ),
  prototype=list(show.diff=TRUE)
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

setGeneric("as.Diffs", function(x, ...) StandardGeneric("as.Diff"))
setMethod("as.Diffs", "unitizerItemTestsErrors",
  function(x, state.ref, state.new, width=getOption("width"), ...) {
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
      diff <- diffObj(
        curr.err@.ref, curr.err@.new, tar.banner=make_cont(".ref"),
        cur.banner=make_cont(".new")
      )
      diffs[[i]] <- new(
        "unitizerItemTestsErrorsDiff", diff=diff, txt=out,
        err=curr.err@compare.err
      )
    }
    invisible(do.call("new", c(list("unitizerItemTestsErrorsDiffs"), diffs)))
  }
)
setMethod("show", "unitizerItemTestsErrorsDiffs",
  function(object) {
    sn <- slotNames(object)
    null.slots <- vapply(sn, function(x) is.null(slot(object, x)), logical(1L))
    if(any(null.slots)) {
      for(i in sn[!null.slots]) show(slot(object, i))
    }
    invisible(object)
} )
setMethod("show", "unitizerItemTestsErrorsDiff",
  function(object) {
    file <- if(object@err) stderr() else stdout()
    meta_word_cat(
      if(object@show.diff) object@txt else object@txt.alt, file=file
    )
    if(object@show.diff) {
    res <- show(object@diff)
    cat("\n")
    }
    invisible(NULL)
} )

## Display Test Errors
##
## Also generates the object that records the diffs and the function output
## for re-display by user.
##
## This is somewhat convoluted.  Better would be to compute the object that has
## all this info, and then use the show method both when we first.
##
#' @rdname unitizer_s4method_doc

setMethod("show", "unitizerItemTestsErrors",
  function(object) {
    if(TRUE) {
      slots <- grep("^[^.]", slotNames(object), value=TRUE)
      slot.errs <- vapply(
        slots, function(x) !is.null(slot(object, x)@value), logical(1L)
      )
      diffs <- text <- vector("list", length(slots))
      errs <- logical(length(slots))
      names(diffs) <- names(text) <- names(errs) <- slots

      for(i in slots[slot.errs]) {
        curr.err <- slot(object, i)
        mismatch <- if(curr.err@compare.err) {
          out.file <- stderr()
          paste0("Unable to compare ", i, ": ")
        } else {
          out.file <- stdout()
          paste0(cap_first(i), " mismatch: ")
        }
        out <- if(length(curr.err@value) < 2L) {
          paste0(mismatch, decap_first(curr.err@value))
        } else {
          c(
            mismatch,
            as.character(
              UL(decap_first(curr.err@value)),
              width=getOption("width") - 2L
          ) )
        }
        meta_word_cat(out, file=out.file)
        make_cont <- function(x) {
          res <- if(identical(i, "value")) {
            as.name(x)
          } else call("$", as.name(toupper(x)), as.name(i))
          call("quote", res)
        }
        diff <- diffObj(
          curr.err@.ref, curr.err@.new, tar.banner=make_cont(".ref"),
          cur.banner=make_cont(".new")
        )
        diffs[[i]] <- new(
          "unitizerItemTestsErrorsDiff", diff=diff, text=out,
          err=out.file==std.err()
        )
        show(diff)
        cat("\n")
      }
    }
    invisible(do.call("new", c(list("unitizerItemTestsErrorsDiffs"), diffs)))
} )

#' Summary Method for unitizerItemTestsErrors Objects
#'
#' DEPRECATED
#'
#' Used to generate the blurb ahead of each failed test with the components
#' that the test failed on.
#'
#' Only intended to be called within the \code{show} method.
#'
#' @param object a \code{unitizerItemTestsErrors} object
#' @return NULL, invisibly
#' @export

setMethod("summary", "unitizerItemTestsErrors",
  function(object, ...) {
    warning("this method is deprecated")
    slots <- grep("^[^.]", slotNames(object), value=TRUE)
    slot.err <- logical(length(slots))
    for(i in seq_along(slots))
      slot.err[[i]] <- !is.null(slot(object, slots[[i]])@value)

    errs <- slots[slot.err]
    if(!length(errs)) return(invisible(NULL))

    if(length(errs) > 1L) {
      err.chr <- paste(
        paste0(head(errs, -1L), collapse=", "), tail(errs, 1L), sep=", and "
      )
      plrl <- "es"
    } else {
      err.chr <- errs
      plrl <- ""
    }
    meta_word_cat(cc(err.chr, " mismatch", plrl, ":"))
    return(invisible(NULL))
} )
#' Like all.equal but Returns "" If Not all.equal
#'
#' Used as the default value comparison function since when values mismatch
#' we use \code{\link{diffObj}} which would make the text output from
#' \code{\link{all.equal}} somewhat redundant.
#'
#' @export all.equal_
#' @param target R object
#' @param current other R object to be compared to \code{target}
#' @param ... arguments to pass to \code{\link{all.equal}}
#' @return TRUE if \code{all.equal} returns TRUE, "" otherwise

all.equal_ <- function(target, current, ...)
  if(isTRUE(all.equal(target, current, ...))) TRUE else ""

#' Store Functions for New vs. Reference Test Comparisons
#'
#' \code{`unitizerItemTestsFuns`} contains the functions used to compare the
#' results and side effects of running test expressions.
#'
#' By default, the comparison for each of the \code{`unitizerItem-class`}
#' elements are carried out as follows (i.e. this is what the default
#' \code{`unitizerItemTestsFuns-class`} is populated with)
#' \itemize{
#'   \item value: compared using \code{`\link{all.equal}`}
#'   \item conditions: each item in this list is compared to the corresponding
#'     item in the reference list with \code{`\link{all.equal}`}
#'   \item output: not compared (too variable, e.g. screen widths, etc.)
#'   \item message: not compared (note this is presumably included in
#'   \code{`conditions`})
#'   \item aborted: not compared (also implied in conditions, hopefully)
#' }
#' @seealso \code{`\link{unitizer_sect}`}
#' @rdname unitizerItemTestsFuns
#' @name unitizerItemTestsFuns
#' @export unitizerItemTestsFuns
#' @examples
#' \dontrun{
#' # use `identical` instead of `all.equal` to compare values
#' unitizerItemTestsFuns(value=identical)
#' }

unitizerItemTestsFuns <- setClass(
  "unitizerItemTestsFuns", contains="unitizerItemTests",
  representation(
    value="unitizerItemTestFun",
    conditions="unitizerItemTestFun",
    output="unitizerItemTestFun",
    message="unitizerItemTestFun",
    aborted="unitizerItemTestFun"
  ),
  prototype(
    value=new("unitizerItemTestFun", fun=all.equal_),
    # note this will dispatch all.equal.condition_list
    conditions=new("unitizerItemTestFun", fun=all.equal_),
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

setMethod("initialize", "unitizerItemTestsFuns", function(.Object, ...) {
  dots <- list(...)
  if(!all(err.slots <- names(dots) %in% slotNames(getClass(.Object))))
    stop("Can't initialize invalid slots ", deparse(names(dots)[!err.slots]))
  fun.names <- vapply(substitute(list(...))[-1L], deparse_fun, character(1L))
  if(!all(names(fun.names) %in% names(dots))) stop("Logic Error: contact package maintainer.")
  for(i in names(dots)) {
    slot(.Object, i) <- if(is(dots[[i]], "unitizerItemTestFun")) {
      dots[[i]]
    } else {
      new("unitizerItemTestFun", fun=dots[[i]], fun.name=fun.names[[i]])
  } }
  .Object
} )
