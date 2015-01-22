# - Required Code -------------------------------------------------------------

#' @include conditions.R
#' @include class_unions.R
#' @include text.R
#' @include list.R
#' @include item.R

NULL

#' Virtual Class To Enforce Slots on Subclasses
#'
#' @keywords internal

setClass("unitizerItemTests", contains="VIRTUAL",
  validity=function(object) {
    if(!isTRUE(msg <- all.equal(sort(slotNames(getClass(object))), sort(slotNames("unitizerItemData"))))) {
      return("slots must be defined as ", deparse(slotNames("unitizerItemData")))
    }
    TRUE
  }
)
#' Validates Functions for Use in New vs. Reference Object Comparison
#' @keywords internal

setClass(
  "unitizerItemTestFun",
  representation(fun="function", fun.name="character"),
  validity=function(object) {
    frms <- formals(object@fun)
    frms <- frms[!(names(frms) %in% "...")]
    if(
      length(frms) < 2L |
      any(vapply(head(frms, 2L), function(frm) !(is.symbol(frm) && nchar(as.character(frm)) == 0L), logical(1L))) |
      any(vapply(tail(frms, -2L), function(frm) (is.symbol(frm) && nchar(as.character(frm)) == 0L), logical(1L)))
    ) {
      return("slot `@fun` must be a function with the first two paramaters non-optional and all others optional.")
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

#' Stores Errors from Evaluating New vs. Ref Comparisons
#'
#' There various nested objects involved:
#' \itemize{
#'   \item \code{`unitizerItemTestError`} contains the error produced from a comparison
#'   \item \code{`unitizerItemTestsErrors`} aggregates the errors for each slot of an item
#'   \item \code{`unitizerItemsTestsErrors`} aggregates all the errors for the
#'      \code{`\link{unitizer-class}`} object
#' }
#'
#' @aliases unitizerItemsTestsErrors-class, unitizerItemTestsErrors-class
#' @keywords internal

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
setClass("unitizerItemTestsErrors", contains="unitizerItemTests",
  representation(
    value="unitizerItemTestError",
    conditions="unitizerItemTestError",
    output="unitizerItemTestError",
    message="unitizerItemTestError",
    aborted="unitizerItemTestError"
) )
setMethod("initialize", "unitizerItemTestsErrors",
  function(.Object, ...) {
    dots <- list(...)
    if(!all(s.n <- names(dots) %in% slotNames(.Object))) stop("Unused arguments ", paste0(deparse(names(dots)[!s.n])))
    for(i in seq_along(dots)) if(is.null(dots[[i]])) dots[[i]] <- new("unitizerItemTestError")
    do.call(callNextMethod, c(list(.Object), dots))
} )
setClass(
  "unitizerItemsTestsErrors", contains="unitizerList",
  validity=function(object) {
    tests <- vapply(as.list(object), is, logical(1L), class2="unitizerItemTestsErrors")
    if(!all(tests)) return("\"unitizerItemsTestsErrors\" may only contain objects of class \"unitizerItemTestsErrors\"")
    TRUE
} )
setClassUnion("unitizerItemsTestsErrorsOrLogical", c("unitizerItemsTestsErrors", "logical"))

#' Convert An Error Into Character
#' @keywords internal

setMethod("as.character", "unitizerItemTestsErrors",
  function(x, ...) {
    err.lst <- list()
    for(i in slotNames(x)) {
      if(!is.null(slot(x, i)@value)) err.lst[[i]] <- slot(x, i)
    }
    chr <- character()
    prep <- if(length(err.lst) > 1L) "- " else ""
    for(i in names(err.lst)) {
      err <- err.lst[[i]]@value
      mismatch <- if(err.lst[[i]]@compare.err) {
        paste0(prep, "Unable to compare ", i, ": ")
      } else {
        paste0(prep, cap_first(i), " mismatch: ")
      }
      if(length(err) < 2L) {
        chr <- c(chr, paste0(mismatch, err))
      } else {
        chr <- c(chr, mismatch, paste0("  + ", err))
    } }
    chr
} )
#' Display Test Errors
#' @keywords internal

setMethod("show", "unitizerItemTestsErrors",
  function(object) {
    slots <- slotNames(object)
    for(i in slots) {
      curr.err <- slot(object, i)
      if(is.null(curr.err@value)) next  # No error, so continue
      mismatch <- if(curr.err@compare.err) {
        paste0("Unable to compare ", i, ": ")
      } else {
        paste0("*", i, "* mismatch: ")
      }
      if(length(curr.err@value) < 2L) {
        chr <- paste0(mismatch, decap_first(curr.err@value))
      } else {
        chr <- c(mismatch, paste0("  + ", decap_first(curr.err@value)))
      }
      for(chr.val in chr) word_cat(chr.val, file=stderr())

      make_cont <- function(x)
        if(identical(i, "value")) x else paste0(toupper(x), "$", i)
      make_banner <- function(x) paste("@@", make_cont(x), "@@\n")

      cat(make_banner(".ref"))
      obj_chr_out(obj_capt(curr.err@.ref), extra=make_cont(".ref"), add=FALSE)
      cat(make_banner(".new"))
      obj_chr_out(obj_capt(curr.err@.new), extra=make_cont(".new"))
    }
    invisible(NULL)
} )

setMethod("summary", "unitizerItemTestsErrors",
  function(object, ...) {
    slots <- slotNames(object)
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
    word_cat(
      "unitizer test fails on", err.chr, paste0("mismatch", plrl, ":"),
      file=stderr()
    )
    return(invisible(NULL))
} )
#' Store Functions for New vs. Reference Test Comparisons
#'
#' \code{`unitizerItemTestsFuns`} contains the functions used to compare the results
#' and side effects of running test expresssions.
#'
#' By default, the comparison for each of the \code{`unitizerItem-class`} elements
#' are carried out as follows (i.e. this is what the default
#' \code{`unitizerItemTestsFuns-class`} is populated with)
#' \itemize{
#'   \item value: compared using \code{`\link{all.equal}`}
#'   \item conditions: each item in this list is compared to the corresponding item
#'     in the reference list with \code{`\link{all.equal}`}
#'   \item output: not compared (too variable, e.g. screen widths, etc.)
#'   \item message: not compared (note this is presumably included in \code{`conditions`})
#'   \item aborted: not compared (also implied in conditions, hopefully)
#' }
#' @seealso \code{`\link{unitizer_sect}`}
#' @rdname unitizerItemTestsFuns
#' @export unitizerItemTestsFuns
#' @examples
#' unitizerItemTestsFuns(value=identical)  # use `identical` instead of `all.equal` to compare values

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
    value=new("unitizerItemTestFun", fun=all.equal),
    conditions=new("unitizerItemTestFun", fun=all.equal),  # note this will dispatch all.equal.condition_list
    output=new("unitizerItemTestFun", fun=function(target, current) TRUE),
    message=new("unitizerItemTestFun", fun=function(target, current) TRUE),
    aborted=new("unitizerItemTestFun", fun=function(target, current) TRUE)
) )

#' Ensures Functions are In Correct Format
#'
#' Also, allows the user to specify functions directly instead of having
#' to instantiate \code{`\link{unitizerItemTestFun-class}`} for each function.
#' Finally, recovers the deparsed passed function name.
#' @keywords internal

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
