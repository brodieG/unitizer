# - Required Code -------------------------------------------------------------

#' @include misc.R
#' @include class_unions.R
#' @include list.R
#' @include item.R

NULL

#' Virtual Class To Enforce Slots on Subclasses
#' 
#' Used for \code{`\link{testorItemsTestFuns}`} and \code{`\link{testorItemTestsErrors}`}
#' @keywords internal

setClass("testorItemTests", contains="VIRTUAL",
  validity=function(object) {
    if(!isTRUE(msg <- all.equal(sort(slotNames(getClass(object))), sort(slotNames("testorItemData"))))) {
      return("slots must be defined as ", deparse(slotNames("testorItemData")))
    }
    TRUE
  }
)
#' Validates Functions for Use in New vs. Reference Object Comparison
#' @keywords internal

setClass(
  "testorItemTestFun",
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

setMethod("initialize", "testorItemTestFun", function(.Object, ...) {
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
#'   \item \code{`testorItemTestError`} contains the error produced from a comparison
#'   \item \code{`testorItemTestsErrors`} aggregates the errors for each slot of an item
#'   \item \code{`testorItemsTestsErrors`} aggregates all the errors for the 
#'      \code{`\link{testor-class}`} object
#' }
#' 
#' @aliases testorItemsTestsErrors-class, testorItemTestsErrors-class
#' @keywords internal

setClass("testorItemTestError",
  representation(
    value="characterOrNULL",
    compare.err="logical"
  ),
  prototype(value=NULL, compare.err=FALSE),
  validity=function(object) {
    if(!identical(length(object@compare.err), 1L)) return("slot `@compare.err` must be a 1 length logical")
} )
setClass("testorItemTestsErrors", contains="testorItemTests",
  representation(
    value="testorItemTestError",
    conditions="testorItemTestError",
    output="testorItemTestError",
    message="testorItemTestError",
    aborted="testorItemTestError"
) )
setMethod("initialize", "testorItemTestsErrors",
  function(.Object, ...) {
    dots <- list(...)
    if(!all(s.n <- names(dots) %in% slotNames(.Object))) stop("Unused arguments ", paste0(deparse(names(dots)[!s.n])))
    for(i in seq_along(dots)) if(is.null(dots[[i]])) dots[[i]] <- new("testorItemTestError")
    do.call(callNextMethod, c(list(.Object), dots))
} )
setClass(
  "testorItemsTestsErrors", contains="testorList",
  validity=function(object) {
    tests <- vapply(as.list(object), is, logical(1L), class2="testorItemTestsErrors")
    if(!all(tests)) return("\"testorItemsTestsErrors\" may only contain objects of class \"testorItemTestsErrors\"")
    TRUE
} )
#' Convert An Error Into Character
#' @keywords internal

setMethod("as.character", "testorItemTestsErrors", 
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
#' Store Functions for New vs. Reference Test Comparisons
#' 
#' \code{`testorItemTestsFuns`} contains the functions used to compare the results
#' and side effects of running test expresssions.
#' 
#' By default, the comparison for each of the \code{`testorItem-class`} elements
#' are carried out as follows (i.e. this is what the default 
#' \code{`testorItemTestsFuns-class`} is populated with)
#' \itemize{
#'   \item value: compared using \code{`\link{all.equal}`}
#'   \item conditions: each item in this list is compared to the corresponding item
#'     in the reference list with \code{`\link{all.equal}`}
#'   \item output: not compared (too variable, e.g. screen widths, etc.)
#'   \item message: not compared (note this is presumably included in \code{`conditions`})
#' }
#' @seealso \code{`\link{testorTest-class}`}
#' @examples
#' new("testorItemTestsFuns", value=identical)  # use `identical` instead of `all.equal` to compare values

setClass(
  "testorItemTestsFuns", contains="testorItemTests",
  representation(
    value="testorItemTestFun",
    conditions="testorItemTestFun",
    output="testorItemTestFun",
    message="testorItemTestFun",
    aborted="testorItemTestFun"
  ),
  prototype(
    value=new("testorItemTestFun", fun=all.equal),
    conditions=new("testorItemTestFun", fun=compare_conditions),
    output=new("testorItemTestFun", fun=function(target, current) TRUE),
    message=new("testorItemTestFun", fun=function(target, current) TRUE),
    aborted=new("testorItemTestFun", fun=identical)
) )
#' Ensures Functions are In Correct Format
#' 
#' Also, allows the user to specify functions directly instead of having
#' to instantiate \code{`\link{testorItemTestFun-class}`} for each function.
#' Finally, recovers the deparsed passed function name.
#' @keywords internal

setMethod("initialize", "testorItemTestsFuns", function(.Object, ...) {
  dots <- list(...)
  if(!all(err.slots <- names(dots) %in% slotNames(getClass(.Object)))) 
    stop("Can't initialize invalid slots ", deparse(names(dots)[!err.slots]))
  fun.names <- vapply(substitute(list(...))[-1L], deparse_fun, character(1L))
  if(!all(names(fun.names) %in% names(dots))) stop("Logic Error: contact package maintainer.")
  for(i in names(dots)) {
    slot(.Object, i) <- if(is(dots[[i]], "testorItemTestFun")) {
      dots[[i]]
    } else {
      new("testorItemTestFun", fun=dots[[i]], fun.name=fun.names[[i]])
  } }
  .Object
} )