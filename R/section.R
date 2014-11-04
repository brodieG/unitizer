#' @include item.R
#' @include item.sub.R
#' @include class_unions.R

NULL

#' Contains Representation For a Section of Tests
#'
#' \code{`unitizerSectionExpression-class`} contains the actual expressions that
#' belong to the section, whereas \code{`unitizerSection-class`} only contains
#' the meta data.  The latter objects are used within \code{`\link{unitizer-class}`},
#' whereas the former is really just a temporary object until we can generate
#' the latter.
#'
#' @keywords internal
#' @aliases unitizerSectionExpression-class
#' @slot title 1 lenght character, the name of the section
#' @slot details character vector containing additional info on the section
#' @slot compare functions to compare the various aspects of a \code{`\link{unitizerItem-class}`}
#' @slot length tracks size of the section

setClass(
  "unitizerSection",
  representation(
    title="character",
    details="character",
    compare="unitizerItemTestsFuns",
    length="integer",
    parent="integer"
  ),
  prototype(parent=NA_integer_),
  validity=function(object) {
    if(length(object@title) != 1L) return("slot `@title` must be length 1")
    if(length(object@length) != 1L | object@length < 1L) return("slot `@length` must be length 1 and greater than 0")
    if(length(object@parent) != 1L) return("slot `@parent` must be a 1 length integer")
  }
)
setMethod("initialize", "unitizerSection",
  function(.Object, ...) {
    if(!("title" %in% (dot.names <- names(list(...))))) {
      return(callNextMethod(.Object, title="<untitled>", ...))
    } else if(is.null(list(...)$title)) {
      return(do.call(callNextMethod, c(list(.Object, title="<untitled>"), list(...)[dot.names != "title"])))
    }
    callNextMethod()
} )
setClass("unitizerSectionExpression", contains="unitizerList",
  representation(
    title="characterOrNULL",
    details="character",
    compare="unitizerItemTestsFuns"
  )
)
setClassUnion("unitizerSectionExpressionOrExpression", c("unitizerSectionExpression", "unitizerSection", "expression"))

#' Compute Length of a \code{`\link{unitizerSection-class}`}
#'
#' @keywords internal
#' @param x a \code{`unitizerSection`} object

setMethod("length", "unitizerSection", function(x) x@length)


#' Extract Reference Section Data
#'
#' Using one unitizer with existing new items, and another unitizer that we
#' just created from it by pulling out the tests we intend to keep, recreate
#' the sections for the tests we intend to keep.
#'
#' This isn't super robust as we're not ensuring that the two unitizers used
#' here are related in any way.  Would be better to have something that does
#' this properly...
#'
#' @param x the new unitizer that will be stored with the reference tests
#' @param y the unitizer that will be used to generate the sections

setGeneric("refSections", function(x, y) standardGeneric("refSections"))
setMethod("refSections", c("unitizer", "unitizer"), valueClass="unitizer"
  function(x, y) {
    if(!length(x@items.ref)) return(x)

    sections.ref.ids <- vapply(as.list(x@items.ref), slot, 1L, "section.id")
    sections.unique <- Filter(Negate(is.na), sort(unique(sections.ref.ids)))
    if(!all(sections.unique %in% seq_along(y@sections)))
      stop("Logic Error: reference tests referencing non-existing sections in original; contact maintainer")

    sects <- y@sections[sections.unique]
    sects.map <- ifelse(
      is.na(sections.ref.ids),
      max(sections.unique) + 1L,
      rank(sections.ref.ids)
    )
    if(na.sects <- sum(is.na(sections.ref.ids))) {
      na.sect <- new(
        "unitizerSection", length=na.sects,
        details="Dummy section for section-less tests."
      )
      sects <- c(sects, list(na.sect))
    }
    x@sections.ref <- sects
    x@section.ref.map <- sects.map

    x
  }
)
#' Define a \code{`unitizer`} Section
#'
#' The purpose of \code{`unitizer`} sections is to allow the user to tag a
#' group of test expressions with meta information as well as to modify the
#' comparison functions used when determining whether the newly evaluated
#' values match the reference values.
#'
#' \code{`unitizer`} will compare values as well as some side effects from
#' the test expression evaluation.  If you wish to modify the comparison function
#' for the value of the test expressions then all you need to do is pass your
#' comparison function as the \code{`compare`} argument.
#'
#' If you wish to modify the comparison functions for the side effects (e.g.
#' screen output or conditions), then you need to pass a
#' \code{`\link{unitizerItemTestsFuns-class}`} object intialized with the
#' appropriate functions (see example).
#'
#' It is possible to have nested sections, but titles, etc. are ignored.  The
#' only effect of sub-sections is to allow you to change the comparison
#' functions for a sub-section of a \code{`unitizer_sect`}.
#'
#' @note if you want to modify the functions used to compare conditions,
#' keep in mind that the conditions are stored in lists, so your function
#' must loop through the lists and compare conditions pairwise.  By default
#' \code{`unitizer`} uses the \code{`all.equal`} method for S4 class \code{`conditionList`}.
#'
#' @note currently sections have no impact whatsoever on reference expressions.
#' The only thing that matters is what section the new expressions are in.
#' New expressions are matched to reference expressions based purely on the
#' deparsed calls irrespective of what section the reference expressions were
#' in.
#'
#' @export
#' @param title character 1 length title for the section, can be omitted
#'   though if you do omit it you will have to refer to the subsequent
#'   arguments by name (i.e. \code{`unitizer_sect(expr=...)`})
#' @param expr test expression(s), most commonly a call to \code{`{}`} with
#'   several calls inside (see examples)
#' @param details character more detailed description of what the purpose
#'   of the section is; currently this doesn't do anything.
#' @param compare a function or a \code{`\link{unitizerItemTestsFuns-class}`}
#'   object
#' @examples
#' unitizer_sect("Custom Tests", {
#'   my_fun("a", FALSE)
#'   my_fun(845, TRUE)
#' })
#' unitizer_sect("Compare With Identical",
#'   {
#'     my_exact_fun(6L)
#'     my_exact_fun("hello")
#'   },
#'   compare=identical
#' )
#' unitizer_sect("Compare With Identical",
#'   {
#'     my_exact_fun(6L)
#'     my_exact_fun("hello")
#'   },
#'   compare=identical
#' )
#' unitizer_sect("Compare With Identical For Screen Output",
#'   {
#'     my_exact_fun(6L)
#'     my_exact_fun("hello")
#'   },
#'   compare=unitizerItemTestsFuns(value=identical, output=identical)
#' )
unitizer_sect <- function(title=NULL, expr=expression(), details=character(), compare=new("unitizerItemTestsFuns")) {
  if(!is(compare, "unitizerItemTestsFuns") & !is.function(compare)) stop("Argument `compare` must be \"unitizerItemTestsFuns\" or a function")
  if(!is.character(details)) stop("Argument `details` must be character")
  if(!is.null(title) && (!is.character(title) || length(title) != 1L)) stop("Argument `title` must be a 1 length character vector.")
  exp.sub <- substitute(expr)
  if(is.call(exp.sub) && is.symbol(exp.sub[[1L]])) expr <- exp.sub
  if(is.call(expr)) {
    if(identical(expr.sub.eval <- eval(expr[[1L]], parent.frame()), base::"{")) {
      expr <- do.call(expression, as.list(expr[-1L]))
    } else if (identical(expr.sub.eval, base::expression)) {
      expr <- eval(expr, parent.frame())
    }
  }
  if (!is.expression(expr)) {
    stop("Argument `expr` must be an expression, or an unevaluated call that evaluates to an expression or `{`.")
  }
  if(!is(compare, "unitizerItemTestsFuns")) {
    if(is.function(compare)) {
      compare <- try(
        new(
          "unitizerItemTestsFuns",
          value=new("unitizerItemTestFun", fun=compare, fun.name=deparse_fun(substitute(compare)))
      ) )
      if(inherits(compare, "try-error")) {
        stop("Problem with provided function for argument `compare`; see previous errors for details")
      }
    } else stop("Logic Error: contact package maintainer.")
  }
  if(length(expr) < 1L) {
    warning("`unitizer_sect` \"", strtrunc(title, 15), "\" is empty.")
    return(NULL)
  }
  attempt <- try(new("unitizerSectionExpression", title=title, .items=expr, details=details, compare=compare))
  if(inherits(attempt, "try-error")) stop("Failed instantiating `unitizerSection`; see previous error for details.")
  attempt
}
