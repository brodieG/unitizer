# Copyright (C) 2020  Brodie Gaslam
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

#' @include item.R
#' @include item.sub.R
#' @include class_unions.R

NULL

#' Contains Representation For a Section of Tests
#'
#' \code{unitizerSectionExpression-class} contains the actual expressions that
#' belong to the section, whereas \code{unitizerSection-class} only contains
#' the meta data.  The latter objects are used within \code{]unitizer-class},
#' whereas the former is really just a temporary object until we can generate
#' the latter.
#'
#' \code{unitizerSectionNA-class} is a specialized section for tests that actually
#' don't have a section (removed tests that are nonetheless chosen to be kept
#' by user in interactive environment)
#'
#' @keywords internal
#' @aliases unitizerSectionExpression-class unitizerSectionNA-class
#' @slot title 1 lenght character, the name of the section
#' @slot details character vector containing additional info on the section
#' @slot compare functions to compare the various aspects of a
#'   \code{unitizerItem-class} @slot length tracks size of the section

setClass(
  "unitizerSection",
  representation(
    title="character",
    details="character",
    compare="testFuns",
    length="integer",
    parent="integer"
  ),
  prototype(parent=NA_integer_, length=0L),
  validity=function(object) {
    if(length(object@title) != 1L) return("slot `@title` must be length 1")
    if(length(object@length) != 1L | object@length < 0L) {
      return("slot `@length` must be length 1 and >= 0")
    }
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
setClass(
  "unitizerSectionNA", contains="unitizerSection",
  prototype=list(
    title="<untitled>", details="Dummy section for section-less tests."
  )
)
setClass("unitizerSectionExpression", contains="unitizerList",
  representation(
    title="characterOrNULL",
    details="character",
    compare="testFuns"
  )
)
setClassUnion("unitizerSectionExpressionOrExpression", c("unitizerSectionExpression", "unitizerSection", "expression"))

#' Compute Length of a \code{unitizerSection-class}
#'
#' @keywords internal
#' @param x a \code{unitizerSection} object

setMethod("length", "unitizerSection", function(x) x@length)

#' Define a \code{unitizer} Section
#'
#' The purpose of \code{unitizer} sections is to allow the user to tag a
#' group of test expressions with meta information as well as to modify
#' how tests are determined to pass or fail.
#'
#' @section Tested Data:
#'
#' \code{unitizer} tracks the following:
#' \itemize{
#'   \item value: the return value of the test
#'   \item conditions: any conditions emitted by the test (e.g. warnings or
#'     errors)
#'   \item output: screen output
#'   \item message: stderr output
#'   \item aborted: whether the test issued an `abort` restart (e.g. by calling
#'     `stop` directly or indirectly)
#' }
#' In the future stdout produced by the test expression itself may be captured
#' separately from that produced by print/showing of the return value, but at
#' this point the two are combined.
#'
#' Each of the components of the test data can be tested, although by default
#' only \code{value} and \code{condition} are checked.  Testing \code{output} is
#' potentially duplicative of testing \code{value}, since most often
#' \code{value} is printed to screen and the screen output of the value closely
#' correlates to the actual value.  In some cases it is useful to explicitly
#' test the \code{output}, such as when testing \code{print} or \code{show}
#' methods.
#'
#' @section Comparison Functions:
#'
#' The comparison function should accept at least two parameters, and
#' require no more than two.  For each test component, the comparison function
#' will be passed the reference data as the first argument, and the newly
#' evaluated data as the second.  The function should return TRUE if the
#' compared test components are considered equivalent, or FALSE.  Instead of
#' FALSE, the function may also return a character vector describing the
#' mismatch, as \code{\link{all.equal}} does.
#'
#' \bold{WARNING}: Comparison functions that set and/or unset \code{\link{sink}}
#' can potentially cause problems.  If for whatever reason you must really  sink
#' and unsink output streams, please take extreme care to restore the streams to
#' the state they were in when the comparison function was called.
#'
#' Any output to \code{stdout} or \code{stderr} is captured and only checked at
#' the end of the \code{unitizer} process with the expectation that there will
#' be no such output.
#'
#' \code{value} and \code{conditions} are compared with \code{\link{all_eq}},
#' which is a wrapper to \code{\link{all.equal}} except that it returns FALSE
#' instead of a descriptive string on failure.  This is because \code{unitizer}
#' will run \code{\link[diffobj]{diffObj}} on the test data components that do
#' not match and including the \code{all.equal} output would be redundant.
#'
#' If a comparison function signals a condition (e.g. throws a warning) the
#' test will not be evaluated, so make sure that your function does not signal
#' conditions unless it is genuinely failing.
#'
#' If you wish to provide custom comparison functions you may do so by passing
#' an appropriately initialized \code{\link{testFuns}} object as the
#' value to the \code{compare} parameter to \code{unitizer_sect}
#' (see examples).
#'
#' Make sure your comparison functions are available to \code{\link{unitize}}.
#' Comparisons will be evaluated in the environment of the test.  By default
#' \code{\link{unitize}} runs tests in environments that are not children to
#' the global environment, so functions defined there will not be automatically
#' available.  You can either specify the function in the test file before the
#' section that uses it, or change the base environment tests are evaluated in
#' with \code{unitize(..., par.env)}, or make sure that the package that
#' contains your function is loaded within the test script.
#'
#' @section Nested Sections:
#'
#' It is possible to have nested sections, but titles, etc. are ignored.  The
#' only effect of nested sections is to allow you to change the comparison
#' functions for a portion of the outermost \code{unitizer_sect}.
#'
#' @note if you want to modify the functions used to compare conditions,
#' keep in mind that the conditions are stored in \code{\link{conditionList}}
#' objects so your function must loop through the lists and compare conditions
#' pairwise.  By default \code{unitizer} uses the \code{all.equal} method for S4
#' class \code{conditionList}.
#'
#' @note \code{untizer} does not account for sections when matching new and
#' reference tests.  All tests will be displayed as per the section they belong
#' to in the newest version of the test file, irrespective of what section they
#' were in when the tests were last run.
#'
#' @note Calls to \code{unitizer_sect} should be at the top level of your test
#' script, or nested within other \code{unitizer_sect}s (see "Nested Sections").
#' Do not expect code like \code{(untizer_sect(..., ...))} or
#' \code{{unitizer_sect(..., ...)}} or \code{fun(unitizer_sect(..., ...))} to
#' work.
#'
#' @export
#' @seealso \code{\link{testFuns}}, \code{\link{all_eq}}
#' @param title character 1 length title for the section, can be omitted
#'   though if you do omit it you will have to refer to the subsequent
#'   arguments by name (i.e. \code{unitizer_sect(expr=...)})
#' @param expr test expression(s), most commonly a call to \code{{}} with
#'   several calls inside (see examples)
#' @param details character more detailed description of what the purpose
#'   of the section is; currently this doesn't do anything.
#' @param compare a function or a \code{\link{testFuns}} object
#' @examples
#' unitizer_sect("Switch to `all.equal` instead of `all_eq`",
#'   {
#'     fun(6L)
#'     fun("hello")
#'   },
#'   compare=testFuns(value=all.equal, conditions=all.equal)
#' )
#' unitizer_sect("Use identical for ALL test data, including stdout, etc.",
#'   {
#'     fun(6L)
#'     fun("hello")
#'   },
#'   compare=identical
#' )
unitizer_sect <- function(
  title=NULL, expr=expression(), details=character(),
  compare=new("testFuns")
) {
  if(!is(compare, "testFuns") && !is.function(compare))
    stop("Argument `compare` must be \"testFuns\" or a function")
  if(!is.character(details)) stop("Argument `details` must be character")
  if(!is.null(title) && (!is.character(title) || length(title) != 1L)) 
    stop("Argument `title` must be a 1 length character vector.")
  exp.sub <- substitute(expr)
  if(is.call(exp.sub) && is.symbol(exp.sub[[1L]])) expr <- exp.sub
  if(is.call(expr)) {
    if(identical(expr.sub.eval <- eval(expr[[1L]], parent.frame()), base::"{")) {
      expr <- do.call(expression, as.list(expr[-1L]))
    } else if (identical(expr.sub.eval, base::expression)) {
      expr <- eval(expr, parent.frame())
    }
  }
  if(!is.expression(expr)) {
    stop(
      "Argument `expr` must be an expression, or an unevaluated call that ",
      "evaluates to an expression or `{`."
    )
  }
  if(!is(compare, "testFuns")) {
    fun.name <- deparse_fun(substitute(compare))
    if(!isTRUE(err.fun <- is.two_arg_fun(compare))) {
      stop(
        "Argument `compare`, if a function, must accept two arguments and ",
        "require no more than two (", err.fun, ")"
      )
    }
    compare <- new(
        "testFuns",
        value=new("unitizerItemTestFun", fun=compare, fun.name=fun.name)
    )
  }
  if(length(expr) < 1L) {
    warning("`unitizer_sect` \"", strtrunc(title, 15), "\" is empty.")
    return(NULL)
  }
  attempt <- try(new("unitizerSectionExpression", title=title, .items=expr, details=details, compare=compare))
  if(inherits(attempt, "try-error")) stop("Failed instantiating `unitizerSection`; see previous error for details.")
  attempt
}
