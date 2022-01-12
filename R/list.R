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

#' @include class_unions.R

NULL

#' S4 Object To Implement Base List Methods
#'
#' Internal \code{unitizer} objects used to manage lists of objects.  The only
#' user facing instance of these objects are \code{\link{conditionList}}
#' objects.  For the most part these objects behave like normal S3 lists.  The
#' list contents are kept in the \code{.items} slot, and the following methods
#' are implemented to make the object mostly behave like a standard R list:
#' \code{[}, \code{[[}, \code{[<-}, \code{[[<-}, \code{as.list}, \code{append},
#' \code{length}, \code{names}, and \code{names<-}.
#'
#' The underlying assumption is that the `.items` slot is a list
#' (or an expression), and that slot is the only slot for which
#' it's order and length are meaningful (i.e. there is no other list
#' or vector of same length as `.items` in a different slot that is
#' supposed to map to `.items`).  This last assumption allows us
#' to implement the subsetting operators in a meaningful manner.
#'
#' The validity method will run \code{validObject} on the first, last, and
#' middle items (if an even number of items, then the middle closer to the
#' first) assuming they are S4 objects.  We don't run on every object to avoid
#' potentially expensive computation on all objects.
#'
#' @name unitizerList
#' @rdname unitizerList
#' @seealso \code{\link{conditionList}}
#' @slot .items a list or expression
#' @slot .pointer integer, used for implementing iterators
#' @slot .seek.fwd logical used to track what direction iterators are going
#' @examples
#' new('unitizerList', .items=list(1, 2, 3))

setClass(
  "unitizerList",
  representation(
    .items="listOrExpression", .pointer="integer", .seek.fwd="logical"
  ),
  prototype(.pointer=0L, .seek.fwd=TRUE),
  validity=function(object) {
    obj.len <- length(object)
    if(!length(object)) return(TRUE)
    idx.to.test <- unique(
      c(1L, max(1L, as.integer(floor(obj.len / 2))), obj.len)
    )
    lapply(
      idx.to.test,
      function(x) if(isS4(object[[x]])) validObject(object[[x]], complete=TRUE)
    )
    TRUE
  }
)
# - Methods -------------------------------------------------------------------

# Compute Number of Items in \code{\link{unitizerList-class}}

#' @rdname unitizer_s4method_doc

setMethod("length", "unitizerList", function(x) length(x@.items))

# Subsetting Method for \code{\link{unitizerList-class}}

#' @rdname unitizer_s4method_doc

setMethod("[", signature(x="unitizerList", i="subIndex", j="missing", drop="missing"),
  function(x, i) {
    x@.items <- x@.items[i]
    x
} )
# Subsetting Method for \code{\link{unitizerList-class}}

#' @rdname unitizer_s4method_doc

setMethod("[[", signature(x="unitizerList", i="subIndex"),
  function(x, i) {
    x@.items[[i]]
} )
# Replace Method for \code{\link{unitizerList-class}}

#' @rdname unitizer_s4method_doc

setReplaceMethod("[", signature(x="unitizerList", i="subIndex"),
  function(x, i, value) {
    pointer.reset <- (
      is.logical(i) && (lt <- sum(which(i) <= x@.pointer)) ||
      is.numeric(i) && (lt <- sum(floor(i) <= x@.pointer)) ||
      is.character(i) && (lt <- sum(match(i, names(x)) <= x@pointer))
    ) && is.null(value)
    x@.items[i] <- if(is(value, "unitizerList")) value@.items else value
    if(pointer.reset) x@.pointer <- x@.pointer - lt
    x
} )
# Replace Method for \code{\link{unitizerList-class}}

#' @rdname unitizer_s4method_doc

setReplaceMethod("[[", signature(x="unitizerList", i="subIndex"),
  function(x, i, value) {
    pointer.reset <- (
      is.numeric(i) && floor(i[[1L]]) <= x@.pointer ||
      is.character(i) && match(i[[1L]], names(x)) <= x@pointer
    ) && is.null(value)
    x@.items[[i]] <- value
    if(pointer.reset) x@.pointer <- x@.pointer - 1L
    x
} )
# Coerce to list by returning items

#' @rdname unitizer_s4method_doc

setMethod("as.list", "unitizerList", function(x, ...) x@.items)
# So that S3 dispatch works
#' @method as.list unitizerList
#' @export

as.list.unitizerList <- function(x, ...) as.list(x, ...)

#' Coerce to expression by returning items coerced to expressions
#'
#' Really only meaningful for classes that implement the \code{.items}
#' slot as an expression, but works for others to the extent
#' \code{.items} contents are coercible to expressions
#'
#' @keywords internal

setMethod("as.expression", "unitizerList", function(x, ...) as.expression(x@.items, ...))

setGeneric("nextItem", function(x, ...) standardGeneric("nextItem"))

#' Iterate through items of a \code{\link{unitizerList}} ObjectJK
#'
#' Extraction process is a combination of steps:
#' \enumerate{
#'   \item Move Internal pointer with \code{nextItem} or \code{prevItem}
#'   \item Retrieve item \code{getItem}
#'   \item Check whether we're done iterating with \code{done}
#' }
#' \code{done} will return TRUE if the pointer is on either the
#' first or last entry depending on what direction you are iterating.
#' If you wish to iterate from the last item forward, you should either
#' \code{reset} with parameter \code{reverse} set to TRUE, or re-order
#' the items.
#'
#' @aliases nextItem,unitizerList-method prevItem,unitizerList-method
#'   getItem,unitizerList-method reset,unitizerList-method,
#'   done,unitizerList-method 
#' @keywords internal
#' @param x a \code{\link{unitizerList}} object
#' @return \code{\link{unitizerList}} for \code{getItem},
#'   an item from the list, which could be anything

setMethod("nextItem", "unitizerList", valueClass="unitizerList",
  function(x) {
    x@.pointer <- x@.pointer + 1L
    x@.seek.fwd <- TRUE
    x
} )
setGeneric("prevItem", function(x, ...) standardGeneric("prevItem"))
setMethod("prevItem", "unitizerList", valueClass="unitizerList",
  function(x) {
    x@.pointer <- x@.pointer - 1L
    x@.seek.fwd <- FALSE
    x
} )
setGeneric("reset", function(x, ...) standardGeneric("reset"))
setMethod("reset", "unitizerList", valueClass="unitizerList",
  function(x, position=NULL) {
    if(
      !is.null(position) && (!is.character(position) ||
      !identical(length(position), 1L) || !(position %in% c("front", "back")))
    ) {
      stop("Argument `position` must be `NULL`, or \"front\" or \"back\"")
    }
    if(is.null(position)) position <- if(x@.seek.fwd) "front" else "back"
    if(identical(position, "front")) {
      x@.seek.fwd <- TRUE
      x@.pointer <- 0L
    } else if (identical(position, "back")) {
      x@.seek.fwd <- FALSE
      x@.pointer <- length(x) + 1L
    } else {
      stop("Internal Error; unexpected `position` argument")  # nocov
    }
    x
} )
setGeneric("getItem", function(x, ...) standardGeneric("getItem"))
setMethod("getItem", "unitizerList",
  function(x) {
    if(!(x@.pointer %in% seq_along(x))) {
      if(x@.pointer %in% c(0L, length(x) + 1L)) {
        if(identical(x@.pointer, 0L) & x@.seek.fwd) {
          stop("Internal pointer for `x` not initialized; initialize with `nextItem`")
        } else if (identical(x@.pointer, length(x) + 1L) & !x@.seek.fwd) {
          stop("Internal pointer for `x` not initialized; initialize with `prevItem`")
        }
        stop(
          "Internal pointer for `x` outside of range for `x`; test for ",
          "this condition with `done`, or reset with `reset`"
        )
      } else {
        stop("Internal pointer for `x` is corrupted")
    } }
    x@.items[[x@.pointer]]
} )
setGeneric("done", function(x, ...) standardGeneric("done"))
setMethod("done", "unitizerList",
  function(x) {
    if(x@.seek.fwd & x@.pointer > length(x)) return(TRUE)
    else if (!x@.seek.fwd & identical(x@.pointer, 0L)) return(TRUE)
    FALSE
} )
#' @export

setGeneric("append")

# Append To a \code{\link{unitizerList}} Object
#
# \code{values} is coerced to list or expression depending on
# type of \code{x} \code{.items} slot.
#
# The resulting object is not tested for validity as this is too expensive
# on a regular basis.  You should check validity with \code{validObject}
#
# @param x the object to append to
# @param values the object to append
# @param after a subscript, after which the values are to be appended.

#' @rdname unitizer_s4method_doc

setMethod("append", c("unitizerList", "ANY"),
  function(x, values, after=length(x)) {
    attempt <- try(
      if(is.list(x@.items)) {
        values <- as.list(values)
      } else if (is.expression(x@.items)) {
        values <- as.expression(values)
      }
    )
    if(inherits(attempt, "try-error")) {
      stop("Unable to coerce argument `values` to appropriate type; see previous errors for details.")
    }
    if(!is.numeric(after) || !identical(length(after), 1L) || after < 0) {
      stop("Argument `after` must be a length 1 numeric greater than zero")
    }
    y <- x
    x <- x@.items
    y@.items <- callNextMethod()
    if(y@.pointer > after) y@.pointer <- y@.pointer + length(values)
    # validObject(y) # too expensive, commented
    y
} )
## Concatenate to a \code{\link{unitizerList}}

#' @rdname unitizer_s4method_doc

setMethod("c", c("unitizerList"),
  function(x, ..., recursive=FALSE) {
    stop("This method is not implemented yet")  # nocov
} )
# Append Factors
#
# Note this is not related to \code{\link{append,unitizerList,ANY-method}}
# except in as much as it is the same generic, so it just got thrown in here.
#
# @keywords internal

#' @rdname unitizer_s4method_doc

setMethod("append", c("factor", "factor"),
  function(x, values, after=length(x)) {
    if(!identical(attributes(x), attributes(values))) NextMethod()
    if(
      !is.numeric(after) || round(after) != after || length(after) != 1L ||
      after > length(x) || after < 0L
    ) stop("Argument after must be integer like between 0 and length(x)")
    if(!length(values)) return(x)
    len.x <- length(x)
    length(x) <- length(x) + length(values)
    if(after < len.x) {
      x[(after + 1L + length(values)):length(x)] <- x[(after + 1L):(len.x)]
    }
    x[(after + 1L):(after + length(values))] <- values
    x
  }
)
#' @rdname unitizer_s4method_doc

setMethod("names", "unitizerList", function(x) names(x@.items))

#' @rdname unitizer_s4method_doc

setReplaceMethod("names", "unitizerList",
  function(x, value) {
    names(x@.items) <- value
    x
  }
)
