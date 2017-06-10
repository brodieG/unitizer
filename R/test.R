#' @include list.R
#' @include class_unions.R
#' @include section.R
#' @include item.R

NULL

#' Collections of Calls For Testing
#'
#' Should probably add an \code{as.unitizerTests} function...
#' @keywords internal

setClass("unitizerTests", contains="unitizerList")
setClassUnion("unitizerTestsOrExpression", c("unitizerTests", "expression"))

# Add More Tests to \code{`\link{unitizerTests-class}`}
#
# Will overwrite the call at the current index with the contents of the
# expression passed as the \code{`e2`} argument, unless current index is 0L
# in which case will just append the expressions.  The intended use for this
# is when the evaluation of one of the tests produces a
# \code{`\link{unitizerSectionExpression-class}`} that should then replace the
# call.
#
# The index of the \code{`\link{unitizerTests-class}`} object will be set so that
# the next call to \code{`\link{nextItem,unitizerList-method}`} will return the first
# call that was added.
#
# @keywords internal
# @return unitizerTests-class

#' @rdname unitizer_s4method_doc

setMethod("+",
  c("unitizerTests", "unitizerSectionExpressionOrExpression"),
  valueClass="unitizerTests",
  function(e1, e2) {
    if(e1@.pointer > length(e1) | e1@.pointer < 0L)
      # nocov start
      stop("Internal Error: invalid internal index value ", e1@.pointer)
      # nocov end
    if(e1@.pointer > 0L) {
      e1@.items[[e1@.pointer]] <- NULL
      e1@.pointer <- e1@.pointer - 1L
    }
    append(e1, e2, after=e1@.pointer)
} )
# Create Template Matrix

tests_result_mat <- function(rows=0) {
  if(!is.numeric(rows) || length(rows) != 1L || rows < 0)
    stop("Argument `rows` must be numeric(1L) >= 0")
  col.names <- slotNames("unitizerItemData")
  res <- matrix(logical(0L), ncol=length(col.names), nrow=rows)
  colnames(res) <- col.names
  res[] <- FALSE
  res
}
