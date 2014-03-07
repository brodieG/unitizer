#' @include list.R
#' @include class_unions.R
#' @include section.R

NULL

#' Collections of Calls For Testing
#' 
#' Should probably add an \code{`as.testorTests`} function...

setClass("testorTests", contains="testorList")
setClassUnion("testorTestsOrExpression", c("testorTests", "expression"))


#' Add More Tests to \code{`\link{testorTests}`}
#' 
#' Will overwrite the call at the current index with the contents of the
#' expression passed as the \code{`e2`} argument, unless current index is 0L
#' in which case will just append the expressions.  The intended use for this
#' is when the evaluation of one of the tests produces a 
#' \code{`\link{testorSectionExpression-class}`} that should then replace the
#' call.
#' 
#' The index of the \code{`\link{testorTests}`} object will be set so that 
#' the next call to \code{`\link{nextItem,testorList-method}`} will return the first
#' call that was added.
#' 
#' @keywords internal
#' @return testorTests-class

setMethod("+", c("testorTests", "testorSectionExpressionOrExpression"), valueClass="testorTests",
  function(e1, e2) {
    if(e1@.pointer > length(e1) | e1@.pointer < 0L) stop("Logic Error: invalid internal index value ", e1@.pointer)
    if(e1@.pointer > 0L) {
      e1@.items[[e1@.pointer]] <- NULL
      e1@.pointer <- e1@.pointer - 1L
    }
    append(e1, e2, after=e1@.pointer)
} )

# HOW DO WE HANDLE COMMENT ADDITION AT THIS STEP?  KEEP A PARALLEL
# .items.comments VECTOR, SEEMS LIKE THIS IS WHAT WE WANT TO DO, THEN
# WHEN ADDING TO TESTOR, THE COMMENTS WILL GET INJECTED INTO THE ITEMS
# ACTUALLY, DO THIS THROUGH AN ATTRIBUTE ASSIGNED TO EACH ELEMENT IN
# THE EXPRESSION, WHICH WILL THEN GET ASSIGNED TO THE ITEM ON
# EVALUATION
