#' Contains A List of Conditions
#'
#' Used by \code{`unitizer`} to capture conditions emitted by tests.
#'
#' When submitting custom comparison functions with:
#'
#' \code{`unitizer_sect(..., compare=unitizerItemTestsFuns(...))`}
#'
#' functions that compare conditions must compare \code{`conditionList`} objects
#'
#' @note Implemented as an S4 class to avoid \code{`setOldClass`} and apparent
#' compatibility issues.
#'
#' @seealso \code{`\link{unitizer_sect}`}
#' @export

setClass("conditionList", contains="unitizerList")

#' Compare Lists of Conditions
#'
#' This is the default function for comparing conditions across the new and
#' reference runs of a test.  Comparison will fail if any of the conditions
#' are not all.equal.  Additionally, we check for the special "printed"
#' attribute on each conditions which indicate that the condition occurred
#' in the print/show methods as applied to the result of a call.
#'
#' @keywords internal
#' @export
#' @param target the list of conditions that we are matching against
#' @param current the list of conditions we are checking
#' @return TRUE if the lists of conditions are equivalent, an character vector explaining
#'   why they are not otherwise

setMethod("all.equal", "conditionList",
  function(target, current, ...) {
    if(
      !all(vapply(as.list(target), inherits, FALSE, "condition")) ||
      !all(vapply(as.list(current), inherits, FALSE, "condition"))
    ) return("`target` or `current` are not both lists of conditions")

    print.show.err <- paste0(
      "Condition mismatch may involve print/show methods; carefully review ",
      "conditions with `.NEW$conditions` and `.REF$conditions` as just ",
      "typing `.ref` or `.new` at the prompt will invoke print/show methods, ",
      "which themselves may be the cause of the mismatch."
    )
    if(length(target) != length(current)) {
      return(
        c(
          paste0(
            "`target` (a.k.a `.ref`) and `current` (a.k.a `.new`) do not have ",
            "the same number of conditions (",length(target), " vs ",
            length(current), ")"
          ),
          if(
            any(
              unlist(lapply(as.list(append(target, current)), attr, "printed")
            ) )
          ) {
            print.show.err
          }
    ) ) }
    cond.len <- min(length(target), length(current))

    res <- lapply(
      seq(len=cond.len),
      function(x) {
        target.printed <- isTRUE(attr(target[[x]], "printed"))
        current.printed <- isTRUE(attr(current[[x]], "printed"))
        if(!is.null(attr(target[[x]], "printed"))) attr(target[[x]], "printed") <- NULL
        if(!is.null(attr(current[[x]], "printed"))) attr(current[[x]], "printed") <- NULL

        err.msg <- all.equal(target[[x]], current[[x]])
        if(!isTRUE(err.msg) && (target.printed || current.printed)) {
          err.msg <- c(err.msg, print.show.err)
        }
        err.msg
    } )
    errs <- which(vapply(res, is.character, logical(1L)))
    if((err.len <- length(errs)) == 1L) {
      err.msg <- "There is 1 condition mismatch; "
    } else  {
      err.msg <- paste0("There are ", err.len, " condition mismatches; ")
    }
    if(err.len) {
      return(
        c(
          paste0(err.msg, "showing first mismatch at condition #", errs[[1L]]),
          res[[errs[[1L]]]]
    ) ) }
    if(all(unlist(res))) return(TRUE)
    stop("Logic Error, unexpected return values from comparison function.")
} )
#' Compare Conditions
#'
#' @keywords internal
#' @export
#' @param target a condition
#' @param current another condition to compare
#' @return TRUE if conditions match, character vector describing differences otherwise

all.equal.condition <- function(target, current, ...) {
  if(!inherits(target, "condition") || !inherits(current, "condition"))
    return("One of `target` or `current` is not a condition")
  if(
    !identical(
      type.targ <- get_condition_type(target),
      type.curr <- get_condition_type(current)
    )
  ) {
    return(
      paste0(
        "Condition type mismatch, `target` (a.k.a. `.ref`) is '", type.targ,
        "', but `current` (a.k.a. `.new`) is '", type.curr, "'"
  ) ) }
  if(!isTRUE(all.equal(conditionMessage(target), conditionMessage(current))))
    return(paste0(type.targ, " condition messages do not match"))
  if(!isTRUE(all.equal(conditionCall(target), conditionCall(current))))
    return(paste0(type.targ, " condition calls do not match"))
  TRUE
}
#' Prints A list of Conditions
#'
#' @keywords internal
#' @export
#' @param object a \code{`conditionList`} object (list of conditions)
#' @return object, invisibly

setMethod("show", "conditionList",
  function(object) {
    width=getOption("width")
    if(!length(object)) {
      cat("Empty condition list\n")
      return(invisible(object))
    }
    out <- paste0(
      format(seq_along(object)), ": ",
      ifelse(
        print.show <- vapply(as.list(object), function(y) isTRUE(attr(y, "printed")), logical(1L)),
        "[print] ", ""
      ),
      vapply(as.list(object), get_condition_type, character(1L)),
      " in "
    )
    desc.chars <- max(width - nchar(out), 20L)
    cond.detail <- vapply(as.list(object), FUN.VALUE=character(1L),
      function(y) {
        paste0(deparse(conditionCall(y))[[1L]], " : ", conditionMessage(y))
    } )
    out <- paste0(out, substr(cond.detail, 1, desc.chars))
    if(any(print.show)) {
      out <- c(out, "[print] means condition was issued in print/show method rather than in actual evaluation.")
    }
    cat(out, sep="\n")
    return(invisible(object))
} )
#' Extracts Condition Type From Condition Classes
#'
#' Type (e.g. Error, Warning), is taken to be the second to last class.
#'
#' @keywords internal
#' @param x a condition
#' @return character 1 length the type of condition

get_condition_type <- function(x) {
  if(!inherits(x, "condition")) stop("Argument `x` must be a condition")
  classes <- rev(class(x))
  if(length(classes) < 2L || classes[[1L]] != "condition") "Unknown"
  cap_first(classes[[2L]])
}
