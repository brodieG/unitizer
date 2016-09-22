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
#' @export
#' @param target the list of conditions that we are matching against
#' @param current the list of conditions we are checking
#' @param ... provided for compatibility with generic
#' @return TRUE if the lists of conditions are equivalent, an character vector explaining
#'   why they are not otherwise

setMethod("all.equal", "conditionList",
  function(target, current, ...) {
    if(
      !all(vapply(as.list(target), inherits, FALSE, "condition")) ||
      !all(vapply(as.list(current), inherits, FALSE, "condition"))
    ) return("`target` or `current` are not both lists of conditions")

    if(length(target) != length(current)) {
      return(
        paste0(
          "Condition count mismatch; expected ",length(target), " (got ",
          length(current), ")"
    ) ) }
    cond.len <- min(length(target), length(current))

    res <- lapply(
      seq(len=cond.len), function(x) all.equal(target[[x]], current[[x]])
    )
    errs <- which(vapply(res, Negate(isTRUE), logical(1L)))
    if(!(err.len <- length(errs))) {
      return(TRUE)
    } else if (err.len == 1) {
      err.msg <- paste0(
        "There is one condition mismatch at index [[", errs, "]]"
      )
    } else {
      err.msg <- paste0(
        "There are ", err.len, " condition mismatches, first one at index [[",
        errs[[1]],"]]"
      )
    }
    if(err.len) return(err.msg)
} )
# So that S3 dispatch works
#' @export

all.equal.conditionList <- function(target, current, ...)
  all.equal(target, current, ...)

#' Compare Conditions
#'
#' @export
#' @param target a condition
#' @param current another condition to compare
#' @param ... provided for compatibility with generic
#' @return TRUE if conditions match, character vector describing differences
#'   otherwise

all.equal.condition <- function(target, current, ...) {
  if(!inherits(target, "condition") || !inherits(current, "condition"))
    return("One of `target` or `current` is not a condition")

  target.printed <- isTRUE(attr(target, "unitizer.printed"))
  current.printed <- isTRUE(attr(current, "unitizer.printed"))

  if(!is.null(attr(target, "unitizer.printed")))
    attr(target, "unitizer.printed") <- NULL
  if(!is.null(attr(current, "unitizer.printed")))
    attr(current, "unitizer.printed") <- NULL

  err.msg <- character()
  if(
    !identical(
      type.targ <- get_condition_type(target),
      type.curr <- get_condition_type(current)
    )
  ) {
    err.msg <- paste0(
      "Condition type mismatch, `target` is '", type.targ,
      "', but `current` is '", type.curr, "'"
    )
  } else if(
    !isTRUE(all.equal(conditionMessage(target), conditionMessage(current)))
  ) {
    err.msg <- paste0(type.targ, " condition messages do not match")
  } else if(!isTRUE(all.equal(conditionCall(target), conditionCall(current)))) {
    err.msg <- paste0(type.targ, " condition calls do not match")
  }
  if(length(err.msg) && (target.printed || current.printed)) {
    print.show.err <- paste0(
      "Condition mismatch may involve print/show methods; carefully review ",
      "conditions with `.NEW$conditions` and `.REF$conditions` as just ",
      "typing `.ref` or `.new` at the prompt will invoke print/show methods, ",
      "which themselves may be the cause of the mismatch"
    )
    err.msg <- c(err.msg, print.show.err)
  }
  if(length(err.msg)) return(err.msg)
  TRUE
}
#' Prints A list of Conditions
#'
#' @export
#' @param object a \code{`conditionList`} object (list of conditions)
#' @return object, invisibly

setMethod("show", "conditionList",
  function(object) {
    width=getOption("width")
    cond.len <- length(object)
    if(!cond.len) {
      word_cat("Empty condition list")
      return(invisible(object))
    } else {
      word_cat(
        "Condition list with", cond.len,
        paste0("condition", if(cond.len > 1) "s", ":")
      )
    }
    cond.calls <- vapply(
      as.list(object), function(x) !is.null(conditionCall(x)), logical(1L)
    )
    out <- paste0(
      format(seq_along(object)), ": ",
      ifelse(
        print.show <- vapply(
          as.list(object),
          function(y) isTRUE(attr(y, "unitizer.printed")), logical(1L)
        ),
        "[print] ", ""
      ),
      vapply(as.list(object), get_condition_type, character(1L)),
      ifelse(cond.calls, " in ", "")
    )
    desc.chars <- max(width - nchar(out), 20L)
    cond.detail <- vapply(
      as.list(object), FUN.VALUE=character(1L),
      function(y) {
        if(is.null(conditionCall(y))) {
          paste0(": ", conditionMessage(y))
        } else {
          paste0(deparse(conditionCall(y))[[1L]], " : ", conditionMessage(y))
        }
    } )
    out <- paste0(out, substr(cond.detail, 1, desc.chars))
    if(any(print.show)) {
      out <- c(
        out,
        paste0(
          "[print] means condition was issued in print/show method rather ",
          "than in actual evaluation."
    ) ) }
    out <- c(out)
    cat(out, sep="\n")
    word_cat(
      "You can access conditions directly (e.g. `.NEW$conditions[[1L]]`)."
    )
    return(invisible(object))
  }
)
# Extracts Condition Type From Condition Classes
#
# Type (e.g. Error, Warning), is taken to be the second to last class.
#
# @keywords internal
# @param x a condition
# @return character 1 length the type of condition

get_condition_type <- function(x) {
  if(!inherits(x, "condition")) stop("Argument `x` must be a condition")
  classes <- rev(class(x))
  if(length(classes) < 2L || classes[[1L]] != "condition") "Unknown"
  cap_first(classes[[2L]])
}
