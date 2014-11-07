#' Summary of Changes
#'
#' Changes arise any time a user, through the interactive unitizer mode,
#' requests the storing of a change (accept new version of failed test,
#' add new test, remove old test).
#'
#' @keywords internal

setClass("unitizerChanges",
  representation(failed="integer", new="integer", removed="integer", corrupted="integer", passed="integer"),
  prototype(failed=integer(2L), new=integer(2L), removed=integer(2L), corrupted=integer(2L), passed=integer(2L)),
  validity=function(object) {
    for(i in slotNames(object)) {
      if((len <- length(slot(object, i))) > 0L && len != 2L) {
        return(paste0("slot `@`", i, " must be of length 2"))
      } else if (slot(object, i)[[1L]] > slot(object, i)[[2L]]) {
        return(paste0("slot `@`", i, " must be length 2 with the first value less than the second"))
      }
    }
    TRUE
  }
)
#' Print Out A Summary Of the Changes
#' @keywords internal

setMethod("show", "unitizerChanges",
  function(object) {
    if(object@failed[[1L]]) cat("- Replace", object@failed[[1L]] ,"out of", object@failed[[2L]], "failed tests\n")
    if(object@new[[1L]]) cat("- Add", object@new[[1L]] , "out of", object@new[[2L]], "new tests\n")
    if(object@removed[[1L]]) cat("- Remove", object@removed[[1L]], "out of", object@removed[[2L]], "removed tests\n")
    if(object@corrupted[[1L]]) cat("- Replace", object@corrupted[[1L]], "out of", object@corrupted[[2L]], "tests with errors\n")
  }
)
#' Return Sum of Total Changes
#' @keywords internal

setMethod(
  "length", "unitizerChanges",
  function(x) {
    sum(vapply(slotNames(x), function(y) slot(x, y)[[1L]], 1L))
  }
)
