#' Edit Calls In Unitizer
#'
#' Used if you want to change the name of a function you have pre-existing
#' tests for.  Useful if you want to change the function name, but not have to
#' re-run and review all the tests since they will no longer match due to the
#' function name change.
#'
#' @note this is a somewhat experimental function, so make sure you backup any
#'   unitizers before you try to use it.
#'
#' @export
#' @rdname editFunNames
#' @param x a unitizer object
#' @param fun.name.old the name of the function replace
#' @param fun.name.new the new name of the function
#' @param ... unused
#' @return a untizer object with function names modifies

setGeneric("editFunNames", function(x, fun.name.old, fun.name.new, ...)
  standardGeneric("editFunNames")
)
#' @export
#' @rdname editFunNames
#' @param interactive.only logical(1L) set to FALSE if you want to allow this to
#'   run in non-interactive mode, but warnings will be suppressed and will
#'   proceed without prompting, obviously...
#' @examples
#' \donrun{
#' untz <- get_unitizer("tests/unitizer/mytests.unitizer")
#' untz.edited <- editFunNames(untz, quote(myFun), quote(my_fun))
#' set_unitizer(untz.edited, "tests/unitizer/mytests.unitizer")
#' }

setMethod("editFunNames", c("unitizer", "name", "name"),
  function(x, fun.name.old, fun.name.new, interactive.only=TRUE, ...) {
    warning(
      "This is an experimental function; make sure you backup any unitizers ",
      "before you edit them", immediate.=TRUE
    )
    if(!interactive() && interactive.only)
      stop("Set interactive.only to FALSE to run in non-interactive mode")
    i <- 0L
    repeat {
      ans <- readline("Do you wish to proceed ([Y]es/[N]o)? ")
      if(tolower(ans) %in% c("y", "yes")) break
      else if(tolower(ans) %in% c("n", "no")) {
        message("Existing without edits")
        return(x)
      } else {
        message("Invalid input, should be \"Y\" or \"N\"")
      }
      if((i <- i + 1L) > 2L) stop("You are not making sense; I give up!")
    }
    call_sub <- function(call, old.name, new.name) {
      if(is.call(call)) {
        if(identical(call[[1L]], old.name)) call[[1L]] <- new.name
        if(length(call) < 2L) return(call)
        for(i in 2:length(call)) {
          if(is.call(call[[i]]))
            call[[i]] <- Recall(call[[i]], old.name, new.name)
        }
      }
      call
    }
    for(i in seq_along(x@items.ref)) {
      x@items.ref[[i]]@call <-
        call_sub(x@items.ref[[i]]@call, fun.name.old, fun.name.new)
      x@items.ref.calls.deparse[[i]] <- deparse_call(x@items.ref[[i]]@call)
    }
    x
} )
