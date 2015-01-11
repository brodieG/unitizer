setGeneric("edit_fun_names", function(x, fun.name.old, fun.name.new, ...)
  standardGeneric("edit_fun_names")
)

#' Edit Calls In Unitizer
#'
#' Used if you want to change the name of a function you have pre-existing
#' tests for.
#'
#' @export

setMethod("edit_fun_names", c("unitizer", "name", "name"),
  function(x, fun.name.old, fun.name.new, ...) {

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
