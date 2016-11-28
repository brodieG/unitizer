#' @include unitizer.R

NULL

#' Repair Environment Chains
#'
#' In theory should never be needed, but use in case you get errors about
#' corrupted environments.  You should only use this if you get an error
#' telling you to use it.
#'
#' If you pass a store id this will re-save the repaired \code{unitizer} to
#' the location specified by the store id.
#'
#' @seealso \code{\link{unitize}}
#' @export
#' @param x either a unitizer, or a store id (see \code{\link{unitize}})
#' @return a \code{unitizer} object

repair_environments <- function(x) {
  save <- FALSE
  if(!is(x, "unitizer")) {
    unitizer <- try(
      load_unitizers(
        list(x), test.files=NA_character_, par.frame=baseenv(),
        interactive.mode=interactive_mode(), mode="unitize"
      )[[1L]]
    )
    if(inherits(unitizer, "try-error"))
      stop("Unable to load `unitizer`; see prior errors.")
    save <- TRUE
  } else {
    unitizer <- x
  }
  unitizer <- repairEnvs(unitizer)
  if(save) {
    if(inherits(try(store_unitizer(unitizer)), "try-error"))
      warning(
        "Unable to store repaired unitizer, though we are still returning the ",
        "repaired unitizer."
      )
  }
  unitizer
}
setGeneric("repairEnvs", function(x, ...) standardGeneric("repairEnvs"))

setMethod("repairEnvs", "unitizer",
  function(x, ...) {
    parent.env(x@zero.env) <- baseenv()
    parent.env(x@base.env) <- x@zero.env
    parent.env(x@items.ref@base.env) <- x@base.env
    x@items.ref <- repairEnvs(x@items.ref)
    x
} )

setMethod("repairEnvs", "unitizerItems",
  function(x, ...) {
    warning(
      "Detected corrupted environment history; we will attempt to repair, ",
      "but keep in mind that even when repaired the test environments may ",
      "be missleading.  For example, the objects other than `.new` or `.ref` ",
      "when reviewing tests at the `unitzer` prompt may not be those you ",
      "expect or those reported by `ls`.  To fully restore environments ",
      "re-unitize with `unitize(..., force.update=TRUE)`.  If errors persist ",
      "after an attempt to repair, please contact maintainer.",
      immediate. = TRUE
    )
    if(!length(x)) x
    prev.par <- x@base.env

    for(i in 1:length(x)) {
      if(!identical(x[[i]]@env, prev.par)) parent.env(x[[i]]@env) <- prev.par  # can happen with ignored tests
      prev.par <- x[[i]]@env
    }
    invalidateLs(x)
} )
