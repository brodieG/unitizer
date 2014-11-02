
setGeneric("review", function(x, ...) standardGeneric("review"))

setMethod(
  "review", "unitizer",
  function(x, ...) {

  }

)

setMethod(
  "review", "ANY",
  function(x, par.frame, ...) {
    unitizer <- try(load_unitizer(store.id, par.frame))

    if(inherits(unitizer, "try-error")) stop("Unable to load `unitizer`; see prior errors.")
    if(!is(unitizer, "unitizer")) return(unitizer)  # most likely because we upgraded and need to re-run

    review(unitizer)
  }
)
