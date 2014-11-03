
setGeneric("review", function(x, ...) standardGeneric("review"))

setMethod(
  "review", "unitizer",
  function(x, ...) {

    unitizer.browse <- new("unitizerBrowse")

    for(i in seq_along(x@sections.ref)) {                   # Loop through parent sections
      sect.map <- x@section.ref.map == i   # will have to check what the section numbers are, this might not be right
      if(!length(which(sect.map))) next

      browse.sect <- new(
        "unitizerBrowseSection", section.id=i,
        section.title=x@sections.ref[[i]]@title
      )
      # Note: anything querying reference items has to go through items.new.map
      # since order isn't same.

      browse.sect <- browse.sect + new(                            # Passed tests
        "unitizerBrowseSubSectionPassed",
        items.new=x@items.ref[stop("need to get mapping here")],
        show.fail=FALSE
      )
      unitizer.browse <- unitizer.browse + browse.sect
      NULL # SO above isn't last step in loop used for debugging
    }
    message("did we handle tests without a section?")

    unitizer.browse
  }
)

setMethod(
  "review", "ANY",
  function(x, par.frame, ...) {
    unitizer <- try(load_unitizer(store.id, par.frame))

    if(inherits(unitizer, "try-error")) stop("Unable to load `unitizer`; see prior errors.")
    if(!is(unitizer, "unitizer")) return(unitizer)  # most likely because we upgraded and need to re-run

    NextMethod()
  }
)
