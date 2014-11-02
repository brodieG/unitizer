#' Alter Older unitizer Versions So They Pass Validation in Latest Version
#'
#' Sequentially applies all applicable patches
#'
#' @export
#' @param object an unitizer object
#' @param ... other arguments
#' @return an upgraded unitizer object

setMethod("upgrade", "unitizer", valueClass="unitizer",
  function(object, ...) {
    if(object@version < "0.4.3") {
      slots <- slotNames(object)
      slots <- slots[slots != "tests.conditions.new"]
      slot.objs <- lapply(slots, slot, object=object)
      names(slot.objs) <- slots
      slot.objs[["tests.conditions.new"]] <- logical(length(object@items.new))

      obj.new <- new("unitizer", id="", zero.env=new.env())
      for(i in names(slot.objs)) {
        slot(obj.new, i) <- slot.objs[[i]]
      }
      object <- obj.new
    }
    if(object@version < "0.5.2") {
      # This adds the reference test section data

      stop("Need to implement conversion to new object")

    }

    object
} )
