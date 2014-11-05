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
    # - 0.4.3 ------------------------------------------------------------------

    # Need to add tests.conditions.new slot

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
    # - 0.5.2 ------------------------------------------------------------------

    # Need to add sections.ref and section.map.ref, and add a section id slot
    # to all the unitizerItem objects

    if(object@version < "0.5.2") {
      # This adds the reference test section data

      # Add the requisite reference section fields

      slots <- slotNames(object)
      slots <- slots[!slots %in% c("sections.ref", "section.ref.map")]
      slot.objs <- lapply(slots, slot, object=object)
      names(slot.objs) <- slots

      ref.len <- length(object@items.ref)
      slot.objs[["sections.ref"]] <- list(new("unitizerSectionNA", length=ref.len))
      slot.objs[["section.ref.map"]] <- rep(1L, ref.len)

      obj.new <- new("unitizer", id="", zero.env=new.env())
      for(i in names(slot.objs)) {
        slot(obj.new, i) <- slot.objs[[i]]
      }
      object <- obj.new

      # Now add the new section.id field to every item

      object@items.ref@.items <- lapply(
        as.list(object@items.ref),
        function(x) {
          slots <- slotNames(x)
          slots <- slots[slots != "section.id"]
          slot.vals <- c(
            Class="unitizerItem",
            setNames(lapply(slots, function(y) slot(x, y)), slots),
            section.id=NA_integer_
          )
          do.call(new, slot.vals, quote=TRUE)
        }
      )
    }
    # - Make Sure Conversion Succeeded -----------------------------------------

    if(inherits(try(validObject(object, complete=TRUE)), "try-error")) {
      stop("Failed attempting to upgrade unitizer")
    }
    object
} )
