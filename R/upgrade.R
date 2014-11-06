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
      object <- addSlot(object, "tests.conditions.new", logical(length(object@items.new)))
    }
    # - 0.5.2 ------------------------------------------------------------------

    # Need to add sections.ref and section.map.ref, and add a section id slot
    # to all the unitizerItem objects

    if(object@version < "0.5.2") {
      # This adds the reference test section data

      # Add the requisite reference section fields

      ref.len <- length(object@items.ref)
      object <- addSlot(object, "sections.ref", list(new("unitizerSectionNA", length=ref.len)))
      object <- addSlot(object, "section.ref.map", rep(1L, ref.len))

      # Updated changes sub-object

      object@changes <- addSlot(object@changes, "passed", integer(2L))


      # Now add the new section.id field to every item

      object@items.ref@.items <- lapply(
        object@items.ref@.items,
        function(x) addSlot(x, "section.id", NA_integer_)
      )
    }
    # - Make Sure Conversion Succeeded -----------------------------------------

    if(inherits(try(validObject(object, complete=TRUE)), "try-error")) {
      stop("Failed attempting to upgrade unitizer")
    }
    object
} )
#' Helper Function To Add A Slot to An Out-of-date S4 Object
#'
#' @keywords internal

addSlot <- function(object, slot.name, slot.value) {
  if(!isS4(object))
    stop("Argument `object` must be an S4 object")
  slots <- slotNames(object)
  slot.vals <- list()
  for(i in slots) {
    tmp <- try(slot(object, i), silent=TRUE)
    if(!inherits(tmp, "try-error")) {
      slot.vals <- c(slot.vals, setNames(list(tmp), i))  # growing list, but shouldn't be massive
    }
  }
  slot.vals <- c(slot.vals, setNames(list(slot.value), slot.name))
  new.object <- new(class(object))
  for(slot.name in names(slot.vals)) {
    slot(new.object, slot.name) <- slot.vals[[slot.name]]
  }
  new.object
}

