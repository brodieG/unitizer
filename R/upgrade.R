#' Alter Older unitizer Versions So They Pass Validation in Latest Version
#'
#' Sequentially applies all applicable patches
#'
#' @keywords internal
#' @param object an unitizer object
#' @param ... other arguments
#' @return an upgraded unitizer object

setMethod("upgrade", "unitizer", valueClass="unitizer",
  function(object, ...) {
    # - Do We Need To Upgrade --------------------------------------------------

    if(
      inherits(object <- try(upgrade_internal(object)), "try-error")
    ) {
      return(
        paste0(
          "Upgrade failed: ",
          paste0(conditionMessage(attr(object, "condition")), collapse=TRUE)
      ) )
    }
    if(inherits(try(validObject(object, complete=TRUE)), "try-error")){
      return("Upgrade produced invalid object; contact maintainer.")
    }
    object
} )
#' Helper function to actually do upgrade
#'
#' @keywords internal

upgrade_internal <- function(object) {

  ver <- object@version
  if(is.character(ver)) ver <- package_version(ver)
  if(!is.package_version(ver)) stop("Cannot determine unitizer version.")

  # - 0.4.3 --------------------------------------------------------------------

  # Need to add tests.conditions.new slot

  if(ver < "0.4.3") {
    object <- addSlot(
      object, "tests.conditions.new", logical(length(object@items.new))
    )
  }
  # - 0.5.2 --------------------------------------------------------------------

  # Need to add sections.ref and section.map.ref, and add a section id slot
  # to all the unitizerItem objects

  if(ver < "0.5.2") {
    # This adds the reference test section data

    # Add the requisite reference section fields

    ref.len <- length(object@items.ref)
    object <- addSlot(
      object, "sections.ref", list(new("unitizerSectionNA", length=ref.len))
    )
    object <- addSlot(object, "section.ref.map", rep(1L, ref.len))

    # Updated changes sub-object

    object@changes <- addSlot(object@changes, "passed", integer(2L))

    # Now add the new section.id field to every item

    object@items.ref@.items <- lapply(
      object@items.ref@.items,
      function(x) addSlot(x, "section.id", NA_integer_)
    )
  }
  # - 0.5.3 --------------------------------------------------------------------

  # Make sure ref item ids are reasonable

  if(ver < "0.5.3") {
    for(i in seq(len=length(object@items.ref))) object@items.ref[[i]]@id <- i
  }
  # - 0.9.0 --------------------------------------------------------------------

  # Add new slots

  if(ver < "0.9.0") {
    object <- addSlot(object, "test.file.loc", NA_character_)  # not sure this is completely necessary since we're just using the prototype value
    object <- addSlot(object, "eval", FALSE)                   # not sure this is completely necessary since we're just using the prototype value
    object <- addSlot(object, "eval.time", 0)                  # not sure this is completely necessary since we're just using the prototype value
    object <- addSlot(object, "updated", FALSE)                # not sure this is completely necessary since we're just using the prototype value
    for(i in seq_along(object@items.ref))
      object@items.ref[[i]] <-
        addSlot(object@items.ref[[i]], "section.name", "<unknown>")
  }
  # - 1.0.0 --------------------------------------------------------------------

  # adding some slots, and version now stored as character instead of
  # `package_version` in an 'ANY' slot

  if(ver < "1.0.0") {
    for(i in seq_along(object@items.ref))
      object@items.ref[[i]] <-
        addSlot(
          object@items.ref[[i]], "glob.indices", new("unitizerGlobalIndices")
        )
    object@version <- as.character(object@version)
    object <- addSlot(object, "state.ref", new("unitizerGlobalTrackingStore"))
    object <- addSlot(object, "global", NULL)
  }
  # - Keep at End---------------------------------------------------------------

  # Always make sure that any added upgrades require a version bump as we always
  # set version to current version, not the last version that required upgrades

  object@version <- as.character(packageVersion("unitizer"))

  # - Done ---------------------------------------------------------------------

  object
}

#' Helper Function To Add A Slot to An Out-of-date S4 Object
#'
#' @keywords internal

addSlot <- function(object, slot.name, slot.value) {
  if(!isS4(object))
    stop("Argument `object` must be an S4 object")
  slots <- slotNames(object)
  slot.vals <- list()
  for(i in slots) {
    if(identical(i, slot.name)) next
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

