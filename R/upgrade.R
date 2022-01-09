# Copyright (C) 2022 Brodie Gaslam
# 
# This file is part of "unitizer"
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

# Alter Older unitizer Versions So They Pass Validation in Latest Version
#
# Sequentially applies all applicable patches
#
# @param object an unitizer object
# @param ... other arguments
# @return an upgraded unitizer object

setMethod("upgrade", "unitizer", valueClass="unitizer",
  function(object, ...) {
    # - Do We Need To Upgrade --------------------------------------------------

    if(
      inherits(object <- try(upgrade_internal(object)), "try-error")
    ) {
      return(
        paste0(
          "Upgrade failed: ",
          paste0(conditionMessage(attr(object, "condition")), collapse="")
      ) )
    }
    if(inherits(try(validObject(object, complete=TRUE)), "try-error")){
      # nocov start
      return(
        "Internal Error: Upgrade produced invalid object; contact maintainer."
      )
      # nocov end
    }
    object
} )
# Helper function to actually do upgrade

upgrade_internal <- function(object) {

  ver <- object@version
  if(is.character(ver)) {
    ver <- package_version(ver)
  } else {
    object@version <- as.character(ver)
  }
  if(!is.package_version(ver))
    stop("Cannot determine unitizer version.") # nocov

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
    for(i in seq(length.out=length(object@items.ref)))
      object@items.ref[[i]]@id <- i
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
  if(ver < "1.0.1") {
    object@state.ref <- removeSlots(object@state.ref, c("dummy", ".dummy"))
    object <- addSlot(object, "state.new", new("unitizerGlobalTrackingStore"))
  }
  if(ver < "1.0.2") {
    statObj <- new("unitizerGlobalState")
    object@items.ref@.items <- lapply(
      object@items.ref@.items,
      function(x) addSlot(x, "state", statObj)
    )
  }
  if(ver < "1.0.3") {
    object@items.ref@.items <- lapply(
      object@items.ref@.items,
      function(x) {
        x <- addSlot(x, "call.dep", deparse_call(x@call))
        x@call <- NULL
        x
      }
    )
    object@items.ref.calls.deparse <- vapply(
      object@items.ref@.items, slot, character(1L), "call.dep"
    )
  }
  if(ver < "1.0.4") {
    object <- addSlot(object, "cons", NULL)
  }
  if(ver < "1.0.6") {
    object <- addSlot(object, "res.data", NULL)
  }
  if(ver < "1.0.7") {
    object <- addSlot(object, "updated.at.least.once", FALSE)
  }
  if(ver < "1.0.8") {
    object <- removeSlots(object, "cons")
  }
  if(ver < "1.0.9") {
    object@state.new <- addSlot(object@state.new, "namespaces", list())
    object@state.ref <- addSlot(object@state.ref, "namespaces", list())
  }
  if(ver < "1.0.10") {
    object@items.ref@.items <- lapply(
      object@items.ref@.items,
      function(x) {
        x@glob.indices <- addSlot(x@glob.indices, "namespaces", 0L)
        x@state <- addSlot(x@state, "namespaces", NULL)
        x
      }
    )
  }
  if(ver < "1.0.11") {
    # need to make sure we wrap values in list to avoid issues with recursive
    # validObject attempting to validate S3 objects
    for(i in seq_along(object@items.ref@.items))
      object@items.ref@.items[[i]]@data@value <- list(
        object@items.ref@.items[[i]]@data@value
      )
  }
  # - 1.1.0 --------------------------------------------------------------------

  if(ver < "1.1.0") {
    object <- addSlot(object, "jump.to.test", 0L)
  }
  if(ver < "1.1.1") {
    object <- addSlot(object, "items.new.calls.deparse.id", integer())
    object <- addSlot(object, "bookmark", NULL)
    object <- removeSlots(object, "jump.to.test")
  }
  # - 1.4.15 --------------------------------------------------------------------

  if(ver < "1.4.15") {
    object <- addSlot(object, "upgraded.from", "")
    object <- addSlot(object, "best.name", "")
    object <- addSlot(object, "show.progress", PROGRESS.MAX)
  }
  # - Keep at End---------------------------------------------------------------

  # Always make sure that any added upgrades require a version bump as we always
  # set version to current version, not the last version that required upgrades

  object@version <- as.character(packageVersion("unitizer"))
  object@upgraded.from <- as.character(ver)

  # - Done ---------------------------------------------------------------------

  object
}
# Helper Function To Add A Slot to An Out-of-date S4 Object
#
# @keywords internal

addSlot <- function(object, slot.name, slot.value) {
  if(!isS4(object))
    stop("Internal Error: Argument `object` must be an S4 object") # nocov
  slot.names <- slotNames(getClassDef(class(object)))
  if(!slot.name %in% slot.names) {
    warning(
      "Slot `", slot.name, "` does not exist in current version of `",
      class(object), "` so not added to object.", immediate. = TRUE
    )
    return(object)
  }
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
# Rename a slot
#
# Basically assumes old name exists in object, but not in new class definition

renameSlot <- function(object, old.name, new.name) {
  stopifnot(isS4(object), is.chr1(old.name), is.chr1(new.name))
  slots <- slotNames(object)
  stopifnot(!old.name %in% slots, new.name %in% slots)
  old.slot <- try(slot(object, old.name))
  if(inherits(old.slot, "try-error"))
    stop("Old slot `", old.name, "` doesn't exist in object")

  slot.vals <-
    sapply(slots[slots != new.name], slot, object=object, simplify=FALSE)
  args.final <- c(
    class(object), slot.vals, setNames(list(slot(object, old.name)), new.name)
  )
  do.call("new", args.final)
}
# Removes Slots
#
# Ignores slots that are already missing

removeSlots <- function(object, slots.to.remove) {
  stopifnot(isS4(object))
  slots <- slotNames(object)
  slot.vals <- sapply(
    slots[!slots %in% slots.to.remove], slot, object=object, simplify=FALSE
  )
  do.call("new", c(class(object), slot.vals))
}
# Intended for use only within unitize_core loop

upgrade_warn <- function(unitizers, interactive.mode, global) {
  review <- to_review(summary(unitizers, silent=TRUE))
  upgraded <- vapply(as.list(unitizers), slot, '', 'upgraded.from')
  up.rev <- which(review & nzchar(upgraded))

  if(length(up.rev)) {
    many <- length(up.rev) > 1L
    to.up.ids <- vapply(as.list(unitizers)[up.rev], slot, '', 'best.name')
    meta_word_cat(
      paste0(
        "\nThe following unitizer", if(many) "s", " will be upgraded to ",
        "version '", as.character(packageVersion('unitizer')), "':\n"
      ),
      as.character(UL(paste0(to.up.ids, " (at '", upgraded[up.rev], "')"))),
      width=getOption("width") - 2L
    )
    if(!interactive.mode) invokeRestart("unitizerInteractiveFail")
    else {
      meta_word_msg(
        "unitizer upgrades are IRREVERSIBLE and not backwards compatible. ",
        "Proceed?"
      )
      pick <- unitizer_prompt(
        "Upgrade unitizer stores?", hist.con=NULL,
        valid.opts=c(Y="[Y]es", N="[N]o"), global=global,
        browse.env=  # missing?
      )
      if(!identical(pick, 'Y'))
        invokeRestart("unitizerUserNoUpgrade")
    }
  }
  invisible(NULL)
}

