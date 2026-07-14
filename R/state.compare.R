# Copyright (C) Brodie Gaslam
#
# This file is part of "unitizer - Interactive R Unit Tests"
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

# This used to have all the state comparison methods, but those just became
# unneeded with the advent of diffobj
#
# \code{all.equal} methods involving dummy

#' @rdname unitizer_s4method_doc

setMethod(  # We could just drop this altogether, but leaving it for future use
  "all.equal", c("unitizerDummy", "unitizerDummy"),
  function(target, current, ...) TRUE
)
#' @rdname unitizer_s4method_doc

setMethod(
  "all.equal", c("unitizerDummy", "ANY"),
  function(target, current, ...)
    paste(
      "`.REF` value was not recorded, but `.NEW` value was; they are likely",
      "different"
    )
)
#' @rdname unitizer_s4method_doc

setMethod(
  "all.equal", c("ANY", "unitizerDummy"),
  function(target, current, ...)
    paste(
      "`.NEW` value was not recorded, but `.REF` value was; they are likely",
      "different"
    )
)
#' @export
#' @rdname unitizer_s4method_doc

setMethod("all.equal", c("unitizerStateRaw", "unitizerStateRaw"), 
  function(target, current, ...) {
    for(i in slotNames(target)) {
      if(!isTRUE(diff <- all.equal(slot(target, i), slot(current, i))))
        return(sprintf("Slot `%s` is not all equal: %s", i, diff))
    }
    TRUE
  }
)

# To force recognizing the S4 method when called from inside another package
# which happens when we're doing `in_pkg`; will only work if the first argument
# is `unitizerDummy`, which should be the most common use case

#' @method all.equal unitizerDummy
#' @exportS3Method all.equal unitizerDummy

all.equal.unitizerDummy <- function(target, current, ...) {
  all.equal(target, current, ...)
}
#' @method all.equal unitizerStateRaw
#' @exportS3Method all.equal unitizerStateRaw

all.equal.unitizerStateRaw <- function(target, current, ...) {
  all.equal(target, current, ...)
}

# specifically an all.equal that returns garbage for testing; unfortunately
# this needs to be exported to be useable (blergh) (IS THIS STILL USED?)
# nocov start
#' @method all.equal unitizer_glob_state_test
#' @exportS3Method all.equal unitizer_glob_state_test

all.equal.unitizer_glob_state_test <- function(target, current, ...)
  list(1, 2, list("woohoo"))

# nocov end

## Merge State Data Between Reference and New Indices
##
## Required because we track these separately, but when we merge new and
## reference items we have to account for states from both.
##
## The items will be what is created by the review process and will contain a
## mix of new and reference items.  We go through and identify the reference
## items, and pull out the relevant states from the ref states tracking store
## and append them to the new states.  As part of this process, we need to
## re-index all the reference state elements to start counting after the end of
## the new state elements.
##
## @param x items, typically "reference" item being prepared for storage
## @param y new states
## @param z ref states

setGeneric("mergeStates", function(x, y, z, ...) standardGeneric("mergeStates"))
setMethod(
  "mergeStates", c(
    "unitizerItems", "unitizerGlobalTrackingStore",
    "unitizerGlobalTrackingStore"
  ),
  function(x, y, z, ...) {
    types <- itemsType(x)
    types.ref <- which(types == "reference")
    if(length(types.ref)) {
      ref.indices <- lapply(x[types.ref], slot, "glob.indices")
      max.indices <- unitizerStateMaxIndices(y)  # max new index

      # Map the global indices in reference to values starting from 1 up beyond
      # the end of the indices in the new indices, though use zeros for zero;
      # these are the index location for the reference items once we append
      # them to the tracking object; use `do.call` because `vapply`/`apply`
      # unstable in 1 column vs multi column inputs (#212, #199)

      remap <- function(w)
        match(w, sort(Filter(as.logical, unique(w))), nomatch=0L)
      ref.ind.mx <- do.call(cbind, lapply(ref.indices, as.integer))
      ref.ind.mx.map <- do.call(
        rbind, lapply(split(ref.ind.mx, row(ref.ind.mx)), remap)
      ) + as.integer(max.indices)
      rownames(ref.ind.mx.map) <- rownames(ref.ind.mx)

      if(!identical(attributes(ref.ind.mx), attributes(ref.ind.mx.map))) {
        stop(  # nocov start
          "Internal Error: global index mapping matrix malformed; contact ",
          "maintainer."
        )      # nocov end
      }
      ref.ind.mx.map[!ref.ind.mx] <- 0L  # these all map to the starting state

      # Pull out the states from ref and copy them into new; note that it is
      # possible for reference states to all reference the 0 index, meaning a
      # state wasn't captured, in that case we don't do anything

      for(i in slotNames(y)) {
        needed.state.ids <- unique(ref.ind.mx[i, ])
        needed.state.ids.map <- unique(ref.ind.mx.map[i, ])
        max.map.id <- max(needed.state.ids.map)
        if(max.map.id) { # 0 index only
          length(slot(y, i)) <- max.map.id
          for(j in seq_along(needed.state.ids)) {
            id <- needed.state.ids[[j]]
            id.map <- needed.state.ids.map[[j]]
            if(!id.map) next
            slot(y, i)[[id.map]] <- slot(z, i)[[id]]
          }
        }
      }
      # For each ref index, remap to the new positions in new state

      for(i in seq_along(types.ref)) {
        old.id <- types.ref[[i]]
        x[[old.id]]@glob.indices <- do.call(
          "new", c(list("unitizerGlobalIndices"), as.list(ref.ind.mx.map[, i]))
    ) } }
    # Return a list with the update item list and the states

    list(items=x, states=y)
} )
