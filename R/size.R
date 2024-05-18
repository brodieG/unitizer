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

#' @include item.R

NULL

#' Utility To Examine Object Size
#'
#' Funny name is just to avoid conflicts with functions with same names in
#' other packages.
#' @keywords internal

setGeneric("sizeUntz", function(x, ...) StandardGeneric("sizeUntz")) # nocov
setMethod(
  "sizeUntz", "ANY", function(x, ...) c(size=object.size(x), rds=sizeRDS(x))
)
setMethod(
  "sizeUntz", "unitizerItems",
  function(x, ...) {
    items <- flattenUntz(x)
    if(!length(items)) return(c(size=0, rds=0))
    t(apply(items, 2, function(x) c(size=object.size(x), rds=sizeRDS(x))))
  }
)
setMethod("sizeUntz", "unitizer",
  function(x, ...) {
    res <- lapply(slotNames(x),
      function(y) {
        size.tmp <- sizeUntz(slot(x, y))
        if(is.matrix(size.tmp)) {
          rbind(
            matrix(apply(size.tmp, 2, sum), ncol=2, dimnames=list(y, NULL)),
            `rownames<-`(size.tmp, paste0("    ", rownames(size.tmp)))
          )
        } else matrix(size.tmp, ncol=2, dimnames=list(y, NULL))
      }
    )
    do.call(rbind, res)
} )
#' Reduce S4 objects Into Lists
#'
#' This is particularly useful with "list" type S4 objects, and relates loosely
#' to the subsetting functions defined for \code{unitizerBrowse} objects.
#'
#' Currently we only define a method for \code{unitizerItems-class} objects
#' @keywords internal

setGeneric(  # nocov start
  "flattenUntz", function(x, ...) StandardGeneric("flattenUntz")
)            # nocov end
setMethod(
  "flattenUntz", "unitizerItems",
  function(x, ...) {
    rows <- length(x)
    if(!rows) return(list())
    dat.base <- setdiff(slotNames(x[[1L]]), "data")
    dat.extra <-  slotNames(x[[1L]]@data)
    col.names <- c(dat.base, dat.extra)
    cols <- length(col.names)
    items <- structure(
      vector("list", cols * rows),
      dim=c(rows, cols), dimnames=list(NULL, col.names)
    )
    for(i in seq.int(rows)) {
      items[i, ] <- c(
        lapply(dat.base, function(z) slot(x[[i]], z)),
        lapply(dat.extra, function(z) slot(x[[i]]@data, z))
    ) }
    items
  }
)
#' Measure object size as an RDS
#' @keywords internal

sizeRDS <- function(object) {
  f <- tempfile()
  if(inherits(try(saveRDS(object, f, version=2)), "try-error")) return(NA)
  size <- file.info(f)$size
  unlink(f)
  size
}
