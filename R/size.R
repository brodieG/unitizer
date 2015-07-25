#' @include item.R

NULL

#' Utility To Examine Object Size
#'
#' This is not considered a core function, and the name might change in the
#' future given the high probability of conflict with functions defined in
#' other packages.

setGeneric("size", function(x, ...) StandardGeneric("size"))
setMethod(
  "size", "ANY", function(x, ...) c(size=object.size(x), rds=sizeRDS(x))
)
setMethod(
  "size", "unitizerItems",
  function(x, ...) {
    # Extract all the component items into a 2D list

    rows <- length(x)
    if(!rows) return(0)
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
    t(apply(items, 2, function(x) c(size=object.size(x), rds=sizeRDS(x))))
  }
)
setMethod("size", "unitizer",
  function(x, ...) {
    res <- lapply(slotNames(x),
      function(y) {
        size.tmp <- size(slot(x, y))
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

#' Measure object size as an RDS

sizeRDS <- function(object) {
  f <- tempfile()
  if(inherits(try(saveRDS(object, f)), "try-error")) return(NA)
  size <- file.info(f)$size
  unlink(f)
  size
}
