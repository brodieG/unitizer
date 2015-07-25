#' @include item.R

NULL

#' Utility To Examine Object Size
#'
#' This is not considered a core function, and the name might change in the
#' future given the high probability of conflict with functions defined in
#' other packages.

setGeneric("size", function(x, ...) StandardGeneric("size"))
setMethod("size", "ANY", function(x, ...) object.size(x))
setMethod(
  "size", "unitizerItems",
  function(x, ...) {
    item.sizes <- sapply(
      x@.items,
      function(y) {
        c(
          sapply(setdiff(slotNames(y), "data"), function(z) size(slot(y, z))),
          sapply(slotNames(y@data), function(z) size(slot(y@data, z)))
    ) } )
    special.rows <- c("value", "conditions", "output", "message")
    special.rows.idx <- match(special.rows, rownames(item.sizes))
    if(any(is.na(special.rows.idx)))
      stop("Logic Error: missing row names; contact maintainer.")
    item.sizes.grp <- rbind(
      item.sizes[special.rows.idx, ],
      other=colSums(item.sizes[-special.rows.idx, ]),
      total=colSums(item.sizes)
    )
    items <- ncol(item.sizes.grp)
    top.20.pct <-
      order(-item.sizes.grp["total", ])[seq.int(ceiling(items * .2))]
    rbind(
      All=rowSums(item.sizes.grp),
      `Top 20%`=rowSums(item.sizes.grp[, top.20.pct]),
      Largest=item.sizes.grp[, top.20.pct[[1L]]]
    )
  }
)
setMethod("size", "unitizer", function(x, ...) NULL)


#' Measure object size as an RDS

sizeRDS <- function(object) {
  f <- tempfile()
  if(inherits(try(saveRDS(object, f)), "try-error")) return(NA)
  size <- file.info(f)$size
  unlink(f)
  size
}
