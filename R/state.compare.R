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
# To force recognizing the S4 method when called from inside another package
# which happens when we're doing `in_pkg`; will only work if the first argument
# is `unitizerDummy`, which should be the most common use case

#' @export

all.equal.unitizerDummy <- function(target, current, ...) {
  all.equal(target, current, ...)
}

# specifically an all.equal that returns garbage for testing; unfortunately
# this needs to be exported to be useable (blergh)
#' @export

all.equal.unitizer_glob_state_test <- function(target, current, ...)
  list(1, 2, list("woohoo"))

#' Display State Differences Between New and Reference Tests
#'
#' Intended to be called primarily from \code{unitizer} prompt where it can
#' find the \code{.NEW} and \code{.REF} objects; interface params provided for
#' testing.
#'
#' @export
#' @param target unitizerGlobalState object
#' @param current unitizerGlobalState object
#' @param width how many characters wide the display should be
#' @param file connection to output to
#' @return NULL
#' @examples
#' \dontrun{
#' ## type at `unitizer` prompt
#' diff_state()
#' }

diff_state <- function(target=NULL, current=NULL, width=getOption("width")) {
  target <- if(!is.null(target))
    target else try(get(".REF", parent.env(parent.frame()))$state, silent=T)
  current <- if(!is.null(current))
    current else try(get(".NEW", parent.env(parent.frame()))$state, silent=T)
  if(inherits(target, "try-error")) return(cat("`.NEW` object not present"))
  if(inherits(current, "try-error")) return(cat("`.REF` object not present"))
  stopifnot(
    is(target, "unitizerGlobalState"), is(current, "unitizerGlobalState")
  )
  out <- character(0L) # we're growing this, but it's small

  has.state.diffs <- FALSE
  first.diff <- TRUE
  for(i in .unitizer.global.settings.names) {
    cur <- slot(current, i)
    tar <- slot(target, i)
    if(is.null(cur) || identical(tar, cur)) next

    if(!has.state.diffs) cat("\n")
    meta_word_cat(sprintf("`%s` state mismatch:", i))
    has.state.diffs <- TRUE

    if(identical(i, "options")) {
      # Compare common options with state_item_compare, all others are
      # assumed to be different; note that system and asis options should
      # not be part of these lists

      common.opts <- intersect(names(tar), names(cur))
      deltas.common <- Map(all.equal, tar[common.opts], cur[common.opts])
      deltas.cur.miss <- Map(
        setdiff(names(tar), names(cur)),
        f=function(x) "this option is missing from `.NEW` state"
      )
      deltas.tar.miss <- Map(
        setdiff(names(cur), names(tar)),
        f=function(x) "this option is missing from `.REF` state"
      )
      deltas.opts <- c(deltas.common, deltas.cur.miss, deltas.tar.miss)
      deltas.real <- vapply(deltas.opts, Negate(isTRUE), logical(1L))
      deltas.names <- names(deltas.opts)
      deltas.count <- sum(deltas.real)

      # Depending on how many options there are, either show a full diff, or
      # the all.equal output, or just what options are different

      if(deltas.count < 1L) next

      if(deltas.count == 1L) {
        diff.name <- deltas.names[[which(deltas.real)]]
        diff.call <- quote(x$state@y$z)
        diff.call[[2L]][[3L]] <- as.name(i)
        diff.call[[3L]] <- as.name(diff.name)
        diff.call.new <- diff.call.ref <- diff.call
        diff.call.new[[2L]][[2L]][[2L]] <- as.name(".NEW")
        diff.call.ref[[2L]][[2L]][[2L]] <- as.name(".REF")
        show(
          diffObj(
            tar[[diff.name]], cur[[diff.name]],
            tar.banner=diff.call.ref, cur.banner=diff.call.ref,
            disp.width=width
        ) )
      } else if(deltas.count <= 10L) {
        # Depending on whether `all.equal` output is one or more lines, use
        # different display mode

        diff.list <- vector("list", 2 * deltas.count)
        k <- 0L

        for(j in which(deltas.real)) {
          k <- k + 1L
          if(!is.character(deltas.opts[[j]]) || !length(deltas.opts[[j]])) {
            diff.list[[k]] <-
              paste0(deltas.names[[j]], ": <unknown difference>")
          } else if(length(deltas.opts[[j]]) == 1L) {
            diff.list[[k]] <-
              paste0(deltas.names[[j]], ": ", deltas.opts[[j]])
          } else {
            diff.list[[k]] <- paste0(deltas.names[[j]], ": not `all.equal`:")
            diff.list[[k + 1L]] <- OL(deltas.opts[[j]])
            k <- k + 1L
        } }
        print(UL(diff.list[seq.int(k)]), width=width)
      } else {
        # this is a mess, need to cleanup someday

        tmp <- character()
        common.fail <- vapply(deltas.common, Negate(isTRUE), logical(1L))
        if(any(common.fail)) {
          tmp.out <- capture.output(
            print(sort(names(deltas.common)[common.fail]), width=width)
          )
          tmp <- c(tmp, "The following options have mismatches: ", tmp.out)
        }
        if(length(deltas.cur.miss)) {
          tmp.out <- capture.output(
            print(sort(names(deltas.cur.miss)), width=width)
          )
          tmp <- c(
            tmp, "The following options are missing from `.NEW`: ", tmp.out
          )
        }
        if(length(deltas.tar.miss)) {
          tmp.out <- capture.output(
            print(sort(names(deltas.tar.miss)), width=width)
          )
          tmp <- c(
            tmp, "The following options are missing from `.REF`: ", tmp.out
          )
        }
        cat(word_wrap(tmp, width=width), sep="\n")
      }
    } else {
      diff.call <- quote(x$state@y)
      diff.call[[3L]] <- as.name(i)
      diff.call.new <- diff.call.ref <- diff.call
      diff.call.new[[2L]][[2L]] <- as.name(".NEW")
      diff.call.ref[[2L]][[2L]] <- as.name(".REF")

      show(
        diffPrint(  # should try to collapse this with the one for options
          tar, cur,
          tar.banner=paste0(deparse(diff.call.ref), collapse="\n"),
          cur.banner=paste0(deparse(diff.call.new), collapse="\n"),
          disp.width=width
      ) )
    }
    cat("\n")
  }
  msg.no.diff <- paste0(
    "Note that there may be state differences that are not reported here as ",
    "state tracking is incomplete.  See vignette for details."
  )
  meta_word_cat(
    if(has.state.diffs) paste0(
      "For a more detailed comparison you can access state values ",
      "directly (e.g. .NEW$state@options).  ", msg.no.diff
    ) else c("No state differences detected.", "", msg.no.diff)
  )
  invisible(NULL)
}
# Merge State Data Between Reference and New Indices
#
# Required because we track these separately, but when we merge new and reference
# items we have to account for states from both.

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
      ref.indices <- lapply(x[types.ref ], slot, "glob.indices")
      max.indices <- unitizerStateMaxIndices(y)

      # Map the global indices in reference to values starting from 1 up beyond
      # the end , or 0 for zero

      ref.ind.mx <- do.call(cbind, lapply(ref.indices, as.integer))
      ref.ind.mx.map.tmp <- apply(
          ref.ind.mx, 1, function(z) {
            match(z, sort(Filter(as.logical, unique(z))), nomatch=0L)
      } ) + as.integer(max.indices)
      ref.ind.mx.map <- if(is.matrix(ref.ind.mx.map.tmp))
        t(ref.ind.mx.map.tmp) else as.matrix(ref.ind.mx.map.tmp)

      if(!identical(attributes(ref.ind.mx), attributes(ref.ind.mx.map))) {
        stop(
          "Logic Error: global index mapping matrix malformed; contact ",
          "maintainer."
        )
      }
      ref.ind.mx.map[!ref.ind.mx] <- 0L  # these all map to the starting state

      # Pull out the states from ref and copy them into new

      for(i in slotNames(y)) {
        needed.state.ids <- unique(ref.ind.mx[i, ])
        needed.state.ids.map <- unique(ref.ind.mx.map[i, ])
        length(slot(y, i)) <- max(needed.state.ids.map)

        for(j in seq_along(needed.state.ids)) {
          id <- needed.state.ids[[j]]
          id.map <- needed.state.ids.map[[j]]
          if(!id.map) next
          slot(y, i)[[id.map]] <- slot(z, i)[[id]]
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
