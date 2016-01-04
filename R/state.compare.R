# Compare State Between Reference and New Tests
#
# These functions were implemented before \code{\link{diff_state}}, and we
# recommend you use \code{\link{diff_state}} to get a high level view of
# state differences instead of these.
#
# Designed to compare the state snapshots as stored and recorded in a
# \code{unitizer}.  These approximate various aspects of global state.
# The approximation is because state objects that are too large are not kept,
# so we are not always certain whether some parts of state changed or not.
#
# We report several levels of differences between state values:
# \itemize{
#    \item affirmative difference, when we know with certainty the state values
#      are different
#    \item probable difference, when we have one known value and one unknown
#      value, typically because one was too large to store
#    \item possible difference, when both values are unknown
# }
# Additionally, it is possible that the state tracking settings change between
# \code{unitizer} runs.  If that happens, then:
# \itemize{
#   \item if state type is not tracked in new (current), then we assume state
#     is unchanged
#   \item if state is tracked in new, but was not in reference (target), then
#     we report a probable difference
# }
# @keywords internal

#' @rdname unitizer_s4method_doc

setMethod(
  "all.equal",
  c("unitizerGlobalState", "unitizerGlobalState"),
  function(target, current, verbose=TRUE, strict=FALSE, ...) {
    stopifnot(is.TF(verbose), is.TF(strict))
    valid.diff <- 0L:3L
    err.msgs <- paste0(c("possible ", "likely ", "known "), "differences")
    deltas <- setNames(
      vector("list", length(.unitizer.global.settings.names)),
      .unitizer.global.settings.names
    )
    for(i in .unitizer.global.settings.names) {
      tar <- slot(target, i)
      cur <- slot(current, i)
      msg.header <- sprintf("`%s` state mismatch:", i)

      deltas[[i]] <- if(is.null(tar) && !is.null(cur)) {
        paste(msg.header, "reference state not recorded")
      } else if(is.null(cur)) {
        NULL
      } else {
        if(identical(i, "options")) {
          # Compare common options with state_item_compare, all others are
          # assumed to be different; note that system and asis options should
          # not be part of these lists

          common.opts <- intersect(names(tar), names(cur))
          deltas.opts <- unlist(
            Map(
              state_item_compare, tar[common.opts], cur[common.opts]
          ) )
          mismatch.opts <- c(
            setdiff(names(tar), names(cur)), setdiff(names(cur), names(tar))
          )
          deltas.opts <- c(
            deltas.opts, setNames(rep(3L, length(mismatch.opts)), mismatch.opts)
          )
          if(any(deltas.opts > !strict)) {
            deltas.split <- split(names(deltas.opts), deltas.opts)
            deltas.by.type <- unlist(  # unlist drops any NULL values so we get rid of "0"
              lapply(
                sort(names(deltas.split), decreasing=TRUE),
                function(x) {
                  if(x %in% c("0", if(!strict) "1")) return(NULL)
                  paste0(
                    err.msgs[[as.integer(x)]], " for option",
                    if(length(deltas.split[[x]]) > 1L) "s", " ",
                    paste0(sprintf("\"%s\"", deltas.split[[x]]), collapse=", ")
            ) } ) )
            # Bulleted list of more than one type of option delta

            if(length(deltas.by.type) > 1L) {
              c(msg.header, as.character(UL(deltas.by.type)))
            } else paste(msg.header, deltas.by.type)
          }
        } else {
          # States other than options that don't need to be compared element
          # for element

          state.diff <- state_item_compare(tar, cur)
          if(state.diff)
            paste(
              msg.header,
              if(identical(state.diff, 3L)) {
                aq <- all.equal(tar, cur)
                if(isTRUE(aq) || !is.character(aq)) {
                  err.msgs[[state.diff]]
                } else {
                  if(length(aq) > 1L) paste0(aq[[1L]], "...") else aq
                }
              } else err.msgs[[state.diff]]
            )
    } } }
    # Finalize

    if(length(deltas)) {
      res <- vapply(deltas, paste0, character(1L), collapse="\n")
      if(verbose) word_cat(res, sep="\n")
      invisible(res)
    } else TRUE
} )
# Helper function for all.equal

state_item_compare <- function(tar, cur) {
  tar.dum <- is(tar, "unitizerDummy")
  cur.dum <- is(cur, "unitizerDummy")
  if(tar.dum && cur.dum) 1L else if (tar.dum || cur.dum) 2L else {
    if(identical(tar, cur)) 0L else 3L
} }
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

diff_state <- function(
  target=NULL, current=NULL, width=getOption("width"), file=stdout()
) {
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
  for(i in .unitizer.global.settings.names) {
    cur <- slot(current, i)
    tar <- slot(target, i)
    if(is.null(cur) || identical(tar, cur)) next

    cat(out <- sprintf("`%s` state mismatch:", i), sep="\n")
    if(!is.int.1L(width) || width < 8L) width <- 8L else width <- width - 4L

    diff.string <- if(identical(i, "options")) {
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
        diff.call.new[[2L]] <- as.name(".NEW")
        diff.call.ref[[2L]] <- as.name(".REF")
        diff_obj_internal(
          tar[[diff.name]], cur[[diff.name]],
          tar.exp=diff.call.ref, cur.exp=diff.call.new, width=width
        )
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
        as.character(UL(diff.list[seq.int(k)]), width=width)
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
        word_wrap(tmp, width=width)
      }
    } else {
      diff.call <- quote(x$state@y)
      diff.call[[3L]] <- as.name(i)
      diff.call.new <- diff.call.ref <- diff.call
      diff.call.new[[2L]] <- as.name(".NEW")
      diff.call.ref[[2L]] <- as.name(".REF")

      diff_obj_internal(  # should try to collapse this with the one for options
        tar, cur, tar.exp=diff.call.ref, cur.exp=diff.call.new, width=width
      )
    }
    out <- c(out, diff.string)
  }
  msg.no.diff <- paste0(
    "Note that there may be state differences that are not reported here as ",
    "state tracking is incomplete.  See vignette for details."
  )
  msg.extra <- word_wrap(
    if(length(out)) paste0(
      "For a more detailed comparison you can access state values ",
      "directly (e.g. .NEW$state@options).  ", msg.no.diff
    ) else c("No state differences detected.", "", msg.no.diff)
  )
  out <- c(out, msg.extra)
  if(!is.null(file)) cat(msg.extra, sep="\n", file=file)
  invisible(out)
}
