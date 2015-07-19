#' Compare State Between Reference and New Tests
#'
#' Designed to compare the state snapshots as stored and recorded in a
#' \code{unitizer}.  These approximate various aspects of global state.
#' The approximation is because state objects that are too large are not kept,
#' so we are not always certain whether some parts of state changed or not.
#'
#' We report several levels of differences between state values:
#' \itemize{
#'    \item affirmative difference, when we know with certainty the state values
#'      are different
#'    \item probable difference, when we have one known value and one unknown
#'      value, typically because one was too large to store
#'    \item possible difference, when both values are unknown
#' }
#' Additionally, it is possible that the state tracking settings change between
#' \code{unitizer} runs.  If that happens, then:
#' \itemize{
#'   \item if state type is not tracked in new (current), then we assume state
#'     is unchanged
#'   \item if state is tracked in new, but was not in reference (target), then
#'     we report a probable difference
#' }
#' @export

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

setMethod(
  "all.equal", c("unitizerDummy", "unitizerDummy"),
  function(target, current, ...) {
    paste0(
      "state values not explicitly recorded for either `target` or `current`; ",
      "these may or may not have been different."
    )
  }
)
setMethod(
  "all.equal", c("unitizerDummy", "ANY"),
  function(target, current, ...)
    "`target` state was not recorded; it is likely different to `current` state"
)
setMethod(
  "all.equal", c("ANY", "unitizerDummy"),
  function(target, current, ...)
    "`current` state was not recorded; it is likely different to `target` state"
)
#' Present State Differences in Easy to Review Form
#'
#' Intended to be called primarily from \code{unitizer} prompt where it can
#' find the \code{.NEW} and \code{.REF} objects; interface params provided for
#' testing.
#'
#' @export
#' @param target unitizerGlobalState object
#' @param current unitizerGlobalState object
#' @param width how many characters wide the display should be
#' @return NULL

diff_state <- function(target=NULL, current=NULL, width=getOption("width")) {
  target <- if(!is.null(target))
    target else try(get(".NEW", parent.env(envir))$state, silent=T)
  current <- if(!is.null(current))
    current else try(get(".REF", parent.env(envir))$state, silent=T)
  if(inherits(target, "try-error")) return(cat("`.NEW` object not present"))
  if(inherits(current, "try-error")) return(cat("`.REF` object not present"))
  stopifnot(
    is(target, "unitizerGlobalState"), is(current, "unitizerGlobalState")
  )
  for(i in .unitizer.global.settings.names) {
    cur <- slot(current, i)
    tar <- slot(target, i)
    if(is.null(cur) || identical(tar, cur)) next

    msg.header <- sprintf("`%s` state mismatch:", i)
    if(!is.int.1L(width) || width < 8L) width <- 8L else width <- width - 2L

    diff.string <- if(identical(i, "options")) {
      # Compare common options with state_item_compare, all others are
      # assumed to be different; note that system and asis options should
      # not be part of these lists

      common.opts <- intersect(names(tar), names(cur))
      deltas.opts <- Map(all.equal, tar[common.opts], cur[common.opts])
      missing.cur <- setdiff(names(tar), names(cur))
      missing.tar <- setdiff(names(cur), names(tar))
      deltas.opts <- c(
        deltas.opts,
        Map(missing.cur, f=function(x) "missing from `current` state"),
        Map(missing.tar, f=function(x) "missing from `target` state")
      )
      deltas.real <- vapply(deltas.opts, Negate(isTRUE), logical(1L))
      deltas.names <- names(deltas.opts)
      deltas.count <- sum(deltas.real)

      # Depending on how many options there are, either show a full diff, or
      # the all.equal output, or just what options are different

      if(deltas.count < 1L) next

      if(deltas.count == 1L) {
        diff_obj_out(
          tar, cur,
          obj.rem.name=sprintf(".REF$state@%s", i),
          obj.add.name=sprintf(".REF$state@%s", i),
          width=width, file=NULL
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
            diff.list[[k]] <- deltas.names[[j]]
            diff.list[[k + 1L]] <- UL(deltas.opts, style="+")
            k <- k + 1L
        } }
        as.character(UL(diff.list[seq.int(k)]), width=width)
      } else {
        word_wrap(
          "The following options have mismatches: ",
          paste0(diff.list, collapse=","),
          width=width
      ) }
    } else {
      diff_obj_out(
        tar, cur,
        obj.rem.name=sprintf(".REF$state@%s", i),
        obj.add.name=sprintf(".REF$state@%s", i),
        width=getOption("width", 2L) - 2L, file=NULL
      )
    }
    cat(msg.header, "\n", sep="")
    cat(paste0(paste0(rep(" ", 2L), collapse=""), diff.string), sep="\n")
  }
}
