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
