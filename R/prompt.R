#' @include exec.R

NULL

#' Handles The Actual User Interaction
#'
#' Will keep accepting user input until either:
#' \itemize{
#'   \item User types one of the names of \code{`valid.opts`}, typically "Y" or
#'     "N", but will vary
#'   \item User types "Q"
#'   \item User inputs an expression that when evaluated and fed to
#'     \code{`exit.condition`} returns TRUE
#' }
#' The set-up is intended to replicate something similar to what happens when
#' code hits a \code{`browse()`} statement.  User expressions are evaluated
#' and output to screen, and special expressions as described above cause the
#' evaluation loop to terminate.
#'
#' @keywords internal
#' @seealso browse_unitizer_items
#' @param text the prompt text to display
#' @param browse.env the environment to evaluate user expressions in; typically
#'   this will contain interesting objects (use \code{ls()} to review)
#' @param valid opts the special letters user can type to get a special action,
#'   typically a character vector where the names are one letter (though they
#'   don't actually have to be) and are looked for as user typed input; note that
#'   the quit and help options will always be appended to this
#' @param help a character vector with help suggestions
#' @param hist.con connection to save history to
#' @param exit.condition function used to evaluate whether user input should
#'   cause the prompt loop to exit; this function should accept two parameters:
#'   \itemize{
#'     \item expression typed in by the user
#'     \item environment the environment user expressions get evaluated in
#'   }
#'   The function can then decide to exit or not based on either the literal
#'   expression or evaluate the expression and decide based on the result.  This
#'   is implemented this way because \code{`eval_user_exp`} will print to screen
#'   which may not be desirable.  Function should return a value which will then
#'   be returned by \code{`unitizer_prompt`}, unless this value is \code{`FALSE`}
#'   in which case \code{`unitizer_prompt`} will continue with normal evaluation.
#' @return mixed allowable user input

unitizer_prompt <- function(
  text, browse.env=globalenv(), help=character(),
  valid.opts, hist.con=NULL, exit.condition=function(exp, env) FALSE
) {
  if(!is.null(hist.con) && (!inherits(hist.con, "file") || !isOpen(hist.con)))
    stop("Argument `hist.con` must be an open file connection or NULL")
  if(!is.environment(browse.env))
    stop("Argument `browse.env` must be an environment")
  if(!is.character(valid.opts))
    stop("Argument `valid.opts` must be character")
  valid.opts <- c(valid.opts, Q="[Q]uit", H="[H]elp")
  # should validate other parameters as well
  opts.txt <- paste0("(", paste0(valid.opts, collapse=", "), ")?")
  repeat {
    while(inherits(try(val <- faux_prompt("unitizer> ")), "try-error")) NULL

    if(  # Input matches one of the options
      length(val) == 1L && is.symbol(val[[1L]]) &&
      as.character(val[[1L]]) %in% names(valid.opts) &&
      !(as.character(val[[1L]]) %in% c("Q", "H")) && nchar(val[[1L]])
    ) {
      return(as.character(val[[1L]]))
    } else if (length(val) == 1L && identical(val[[1L]], quote(Q))) {
      return("Q")
    } else if (length(val) == 1L && identical(val[[1L]], quote(H))) {
      if(!length(help)) {
        cat("No help available.", "", paste(text, opts.txt), sep="\n")
      } else {
        cat(help, "", paste(text, opts.txt), sep="\n")
      }
      next
    }
    # Check whether input should be captured specially

    if(inherits(res <- try(exit.condition(val, browse.env)), "try-error")) {
      stop("Logic Error: exit condition testing function failed; contact maintainer.")
    } else {
      if(!identical(res, FALSE)) return(res)
    }
    warn.opt <- getOption("warn")     # Need to ensure warn=1 so that things work properly
    on.exit(options(warn=warn.opt))
    if(warn.opt != 1L) options(warn=1L)
    trace.res <- NULL

    # Note `val` here is the expression the user inputted, not the result of the
    # evaluation.  The latter will be in res$value

    res <- eval_user_exp(val, browse.env)

    # store / record history

    if(!is.null(hist.con) && length(val) == 1L) {
      cat(deparse(val[[1L]]), file=hist.con, sep="\n")
      loadhistory(showConnections()[as.character(hist.con), "description"])
    }
    if(res$aborted || !length(val)) cat(text, opts.txt)  # error or no user input, re-prompt user
    if(res$aborted && !is.null(res$trace)) set_trace(res$trace)  # make error trace available for `traceback()`
} }
#' Wrapper Around User Interaction
#'
#' Specifically for cases were user has the choice to input something or to try
#' to navigate to another test.
#'
#' @seealso unitizer_prompt
#' @inheritParams unitizer_prompt
#' @param x a unitizer.browse object
#' @param browse.env1 environment to have user review tests, run commands, etc
#' @param browse.env2 navigation environment
#' @param curr.id which id we are currently browsing
#' @keywords internal

navigate_prompt <- function(
  x, curr.id, text, browse.env1=globalenv(), browse.env2=globalenv(),
  help=character(), valid.opts
) {
  if(!is(x, "unitizerBrowse")) {
    stop("Logic Error, expected unitizerBrowse object as param `x`; contact maintainer.")
  }
  # User input

  prompt.val <- unitizer_prompt(
    text, browse.env=browse.env1, help=help, valid.opts=valid.opts,
    hist.con=x@hist.con
  )
  if(identical(prompt.val, "B")) {

    # Go back to previous

    if(curr.id == 1L) {
      message("At first reviewable item; nothing to undo")
      return(x)
    }
    prev.tests <- x@mapping@item.id[!x@mapping@ignored] < curr.id
    x@last.id <- if(any(prev.tests)) max(which(prev.tests)) - 1L else 0L
    return(x)
  } else if (identical(prompt.val, "R")) {
    return(review_prompt(x, browse.env2))
  }
  return(prompt.val)
}
#' Manages Producing Test Navigation Message / Prompt
#'
#' Probably should be an S4 method, along with \code{`\link{navigatePrompt}`}
#'
#' @param x a unitizerBrowse object
#' @param nav.env an environment
#' @return either a \code{`unitizerBrowse`}, or "Q" if the user chose to quit

review_prompt <- function(x, nav.env) {

  if(!is(x, "unitizerBrowse") || !is.environment(nav.env))
    stop("Logic Error: unexpected inputs to internal function; contact maintainer.")

  # Navigation Prompt

  nav.help <- paste0(
    "You may re-review any of the tests that you have already reviewed by ",
    "selecting that test's number.  The last letter on each line represents ",
    "your previous input.  The word before the colon before the last letter ",
    "describes the type of test (e.g. Failed / Removed / Corrupted). The ",
    "numbering is not continuous because some statements in the store are ",
    "not considered tests (e.g. assignments). Additionally, the numbers may ",
    "not line up to the expressions in the test file because passed tests ",
    "are excluded from the numbering sequence. Alternatively, typing U will ",
    "take you to the first unreviewed test."
  )
  nav.opts <- c(
    "input an integer-like number corresponding to a reviewed test",
    U="[U]nreviewed"
  )
  nav.prompt <- "What test do you wish to review"
  cat(nav.prompt, " (", paste0(nav.opts, collapse=", "), ")?\n\n", sep="")
  show(x)
  exit.fun <- function(y, env) {               # keep re-prompting until user types in valid value
    if(!is.expression(y)) stop("Argument `y` should be an expression.")
    if(
      length(y) != 1L || !is.numeric(y[[1L]]) || length(y[[1L]]) != 1L ||
      y[[1L]] != as.integer(y[[1L]])
    ) return(FALSE)
    valid.vals <- x@mapping@item.id
    if(!isTRUE(y[[1L]] %in% valid.vals)) {
      message(
        "Input must be integer-like and in ",
        paste0(range(valid.vals), collapse=":")
      )
      return(FALSE)
    }
    return(y[[1L]])
  }
  nav.id <- unitizer_prompt(
    text=nav.prompt, help=nav.help, browse.env=nav.env, exit.condition=exit.fun,
    valid.opts=nav.opts
  )
  if(identical(nav.id, "Q")) {
    return("Q")
  } else if (identical(nav.id, "U")) {
    # Go to unreviewed test

    item.len <- length(x@mapping@review.val)
    if(all(x@mapping@reviewed)) {
      message("No unreviewed tests.")
      x@last.id <- item.len
      return(x)
    }
    message("Jumping to first unreviewed test.") # But note that we also show all ignored tests before that one for context

    nav.id <- min(which(!x@mapping@reviewed & !x@mapping@ignored))
  } else if (
    !is.numeric(nav.id) || length(nav.id) != 1L || as.integer(nav.id) != nav.id
  ) {
    stop("Logic Error: Unexpected user input allowed through in Review mode; contact maintainer")
  }
  # Find the test just before the one we selected that is not ignored, and start
  # showing right after that one

  prev.reviewed <- max(
    c(0L, which(head(!x@mapping@ignored, nav.id - 1L)))  # c(0, ...) to avoid warning
  )
  # Set last.id to zero if there is are no previously reviewed tests to start
  # from, otherwise to the test subsequent to the previously reivewed one
  # note the item that will be reviewed is the one right after this

  x@last.id <- prev.reviewed
  return(x)
}

