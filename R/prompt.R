# Copyright (C) 2021 Brodie Gaslam
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

#' @include exec.R

NULL

#' Interactively Retrieve User Input
#'
#' Different functions used in different contexts to capture user input.
#' \code{unitizer_prompt}, \code{navigate_prompt}, and \code{review_prompt} are
#' more advanced and allow evaluation of arbitrary expressions, in addition to
#' searching for specific commands such as "Y", "N", etc. \code{simple_prompt}
#' only matches along specified values.
#'
#' The set-up is intended to replicate something similar to what happens when
#' code hits a \code{browse()} statement.  User expressions are evaluated
#' and output to screen, and special expressions as described above cause the
#' evaluation loop to terminate.
#'
#' \code{navigate_prompt} is just a wrapper around \code{unitizer_prompt} that
#' provides the special shortcuts to navigate to other tests in the
#' \code{unitizer}.
#'
#' \code{review_prompt} is also a wrapper, but used only when at the menu that
#' presents available test items to navigate to.
#'
#' \code{simple_prompt} simpler prompting function used to allow user to select
#' from pre-specified values.
#'
#' \code{exit_fun} is used as a generic function to pass to the
#' \code{exit.condition} argument of \code{unitizer_prompt}.
#'
#' \code{read_line} and \code{read_line_set_vals} are utility functions that
#' are used to implement a version of \code{\link{readline}} that can be
#' automated for testing.
#'
#' \code{interactive_mode} returns interactive status, accounting for whether
#' we are in faux-interactive mode as set by \code{read_line_set_vals}
#'
#' @keywords internal
#' @seealso browse_unitizer_items
#' @param text the prompt text to display
#' @param browse.env the environment to evaluate user expressions in; typically
#'   this will contain interesting objects (use \code{ls()} to review)
#' @param valid.opts the special letters user can type to get a special action,
#'   typically a character vector where the names are one letter (though they
#'   don't actually have to be) and are looked for as user typed input; note that
#'   the quit and help options will always be appended to this
#' @param help a character vector with help suggestions: the first value in the
#'   vector is \code{\link{word_cat}} output, the rest normal \code{cat}
#' @param help.opts a character vector of help options
#' @param hist.con connection to save history to
#' @param exit.condition function used to evaluate whether user input should
#'   cause the prompt loop to exit; this function should accept two parameters:
#'   \itemize{
#'     \item expression typed in by the user
#'     \item environment the environment user expressions get evaluated in
#'   }
#'   The function can then decide to exit or not based on either the literal
#'   expression or evaluate the expression and decide based on the result.  This
#'   is implemented this way because \code{eval_user_exp} will print to screen
#'   which may not be desirable.  Function should return a value which will then
#'   be returned by \code{unitizer_prompt}, unless this value is \code{FALSE}
#'   in which case \code{unitizer_prompt} will continue with normal evaluation.
#' @param x a unitizerBrowse object
#' @param browse.env1 environment to have user review tests, run commands, etc
#' @param browse.env2 navigation environment
#' @param curr.id which id we are currently browsing
#' @param nav.env an environment
#' @param ... additional arguments for \code{exit.condition}
#' @param message character ask the user a question
#' @param values character valid responses
#' @param prompt see \code{\link{readline}}
#' @param attempts how many times to try before erroring
#' @param case.sensitive whether to care about case sensitivity when matching
#'   user input
#' @param global unitizerGlobal or NULL, if the global state tracking object;
#'   will be used to record state after evaluating user expressions
#' @param warn.sticky TRUE or FALSE (default) whether any changes to the "warn"
#'   global option made by the evaluation of an R expression under the prompt
#'   should be allowed to stick after the evaluation.  Normally that option value
#'   is reset after each evaluation.
#' @return \itemize{
#'   \item \code{unitizer_prompt}: mixed allowable user input
#'   \item \code{navigate_prompt}: a \code{unitizerBrowse} object, or allowable
#'     user input
#'   \item \code{review_prompt}: a \code{unitizerBrowse} object, or "Q" if the
#'     user chose to quit
#'   \item \code{simple_prompt}: one of \code{values} as selected by user
#' }

unitizer_prompt <- function(
  text, browse.env=baseenv(), help=character(), help.opts=character(),
  valid.opts, hist.con=NULL, exit.condition=function(exp, env) FALSE,
  global, warn.sticky=FALSE, ...
) {
  if(!interactive_mode())
    # nocov start
    stop(
      "Internal Error: attempting to use interactive unitizer environment in ",
      "non-interactive session."
    )
    # nocov end
  if(!is.null(hist.con) && (!inherits(hist.con, "file") || !isOpen(hist.con)))
    stop("Argument `hist.con` must be an open file connection or NULL")
  if(!is.environment(browse.env)) {
    stop("Argument `browse.env` must be an environment")
  }
  if(!is.character(valid.opts))
    stop("Argument `valid.opts` must be character")
  if(!is(global, "unitizerGlobal") && !is.null(global))
    stop("Argument `global` must be \"unitizerGlobal\" or NULL")
  valid.opts <- c(valid.opts, Q="[Q]uit", H="[H]elp")
  # should validate other parameters as well
  opts.txt <- paste0(
    "(", paste0(valid.opts[nchar(valid.opts) > 0], collapse=", "), ")?"
  )
  repeat {
    prompt.txt <- sprintf("%s> ", "unitizer")

    val <- tryCatch(
      faux_prompt(prompt.txt),
      simpleError=function(e) e
    )
    on.exit(NULL)

    if(inherits(val, "simpleError")) {
      cond.chr <- as.character(val)
      cat(cond.chr, file=stderr())
      next
    }
    if(  # Input matches one of the options
      length(val) == 1L && is.symbol(val[[1L]]) &&
      as.character(val[[1L]]) %in% names(valid.opts) &&
      !(as.character(val[[1L]]) %in% c("Q", "H"))
    ) {
      cat("\n")
      return(as.character(val[[1L]]))
    } else if (length(val) == 1L && identical(val[[1L]], quote(Q))) {
      cat("\n")
      return(as.character(val[[1L]]))
    } else if (length(val) == 1L && identical(val[[1L]], quote(H))) {
      cat("\n")
      if(!length(help)) {
        meta_word_cat("No help available.", "", paste(text, opts.txt), sep="\n")
      } else {
        meta_word_cat(help, trail.nl=FALSE)
        if(length(help.opts))
          meta_word_cat(
            as.character(UL(help.opts), width=getOption("width") - 2L),
            trail.nl=FALSE
          )
        meta_word_cat("", paste(text, opts.txt))
      }
      next
    }
    # Check whether input should be captured specially

    res <- try(exit.condition(val, browse.env, ...))
    if(inherits(res, "try-error")) {
      # nocov start
      stop("Internal Error: exit condition function failed; contact maintainer.")
      # nocov end
    } else {
      if(!identical(res, FALSE)) return(res)
    }
    # Note `val` here is the expression the user inputted, not the result of the
    # evaluation.  The latter will be in res$value

    res <-
      eval_user_exp(val, browse.env, global=global, warn.sticky=warn.sticky)

    # store / record history

    if(!is.null(hist.con) && length(val) == 1L) {
      dval <- deparse(val[[1L]])
      history_write(hist.con, dval)
    }
    # error or no user input, re-prompt user
    if(res$aborted || !length(val)) {
      cat("\n")
      meta_word_cat(text, opts.txt, sep=" ")
    }
    # make error trace available for `traceback()`
    if(res$aborted && !is.null(res$trace)) set_trace(res$trace)
} }
#' @rdname unitizer_prompt
#' @keywords internal

navigate_prompt <- function(
  x, curr.id, text, browse.env1=globalenv(), browse.env2=globalenv(),
  help=character(), help.opts=character(), valid.opts, warn.sticky=FALSE
) {
  if(!is(x, "unitizerBrowse")) {
    stop( # nocov start
      "Internal Error, expected unitizerBrowse object as param `x`; ",
      "contact maintainer."
  ) }     # nocov end
  # User input

  prompt.val <- unitizer_prompt(
    text, browse.env=browse.env1, help=help, help.opts=help.opts,
    valid.opts=valid.opts, hist.con=x@hist.con, global=x@global,
    warn.sticky=warn.sticky
  )
  if(identical(prompt.val, "P")) {
    # Go back to previous
    prev.tests <- x@mapping@item.id < curr.id & !x@mapping@ignored & (
      if(!identical(x@mode, "review")) x@mapping@review.type != "Passed"
      else TRUE
    )
    x@last.id <- if(any(prev.tests)) max(which(prev.tests)) - 1L else 0L
    if(!x@last.id)
      meta_word_msg("At first reviewable item; nothing to step back to")
    x@navigating <- TRUE
    return(x)
  } else if (identical(prompt.val, "B")) {
    return(review_prompt(x, browse.env2))
  } else if (identical(prompt.val, "U")) {
    unreviewed <- unreviewed(x)
    if(!length(unreviewed)) {
      meta_word_msg("No unreviewed tests.")
      x@last.id <- tail(x@mapping@item.id, 1L)
    } else x@last.id <- head(unreviewed, 1L) - 1L
    x@navigating <- TRUE
    return(x)
  }
  return(prompt.val)
}
#' @rdname unitizer_prompt
#' @keywords internal

review_prompt <- function(x, nav.env) {
  if(!is(x, "unitizerBrowse") || !is.environment(nav.env))
    stop(  # nocov start
      "Internal Error: unexpected inputs to internal function; contact ",
      "maintainer."
    )      # nocov end
  # Navigation Prompt

  nav.help <- paste0(
    "Select a test to review by typing that test's number at the prompt. ",
    "Tests that start with a `*`",
    if(identical(x@mode, "unitize")) ", or with status \"Passed\",",
    " are not typically reviewed in this mode.  The letter after the test ",
    "status represents prior user input to test review (a `-` indicates test ",
    "has not been reviewed). Type \"U\" to jump to the first unreviewed ",
    "test.\n\n",
    "Note that tests are displayed in the order they appear in the test",
    "file, not in the order they would be reviewed in.\n"
  )
  nav.opts <- c(
    "input a test number",
    U="[U]nreviewed"
  )
  nav.prompt <- "What test do you wish to review"
  show(x)
  meta_word_cat(
    nav.prompt, paste0("(", paste0(nav.opts, collapse=", "), ")?"), sep=" "
  )
  nav.id <- unitizer_prompt(
    text=nav.prompt, help=nav.help, browse.env=nav.env, exit.condition=exit_fun,
    valid.opts=nav.opts, valid.vals=x@mapping@item.id, global=x@global
  )
  if(identical(nav.id, "Q")) {
    return("Q")
  } else if (identical(nav.id, "U")) { # Go to unreviewed test
    unreviewed <- unreviewed(x)
    nav.id <- if(!length(unreviewed)) {
      meta_word_msg("No unreviewed tests.")
      tail(x@mapping@item.id, 1L) + 1L
    } else head(unreviewed, 1L)
  } else if (
    !is.numeric(nav.id) || length(nav.id) != 1L || as.integer(nav.id) != nav.id
  ) {
    stop( # nocov start
      "Internal Error: Unexpected user input allowed through in Review mode; ",
      "contact maintainer"
    )     # nocov end
  } else {
    # Remap our nav.id to the actual review order instead of file order

    nav.id <- x@mapping@item.id[match(nav.id, x@mapping@item.id.ord)]
    if(is.na(nav.id))
      # nocov start
      stop(
        "Internal Error: failed retrieving internal item id; contact maintainer."
      )
      # nocov end
  }
  # Determine whether test we selected is a test we would normally not review
  # note nav.id can be greater than length if we select Unreviewed and there are
  # no unreviewed

  if(nav.id <= length(x@mapping@ignored)) {
    x@inspect.all <- x@mapping@ignored[[nav.id]] || (
        identical(x@mode, "unitize") && !x@start.at.browser &&
        identical(as.character(x@mapping@review.type[[nav.id]]), "Passed")
      )
    x@review <- if(x@inspect.all) -1L else 1L

    if(x@inspect.all) {
      cat("\n")
      meta_word_msg(
        "You selected a test that is not normally reviewed in this mode;",
        "as such, upon test completion, you will be brought back to this menu",
        "instead of being taken to the next reviewable test."
      )
    }
  }
  # Set last.id to test just before the one we want to review as process will
  # then cause desired test to be reviewed

  x@last.id <- as.integer(nav.id) - 1L
  x@browsing <- TRUE
  x@navigating <- TRUE
  return(x)
}
#' @rdname unitizer_prompt
#' @keywords internal

simple_prompt <- function(
  message, values=c("Y", "N"), prompt="unitizer> ", attempts=5L,
  case.sensitive=FALSE
) {
  if(!interactive_mode())
    stop("This function is only available in interactive mode")
  if(!is.character(message)) stop("Argument `message` must be character")
  if(!is.character(values) || length(values) < 1L || any(is.na(values)))
    stop("Argument `values` must be character with no NAs")
  if(!is.character(prompt) || length(prompt) != 1L || is.na(prompt))
    stop("Argument `prompt` must be character(1L) and not NA")
  if(
    !is.numeric(attempts) || length(attempts) != 1L || is.na(attempts) ||
    attempts < 1
  )
    stop("Argument `attempts` must be numeric(1L), not NA, and one or greater")
  if(!is.TF(case.sensitive))
    stop("Argument `case.sensitive` must be TRUE or FALSE")

  attempts <- attempts.left <- as.integer(attempts)
  val.tran <- if(!case.sensitive) tolower(values)

  meta_word_cat(message)

  while(attempts.left > 0L) {
    x <- read_line(prompt)
    if(!case.sensitive) x <- tolower(x)
    if(!(res.ind <- match(x, val.tran, nomatch=0L))) {
      meta_word_cat(
        paste(
          "Invalid input, please select one of:", paste(values, collapse=", ")
      ) )
    } else return(values[[res.ind]])
    attempts.left <- attempts.left - 1L
  }
  stop("Gave up trying to collect user input after ", attempts, " attempts.")
}
#' @keywords internal
#' @rdname unitizer_prompt


exit_fun <- function(y, env, valid.vals) {
  # keep re-prompting until user types in valid value
  if(!is.expression(y)) stop("Argument `y` should be an expression.")
  if(
    length(y) != 1L || !is.numeric(y[[1L]]) || length(y[[1L]]) != 1L ||
    y[[1L]] != as.integer(y[[1L]])
  ) return(FALSE)
  if(!isTRUE(y[[1L]] %in% valid.vals)) {
    meta_word_msg(
      "Type a number in `", deparse(valid.vals), "` at the prompt",
      sep="", trail.nl=FALSE
    )
    return(FALSE)
  }
  return(y[[1L]])
}
#' @keywords internal
#' @rdname unitizer_prompt

read_line <- function(prompt="") {
  stopifnot(is.chr1(prompt))
  if(is.null(.global$prompt.vals)) {
    readline(prompt)  # nocov can't test this in non-interactive
  } else if(!is.character(.global$prompt.vals)) {
    stop( # nocov start
      "Internal Error: internal object `.global$prompt.vals` has unexpected ",
      "value; contact maintainer."
    )     # nocov end
  } else if(!length(.global$prompt.vals)) {
    # Need dedicated condition so `unitizer_prompt` can catch it
    cond <- simpleCondition(
      "Internal Error: ran out of predefined readline input; contact maintainer."
    )
    class(cond) <- c("readError", "error", class(cond))
    stop(cond)
  } else {
    res <- .global$prompt.vals[[1L]]
    .global$prompt.vals <- tail(.global$prompt.vals, -1L)
    cat(prompt, res, "\n", sep="")
    res
  }
}
#' @keywords internal
#' @rdname unitizer_prompt

read_line_set_vals <- function(vals) {
  stopifnot(is.character(vals) || is.null(vals))
  .global$prompt.vals <- vals
}
#' @keywords internal
#' @rdname unitizer_prompt

interactive_mode <- function() {
  interactive() || is.character(.global$prompt.vals)
}

