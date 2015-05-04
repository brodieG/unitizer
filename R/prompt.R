#' @include exec.R

NULL

#' Handles The Actual User Interaction
#'
#' Will keep accepting user input until either:
#' \itemize{
#'   \item User types one of the names of \code{valid.opts}, typically "Y" or
#'     "N", but will vary
#'   \item User types "Q"
#'   \item User inputs an expression that when evaluated and fed to
#'     \code{exit.condition} returns TRUE
#' }
#' The set-up is intended to replicate something similar to what happens when
#' code hits a \code{browse()} statement.  User expressions are evaluated
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
#' @param help a character vector with help suggestions: the first value in the
#'   vector is \code{\link{word_cat}} output, the rest normal \code{cat}
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
#' @param ... additional arguments for \code{exit.condition}
#' @return mixed allowable user input

unitizer_prompt <- function(
  text,
  browse.env=new.env(
    parent=if(is.environment(pack.env$zero.env.par)) {
      pack.env$zero.env.par
    } else .GlobalEnv
  ),
  help=character(),
  valid.opts, hist.con=NULL, exit.condition=function(exp, env) FALSE,
  ...
) {
  if(!interactive())
    stop(
      "Logic Error: attempting to use interactive `unitizer` environment in ",
      "non-interactive session."
    )
  if(!is.null(hist.con) && (!inherits(hist.con, "file") || !isOpen(hist.con)))
    stop("Argument `hist.con` must be an open file connection or NULL")
  if(!is.environment(browse.env)) {
    stop("Argument `browse.env` must be an environment")
  }
  if(!is.character(valid.opts))
    stop("Argument `valid.opts` must be character")
  valid.opts <- c(valid.opts, Q="[Q]uit", H="[H]elp")
  # should validate other parameters as well
  opts.txt <- paste0(
    "(", paste0(valid.opts[nchar(valid.opts) > 0], collapse=", "), ")?"
  )
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
        word_cat(help[[1L]])
        if(length(help) > 1L) {
          cat(help[-1L], sep="")
        }
        cat("\n", sep="")
        word_cat(paste0(paste(text, opts.txt)))
      }
      next
    }
    # Check whether input should be captured specially

    res <- try(exit.condition(val, browse.env, ...))
    if(inherits(res, "try-error")) {
      stop("Logic Error: exit condition function failed; contact maintainer.")
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
    if(res$aborted || !length(val)) word_cat(text, opts.txt)  # error or no user input, re-prompt user
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
    stop(
      "Logic Error, expected unitizerBrowse object as param `x`; ",
      "contact maintainer."
    )
  }
  # User input

  prompt.val <- unitizer_prompt(
    text, browse.env=browse.env1, help=help, valid.opts=valid.opts,
    hist.con=x@hist.con
  )
  if(identical(prompt.val, "P")) {

    # Go back to previous
    if(curr.id == 1L) {
      message("At first reviewable item; nothing to step back to")
      return(x)
    }
    prev.tests <- x@mapping@item.id < curr.id & !x@mapping@ignored & (
      if(!identical(x@mode, "review")) x@mapping@review.type != "Passed"
      else TRUE
    )
    x@last.id <- if(any(prev.tests)) max(which(prev.tests)) - 1L else 0L
    x@navigating <- TRUE
    return(x)
  } else if (identical(prompt.val, "B")) {
    return(review_prompt(x, browse.env2))
  }
  return(prompt.val)
}
#' Manages Producing Test Navigation Message / Prompt
#'
#' Probably should be an S4 method, along with \code{`\link{navigate_prompt}`}
#'
#' @keywords internal
#' @param x a unitizerBrowse object
#' @param nav.env an environment
#' @return either a \code{`unitizerBrowse`}, or "Q" if the user chose to quit

review_prompt <- function(x, nav.env) {

  if(!is(x, "unitizerBrowse") || !is.environment(nav.env))
    stop(
      "Logic Error: unexpected inputs to internal function; contact maintainer."
    )
  # Navigation Prompt

  nav.help <- paste0(
    "Select a test to review by typing that test's number at the prompt. ",
    "Tests that start with a `*`",
    if(identical(x@mode, "unitize")) ", or with status \"Passed\",", "are not ",
    "typically reviewed in this mode.  The letter after the test status ",
    "represents prior user input to test review (a `-` indicates test has not ",
    " been reviewed). Type \"U\" to jump to the first unreviewed ",
    "test.\n\n",
    "Note that tests are displayed in the order they appear in the test",
    "file, not in the order they would be reviewed in, which is why the test",
    "numbers are not necessarily sequential (see vignette for details and",
    "exceptions).\n"
  )
  nav.opts <- c(
    "input a test number",
    U="[U]nreviewed"
  )
  nav.prompt <- "What test do you wish to review"
  show(x)
  word_cat(nav.prompt, paste0("(", paste0(nav.opts, collapse=", "), ")?"))
  nav.id <- unitizer_prompt(
    text=nav.prompt, help=nav.help, browse.env=nav.env, exit.condition=exit_fun,
    valid.opts=nav.opts, valid.vals=x@mapping@item.id
  )
  if(identical(nav.id, "Q")) {
    return("Q")
  } else if (identical(nav.id, "U")) {
    # Go to unreviewed test

    unreviewed <- !x@mapping@reviewed & !x@mapping@ignored &
      (
        if(!identical(x@mode, "review")) x@mapping@review.type != "Passed"
        else TRUE
      )
    item.len <- length(x@mapping@review.val)
    if(!any(unreviewed)) {
      message("No unreviewed tests.")
      x@last.id <- item.len
      return(x)
    }
    message("Jumping to first unreviewed test.") # But note that we also show all ignored tests before that one for context
    nav.id <- min(which(unreviewed))
  } else if (
    !is.numeric(nav.id) || length(nav.id) != 1L || as.integer(nav.id) != nav.id
  ) {
    stop(
      "Logic Error: Unexpected user input allowed through in Review mode; ",
      "contact maintainer")
  }
  # Determine whether test we selected is a test we would normally not review

  x@inspect.all <- x@mapping@ignored[[nav.id]] || (
      identical(x@mode, "unitize") &&
      identical(as.character(x@mapping@review.type[[nav.id]]), "Passed")
    )
  x@review <- x@inspect.all

  if(x@inspect.all) {
    message(
      "You selected a test that is not normally reviewed in this mode; ",
      "as such, upon test completion, you will be brought back to this menu ",
      "instead of being taken to the next reviewable test."
    )
  }
  # Set last.id to test just before the one we want to review as process will
  # then cause desired test to be reviewed

  x@last.id <- as.integer(nav.id) - 1L
  x@navigating <- TRUE
  return(x)
}
#' A Simple Prompting Function
#'
#' @param message character ask the user a question
#' @param values character valid responses
#' @param prompt see \code{\link{readline}}
#' @param attempts how many times to try before erroring
#' @return one of \code{values} as selected by user

simple_prompt <- function(
  message, values=c("Y", "N"), prompt="unitizer> ", attempts=5L,
  case.sensitive=FALSE
) {
  if(!interactive()) stop("This function is only available in interactive mode")
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

  attempts <- attempts.left <- as.integer(attempts)
  val.tran <- if(!case.sensitive) tolower(values)

  word_cat(message)

  while(attempts.left > 0L) {
    x <- readline(prompt)
    if(!case.sensitive) x <- tolower(x)
    if(!(res.ind <- match(x, val.tran, nomatch=0L))) {
      word_cat(
        paste(
          "Invalid input, please select one of: ", paste(values, collapse=", ")
      ) )
    } else return(values[[res.ind]])
    attempts.left <- attempts.left - 1L
  }
  stop("Gave up trying to collect user input after ", attempts, " attempts.")
}
#' An Exit Fun For Prompts Expecting a Specific Selection
#'
#' Intended for use solely with \code{\link{unitizer_prompt}}
#'
#' @keywords internal
#' @param valid.vals vector to check user input against

exit_fun <- function(y, env, valid.vals) {               # keep re-prompting until user types in valid value
  if(!is.expression(y)) stop("Argument `y` should be an expression.")
  if(
    length(y) != 1L || !is.numeric(y[[1L]]) || length(y[[1L]]) != 1L ||
    y[[1L]] != as.integer(y[[1L]])
  ) return(FALSE)
  if(!isTRUE(y[[1L]] %in% valid.vals)) {
    word_msg("Input must be in `", deparse(valid.vals), "`", sep="")
    return(FALSE)
  }
  return(y[[1L]])
}


