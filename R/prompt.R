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
#' @seealso browse_testor_items
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
#'   cause the prompt loop to exit
#' @return mixed allowable user input

testor_prompt <- function(
  text, browse.env=globalenv(), help=character(), 
  valid.opts, hist.con=NULL, exit.condition=function(...) FALSE
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
    while(inherits(try(val <- faux_prompt("testor> ")), "try-error")) NULL 

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
    warn.opt <- getOption("warn")     # Need to ensure warn=1 so that things work properly
    on.exit(options(warn=warn.opt))
    if(warn.opt != 1L) options(warn=1L)
    trace.res <- NULL

    # Note `val` here is the expression the user inputted, not the result of the
    # evaluation.  The latter will be in res$value

    res <- eval_user_exp(val, browse.env)

    # store / record history
      
    if(!is.null(hist.con)) {
      cat(deparse(val), file=hist.con, sep="\n")
      loadhistory(showConnections()[as.character(hist.con), "description"])    
    }
    # go through conditions and issue warnings and errors as appropriate; note
    # one issue here is that we don't really know whether these warnings/errors
    # were really emitted by warning() and stop()

    lapply(
      res$conditions,
      function(x) {
        cond.type <- if(identical(class(x), c("simpleWarning", "warning", "condition"))) {
          "Warning"
        } else if(identical(class(x), c("simpleError", "error", "condition"))) {
          "Error"
        }
        if(!is.null(cond.type))
          message(cond.type, " in ", getCall(x), " : ", conditionMessage(x))
    } )
    if(res$aborted || !length(val)) cat(text, opts.txt)  # error or no user input, re-prompt user
    if(exit.condition(res$value)) return(res$value)      # user result allows break of prompt loop
    if(res$aborted && !is.null(res$trace)) set_trace(res$trace)  # make error trace available for `traceback()`
} }
#' Wrapper Around User Interaction
#' 
#' Specifically for cases were user has the choice to input something or to try
#' to navigate to another test.
#' 
#' @seealso testor_prompt
#' @inheritParams testor_prompt
#' @param x a testor.browse object
#' @param browse.env1 environment to have user review tests, run commands, etc
#' @param browse.env2 navigation environment
#' @param curr.id which id we are currently browsing
#' @keywords internal

navigate_prompt <- function(  
  x, curr.id, text, browse.env1=globalenv(), browse.env2=globalenv(), 
  help=character(), valid.opts
) {
  if(!is(x, "testorBrowse")) {
    stop("Logic Error, expected testorBrowse object as param `x`; contact maintainer.")
  }
  # User input

  prompt.val <- testor_prompt(
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
    x@last.id <- if(length(prev.tests)) max(which(prev.tests)) - 1L else 0L
    return(x)
  } else if (identical(prompt.val, "R")) {  
    
    # Navigation Prompt

    if(!length(x@mapping@item.id[x@mapping@reviewed])) {
      message("No reviewed tests yet")
      return(x)
    }
    nav.help <- paste0(
      "You may re-review any of the tests that you have already reviewed by ",
      "selecting that test's number.  The numbering is not continuous because ",
      "some statements in the store are not considered tests (e.g. assignments)."
    )
    nav.opts <- c(
      "An integer-like number corresponding to a test",  Q="[Q]uit", H="[H]elp"
    )
    nav.prompt <- "What test do you wish to review"
    cat(nav.prompt, " (", paste0(nav.opts, collapse=", "), ")?\n\n", sep="")
    show(x)
    exit.fun <- function(y) {               # keep re-prompting until user types in valid value
      valid.vals <- x@mapping@item.id[x@mapping@reviewed]
      if(!isTRUE(y %in% valid.vals)) {
        message(
          "Input must be integer-like and in ", 
          paste0(range(valid.vals), collapse="-")
        )
        return(FALSE)
      }
      return(TRUE)
    }
    nav.id <- testor_prompt(
      text=nav.prompt, help=nav.help,
      browse.env=browse.env2, exit.condition=exit.fun, 
      valid.opts=nav.opts
    )
    prev.tests <- x@mapping@item.id[!x@mapping@ignored] < nav.id
    x@last.id <- if(length(prev.tests)) max(which(prev.tests)) else 0L
    return(x)
  }
  return(prompt.val)
}