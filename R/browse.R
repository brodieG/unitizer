#' @include testor.R
#' @include misc.R
#' @include browse.struct.R

setGeneric("browse", function(x, ...) standardGeneric("browse"))
valid.opts.def <- c(Y="[Y]es", N="[N]o", Q="[Q]uit", H="[H]elp")

#' Browse testor
#' 
#' Here we are reviewing all the tests in the testor under three different lenses
#' \enumerate{
#'   \item tests that don't match the stored reference tests
#'   \item tests that don't exist in the reference tests
#'   \item tests that exist in the reference tests but no the new file
#' }
#' Because a lot of the logic for browsing these three types of situations is
#' shared, that logic has been split off into \code{`\link{browse_testor_items}`}.
#' The key is that that function will return the items that are supposed to be
#' stored in the testor.  These items will either be new or reference ones
#' based on user decisions.
#' 
#' Unfortunately, in order to be able to use the same logic for tasks that are
#' not quite the same, a bit of contortion was needed.  In particular, the
#' user is always asked to input either Y, N, or Q, but the corresponding output
#' from \code{`\link{browse_testor_items}`} is very different depending on what
#' situation we're dealing with.
#' 
#' @keywords internal
#' @param x the object to browse
#' @param env the environment to use as a parent to the environment to browse the tests

setMethod("browse", c("testor"), valueClass="testor",
  function(x, ...) {

    # set up local history

    savehistory()    
    hist.file <- tempfile()
    hist.con <- file(hist.file, "at")
    cat("## <testor> (original history will be restored on exit)\n", file=hist.con)
    loadhistory(showConnections()[as.character(hist.con), "description"])
    on.exit({close(hist.con); file.remove(hist.file); loadhistory()})

    testor.browse <- browsePrep(x)   # Group tests by section and outcome for review
    testor.browse@hist.con <- hist.con

    while(!done(testor.browse <- reviewNext(testor.browse))) NULL # Interactively review all tests
 
    # Get summary of changes

    keep <- !testor.browse@mapping@ignored
    changes <- split(
      testor.browse@mapping@review.val[keep], 
      testor.browse@mapping@review.type[keep]
    )
    change.sum <- lapply(changes, function(x) c(sum(x == "Y"), length(x)))
    for(i in names(change.sum)) slot(x@changes, tolower(i)) <- change.sum[[i]]

    items.user <- processInput(testor.browse)

    items.ref <- x@items.new[x@tests.status == "Pass"] + items.user
    items.ref <- healEnvs(items.ref, x) # repair the environment ancestry

    zero.env <- new.env(parent=parent.env(x@zero.env))
    testor <- new("testor", id=x@id, changes=x@changes, zero.env=zero.env)
    testor + items.ref
} )

#' Bring up Review of Next test
#' 
#' Generally we will go from one test to the next, where the next test is 
#' determined by the value of \code{`x@@last.id`}.  This means it is possible
#' to affect the browsing order by modifying \code{`x@@last.id`}.
#' 
#' This method is in charge of displaying all the output for review.
#' 
#' @keywords internal

setGeneric("reviewNext", function(x, ...) standardGeneric("reviewNext"))
setMethod("reviewNext", c("testorBrowse"), 
  function(x, ...) {
    curr.id <- x@last.id + 1L
    if(x@last.reviewed) {
      last.reviewed.sec <- x@mapping@sec.id[[which(x@mapping@item.id == x@last.reviewed)]]
      last.reviewed.sub.sec <- x@mapping@sub.sec.id[[which(x@mapping@item.id == x@last.reviewed)]]
      furthest.reviewed <- if(length(which(x@mapping@reviewed))) 
        max(which(x@mapping@reviewed)) else 0L
    } else {
      last.reviewed.sec <- last.reviewed.sub.sec <- furthest.reviewed <- 0L
    }
    curr.sec <- x@mapping@sec.id[[which(x@mapping@item.id == curr.id)]]
    curr.sub.sec <- x@mapping@sub.sec.id[[which(x@mapping@item.id == curr.id)]]
    curr.sub.sec.obj <- x[[curr.sec]][[curr.sub.sec]]
    id.rel <- x@mapping@item.id.rel[[which(x@mapping@item.id == curr.id)]]

    valid.opts <- c(
      Y="[Y]es", N="[N]o", B="[B]ack", R="[R]eview", Q="[Q]uit", H="[H]elp"
    )
    if(        # Print Section title if appropriate
      !identical(last.reviewed.sec, curr.sec) && 
      !all(x@mapping@ignored[x@mapping@sec.id == curr.sec]) &&
      length(unique(x@mapping@sec.id[!x@mapping@ignored])) > 1L
    ) {
      print(H2(x[[curr.sec]]@section.title))  
    }
    if(        # Print sub-section title if appropriate
      !identical(last.reviewed.sub.sec, curr.sub.sec) &&
      !all(x@mapping@ignored[x@mapping@sub.sec.id == curr.sub.sec])
      ) {
      print(H3(curr.sub.sec.obj@title))
      cat(
        curr.sub.sec.obj@detail, " For each item, choose whether to ", 
        tolower(curr.sub.sec.obj@prompt),":\n\n", sep=""
    ) }
    # Retrieve actual tests objects

    item.new <- if(!is.null(curr.sub.sec.obj@items.new)) 
      curr.sub.sec.obj@items.new[[id.rel]]
    item.ref <- if(!is.null(curr.sub.sec.obj@items.ref)) 
      curr.sub.sec.obj@items.ref[[id.rel]]
    if(is.null(item.new)) {
      item.main <- item.ref
      base.env.pri <- parent.env(curr.sub.sec.obj@items.ref@base.env)
    } else {
      item.main <- item.new
      base.env.pri <- parent.env(curr.sub.sec.obj@items.new@base.env)
    }
    if(length(item.main@comment)) cat(item.main@comment, sep="\n")
    cat(deparse_prompt(item.main@call), sep="\n")
    if(
      is(curr.sub.sec.obj@show.fail, "testorItemsTestsErrors") && 
      !item.main@ignore
    ) {
      cat(as.character(curr.sub.sec.obj@show.fail[[id.rel]]), sep="\n")
    } 
    # If there are conditions that showed up in main that are not in reference
    # show the message

    if(!is.null(item.new) && !is.null(item.ref) && 
      !isTRUE(all.equal(item.new@data@conditions, item.ref@data@conditions)) ||
      curr.sub.sec.obj@show.msg
    ) {
      screen_out(
        item.main@data@message, 
        max.len=getOption("testor.test.msg.lines"), stderr()
    ) }
    if(curr.sub.sec.obj@show.out) screen_out(item.main@data@output)

    # No need to do anything else with ignored tests since default action for 
    # them is "Y", so return those

    if(x@mapping@ignored[[curr.id]]) return(x)  
    if(x@mapping@reviewed[[curr.id]]) {
      message(
        "Test has been reviewed with user input: \"", 
        x@mapping@review.val[[curr.id]], "\""
    ) }      
    # Create evaluation environment; these are really two nested environments,
    # with the parent environment containing the testorItem values and the child 
    # environment containing the actual testor items.  This is so that when
    # user evaluates `.new` or `.ref` they see the value, but then we can 
    # easily retrieve the full object with the `get*` functions.
    
    var.list <- list()        
    var.sub.list <- list()
    if(!is.null(item.new)) {
      var.list <- c(var.list, list(.new=item.new))
      var.sub.list <- c(var.sub.list, list(.new=item.new@data@value))
    }
    if(!is.null(item.ref)) {
      var.list <- c(var.list, list(.ref=item.ref)) 
      var.sub.list <- c(var.sub.list, list(.ref=item.ref@data@value))
    }
    browse.par.env <- list2env(var.list, parent=item.main@env)
    browse.env <- list2env(var.sub.list, parent=browse.par.env)
    browse.eval.env <- new.env(parent=browse.env)

    env.sec <- if(!is.null(item.new) && !is.null(item.ref)) item.ref@env else NULL
    assign("ls", testor_ls, base.env.pri)
    if(!is.null(env.sec)) {
      assign("ref", function(x) eval(substitute(x), env.sec), base.env.pri)
    } else {      
      assign(
        "ref", 
        function(x) {
          message(
            "`ref` is only active when there is an active secondary environment"
        ) },
        base.env.pri
    ) }
    get.msg <- character()
    if(!is.null(item.new)) get.msg <- "`getTest(.new)`"
    if(!is.null(item.ref)) get.msg <- c(get.msg, "`getTest(.ref)`")
    
    # We went back in our tests, now give option to go back to furthest along
    # reviewed test

    help.prompt <- paste(
      "In addition to any valid R expression, you may type the following",
      "at the prompt (without backticks):"
    )
    help.opts <- c(
      "`B` to go Back to the previous test",
      "`R` to see a listing of all previously reviewed tests",
      if(furthest.reviewed > curr.id) "\"U\" to go to first unreviewed test",
      "`ls()` to see what objects are available to inspect",
      paste0(collapse="",
        paste0(get.msg, collapse=" or "),
        "to see more details about the test (see documentation for `getTest` ",
        "for details on other accessor functions such as (",
        paste0(paste0("`", names(getItemFuns), "`"), collapse=", "), ")."
    ) )
    if(furthest.reviewed > curr.id) {
      valid.opts <- append(valid.opts, c(U="[U]nreviewed"), after=4L)
    }
    # User input

    prompt.val <- testor_prompt(
      curr.sub.sec.obj@prompt, browse.env=browse.eval.env, 
      help=c(help.prompt, as.character(UL(help.opts))),
      valid.opts=valid.opts, hist.con=x@hist.con
    )
    if(identical(prompt.val, "B")) {          # Go back to previous
      if(curr.id == 1L) {
        message("At first reviewable item; nothing to undo")
        x@last.reviewed <- curr.id
        return(x)
      }
      x@last.id <- curr.id - 1L
      return(x)
    } else if (identical(prompt.val, "R")) {  # Navigation Prompt
      if(!length(x@mapping@item.id[x@mapping@reviewed])) {
        message("No reviewed tests yet")
        x@last.reviewed <- curr.id
        return(x)
      }
      show(x)
      exit.fun <- function(y) {               # keep re-prompting until user types in valid value
        valid.vals <- x@mapping@item.id[x@mapping@reviewed]
        if(!isTRUE(y %in% valid.vals)) {
          message(
            "Input must be integer-like and in ", 
            paste(range(valid.vals), sep="-")
          )
          return(FALSE)
        }
        return(TRUE)
      }
      nav.help <- paste0(
        "You may re-review any of the tests that you have already reviewed by ",
        "selecting that test's number.  The numbering is not continuous because ",
        "some statements in the store are not considered tests (e.g. assignments)."
      )
      x@last.id <- testor_prompt(
        text="Type in number of test you wish to review:", help=nav.help,
        browse.env=parent.env(base.env.pri), exit.condition=exit.fun, 
        valid.opts=c(
          "Type an integer-like number corresponding to a test", 
          Q="[Q]uit", H="[H]elp"
      ) )
    } else if (prompt.val %in% c("Y", "N")) {
      x@mapping@reviewed[[curr.id]] <- TRUE
      x@mapping@review.val[[curr.id]] <- prompt.val
      x@last.id <- x@last.reviewed <- curr.id
    } else {
      stop("Logic Error: `testor_prompt` returned unexpected value; contact maintainer")
    }
    x
  }
)
#' Handles The Actual User Interaction
#' 
#' Will keep accepting user input until either:
#' \itemize{
#'   \item User types one of the names of \code{`valid.opts`}, typically "Y" or 
#'     "N"
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
#' @param help a character vector with help suggestions
#' @param hist.con connection to save history to

testor_prompt <- function(
  text, browse.env=globalenv(), help=character(), 
  valid.opts, hist.con=NULL, exit.condition=function(...) FALSE
) {  
  if(!is.null(hist.con) && (!inherits(hist.con, "file") || !isOpen(hist.con)))
    stop("Argument `hist.con` must be an open file connection or NULL")
  if(!is.environment(browse.env))
    stop("Argument `browse.env` must be an environment")
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
      stop("User quit.")
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

    evaled <- lapply(val, 
      function(x) {
        res <- NULL
        withRestarts(
          withCallingHandlers(
            res <- eval(call("withVisible", x), browse.env),
            warning=function(e) {
              warning(simpleCondition(conditionMessage(e), x))
              invokeRestart("muffleWarning")
            },
            error=function(e) {
              message(simpleCondition(paste0("Error: ", conditionMessage(e), "\n"), x))
              invokeRestart("abort")
            }
          ),
          abort=function(e) NULL
        )
        if(!is.null(hist.con)) {
          cat(deparse(x), file=hist.con, sep="\n")
          loadhistory(showConnections()[as.character(hist.con), "description"])
        }
        if(is.null(res)) {
          return(list(FALSE, res))  
        } else {
          if(res$visible) print(res$value)
          return(list(TRUE, res$value))
    } } )    
    if(
      !all(vapply(evaled, `[[`, logical(1L), 1L)) || 
      identical(length(evaled), 0L) 
    ) cat(text, opts.txt)
    if(length(evaled)) {
      last.val <- evaled[[length(evaled)]][[2L]]  # exit loop if value meets exit condition
      if(exit.condition(last.val)) return(last.val)
} } }

#' Retrieves Additional Info About Test
#' 
#' Uses the test result to identify whether the get request was issued on the
#' test object or the reference test object.  The check is to ensure the user
#' didn't modify the object (maybe we don't need to do this?).
#' 
#' These functions rely on the \code{`ref`} and \code{`obj`} objects being 
#' defined in their parent environment.
#' 
#' @keywords internal

getItemData <- function(x, name, what, env) {
  if(!(name %in% c(".new", ".ref"))) stop("testor::get* functions may only be called on the test objects (`.new`, or `.ref`).")
  if(!(is.environment(env))) stop("Argument `env` must be an environment")
  if(inherits(obj <- try(get(name, inherits=FALSE, envir=env), silent=TRUE), "try-error")) {
    stop("Requested test object `", name, "` is not defined for this test.")
  }
  if(!is(obj, "testorItem")) stop("Logic Error: retrieved object is not a `testorItem`; contact package maintainer.")
  if(!identical(obj@data@value, x)) {
    stop(
      "Passed `", name, "` value cannot be matched to the test that puportedly produced it; ",
      "are you sure you did not change the value of `", name,"`?"
  ) }
  if(identical(what, "test")) return(obj)
  if(identical(what, "call")) return(obj@call)
  if(!(what %in% slotNames(obj@data))) stop("Logic Error: unknown slot, contact maintainer.")
  slot(obj@data, what)
}

#' Retrieve Additional Info About Tests
#' 
#' Intended for use exclusively within the \code{`testor`} interactive command 
#' line.  For example \code{getMsg(.new)} will retrieve any \file{stderr} that occurred
#' during test evaluation (for reference tests, use \code{getMsg(.ref)}.
#' 
#' @name getTest
#' @usage getTest(x)
#' @aliases getVal, getConds, getMsg, getOut, getAborted
#' @param x object to get additional data for (should be one of \code{`obj`}, \code{`ref`})
#' @return depends on what you requested:
#' \itemize{
#'   \item \code{`getConds`}: the conditions as a list of conditions or an
#'     empty list if no conditions occurred.
#'   \item \code{`getOut`}: the screen output (i.e. anything produced by cat/print,
#'     or any visible evaluation output) as a character vector
#'   \item \code{`getMsg`}: anything that was output to \code{`stderr`}, mostly
#'     this is all contained in the conditions as well, though there could be
#'     other output here, as a character vector
#'   \item \code{`getExpr`}: the call that was tested as an unevaluated call,
#'     but keep in mind that if you intend to evaluate this for a reference
#'     item the environment may not be the same so you could get different
#'     results (\code{`ls`} will provide more details)
#'   \item \code{`getAborted`}: returns whether the test call issues a restart
#'     call to the `abort` restart, as `stop` does.
#'   \item \code{`getVal`}: the value that results from evaluating the test, note
#'     that typing \code{`obj`} and \code{getVal(`obj`)} at the \code{`testor`}
#'     prompt are equivalent
#'   \item \code{`reCall`}: will load the call used to generate the test
#'     on the prompt (not implemented yet).
#' }

NULL
getItemSlots <- c(
  getTest="test", getConds="conditions", getVal="value", 
  getMsg="message", getOut="output", getAborted="aborted", getExpr="call"
)
getItemFuns <- lapply(
  getItemSlots, function(slot) { 
    force(slot)
    function(x) getItemData(x, deparse(substitute(x)), slot, parent.env(parent.env(parent.frame())))
} )
getItemFuns <- c(getItemFuns, list(recall=function(x) stop("Not Implemented")))