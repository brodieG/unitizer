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

    savehistory()
    on.exit(loadhistory())

    testor.browse <- browsePrep(x)   # Group tests by section and outcome for review

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
#' determined by the value of \code{`x@last.id`}.  This means it is possible
#' to affect the browsing order by modifying \code{`x@last.id`}.
#' 
#' This method is in charge of displaying all the output for review.
#' 
#' @keywords internal

setGeneric("reviewNext", function(x, ...) standardGeneric("reviewNext"))
setMethod("reviewNext", c("testorBrowse"), 
  function(x, ...) {
    curr.id <- x@last.id + 1L
    x@last.id <- curr.id
    new.opts <- append(valid.opts.def, c(U="[U]ndo"), after=2L)

    last.reviewed.sec <- x@mapping[x@mapping$item.id == x@last.reviewed, "sec.id"]
    last.reviewed.sub.sec <- x@mapping[x@mapping$item.id == x@last.reviewed, "sub.sec.id"]
    curr.sec <- x@mapping[x@mapping$item.id == curr.id, "sec.id"]
    curr.sub.sec <- x@mapping[x@mapping$item.id == curr.id, "sub.sec.id"]
    curr.sub.sec.obj <- x[[curr.sec]][[curr.sub.sec]]
    id.rel <- x@mapping[x@mapping$item.id == curr.id, "item.id.rel"]

    if(        # Print Section title if appropriate
      !identical(last.reviewed.sec, curr.sec) && 
      !all(x@mapping$ignored[x@mapping$sec.id == curr.sec]) &&
      length(unique(x@mapping$section.id[!x@mapping$ignored])) > 1L
    ) {
      print(h2(x[[curr.sec]]@section.title))  
    }
    if(        # Print sub-section title if appropriate
      !identical(last.reviewed.sub.sec, curr.sub.sec) &&
      !all(x@mapping$ignored[x@mapping$sub.sec.id == curr.sub.sec])
      ) {
      print(H3(curr.obj@title))
      cat(
        curr.sub.sec.obj@detail, " For each item, choose whether to ", 
        tolower(curr.sub.sec.obj@prompt), 
        " (", paste0(new.opts, collapse=", "), "):\n\n", sep=""
    ) }
    # Retrieve actual tests objects

    item.new <- if(!is.null(curr.sub.sec.obj@items.new)) 
      curr.sub.sec.obj@items.new[[id.rel]]
    item.ref <- if(!is.null(curr.sub.sec.obj@items.ref)) 
      curr.sub.sec.obj@items.ref[[id.rel]]
    item.main <- if(is.null(item.new)) item.ref else item.new

    if(length(item.main@comment)) cat(item.main@comment, sep="\n")
    cat(deparse_prompt(item.main@call), sep="\n")
    if(is(show.fail, "testorItemsTestsErrors") && !item.main@ignore) {
      cat(as.character(curr.obj@show.fail[[id.rel]]), sep="\n")
    } 
    # If there are conditions that showed up in main that are not in reference
    # show the message

    if(!is.null(item.new) && !is.null(item.ref) && 
      !isTRUE(all.equal(item.new@data@conditions, item.ref@data@conditions)) ||
      show.msg
    ) {
      screen_out(
        item.main@data@message, 
        max.len=getOption("testor.test.msg.lines"), stderr()
    ) }
    if(show.out) screen_out(item.main@data@output)

    # No need to do anything else with ignored tests since default action for 
    # them is "Y", so return those

    if(x@mapping$ignored[[curr.id]]) return(x)  

    if(x@mapping$reviewed[[curr.id]]) {
      message(
        "Test has been reviewed with user input: \"", 
        x@mapping$review.val[[curr.id]], "\""
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
    parent.env(browse.eval.env) <- browse.env

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
    help <- paste0(
      "Type ls() to see what objects are available to inspect, and use ",
      paste0(get.msg, collapse=" or "),
      " to see more details about the test (see documentation for `getTest` ",
      "for details on other accessor functions such as ", 
      paste0(paste0("`", names(getItemFuns), "`"), collapse=", "), ")."
    )
    # User input

    prompt.val <- testor_prompt(prompt, browse.eval.env, help, new.opts, hist.con)
    if(identical(prompt.val, "U")) {          # Undo
      if(curr.id == 1L) {
        message("At first reviewable item; nothing to undo")
        return(x)
      }
      x@last.id <- curr.id - 1L
    } else if (identical(prompt.val, "R")) {  # Navigation Prompt
      show(x)

    } else {
      x@mapping$reviewed[[curr.id]] <- TRUE
      x@mapping$review.val[[curr.id]] <- prompt.val
      x@last.id <- x@last.reviewed <- curr.id
    }
    x
  }
)




# NEED TO EXPAND testor_prompt TO HANDLE NUMERIC INPUTS, MIGHT HAVE TO RETHINK
# A LITTLE HOW THIS IS IMPLEMENTED; MAYBE IF A NUMBER PASS A VALIDATOR?  HAS
# TO HAPPEN INSIDE testor_prompt DUE TO THE LOOP.


#' Handles The Actual User Interaction
#' 
#' @keywords internal
#' @seealso browse_testor_items
#' @param text the prompt text to display
#' @param browse.env the environment to evaluate user expressions in; typically
#'   this will contain interesting objects (use \code{ls()} to review)
#' @param help a character vector with help suggestions
#' @param hist.con connection to save history to

testor_prompt <- function(
  text, browse.env, help=character(), 
  valid.opts=valid.opts.def, hist.con=NULL
) {  
  if(!is.null(hist.con) && (!inherits(hist.con, "file") || !isOpen(hist.con)))
    stop("Argument `hist.con` must be an open file connection or NULL")
  repeat {
    opts <- paste0("(", paste0(valid.opts, collapse=", "), ")?")
    while(inherits(try(val <- faux_prompt("testor> ")), "try-error")) NULL 
    if(
      length(val) == 1L && is.symbol(val[[1]]) && 
      as.character(val[[1]]) %in% names(valid.opts) && 
      !(as.character(val[[1]]) %in% c("Q", "H"))
    ) {
      return(as.character(val[[1]]))
    } else if (length(val) == 1L && identical(val[[1]], quote(Q))) {
      stop("User quit.")
    } else if (length(val) == 1L && identical(val[[1]], quote(H))) {
      if(!length(help)) {
        cat("No help available.", paste(text, opts), sep="\n")
      } else {
        cat(help, paste(text, opts), sep="\n")
      }
      next
    }
    warn.opt <- getOption("warn")     # Need to ensure warn=1 so that things work properly
    on.exit(options(warn=warn.opt))
    if(warn.opt != 1L) options(warn=1L)

    evaled <- vapply(val, 
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
          return(FALSE)  
        } else {
          if(res$visible) print(res$value)
          return(TRUE)
      } },
      logical(1L)
    )
    if(!all(evaled) || identical(length(evaled), 0L)) cat(text, opts)
  }
}

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