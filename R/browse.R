#' @include testor.R

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
#' The key is that that funciton will return the items that are supposed to be
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

    # The following is a translation layer between the old way of doing things,
    # and the new S4 way; at some point this should be rationalized but this
    # was a way to get it working faster.

    items.1 <- items.2 <- items.3 <- items.4 <- new("testorItems")
    mult.sects <- sum(vapply(x@sections, function(y) if(length(y)) 1L else 0L, integer(1L))) > 1L
    for(i in unique(x@section.parent)) {  # Loop through parent sections
      sect.map <- x@section.map %in% which(x@section.parent == i)  # all items in parent section
      if(
        sum(vapply(x@sections[which(x@section.parent == i)], length, integer(1L))) == 0L || 
        ( 
          length(which(x@tests.fail & sect.map & !ignored(x@items.new))) == 0L && 
          length(which(x@tests.new & sect.map & !ignored(x@items.new))) == 0L && 
          length(which(x@tests.error & sect.map & !ignored(x@items.new))) == 0L
        )
      ) {
         next
      }
      if(mult.sects) {
        print(H2(x@sections[[i]]@title)) 
      }
      items.1 <- items.1 + browse_testor_items(
        title=paste0("Review ", length(which(x@tests.fail & sect.map & !ignored(x@items.new))), " Failed Tests"), 
        detail="Reference test does not match new test from test script.",
        prompt="Overwrite item in store with new value", 
        actions=c(Y="A", N="B"), items.new=x@items.new[x@tests.fail & sect.map],
        show.fail=x@tests.errorDetails[x@tests.fail & sect.map], 
        items.ref=x@items.ref[x@items.new.map[x@tests.fail & sect.map]]
      )
      items.2 <- items.2 + browse_testor_items(
        title=paste0("Review ", length(which(x@tests.new & sect.map & !ignored(x@items.new))), " New Calls"), 
        detail="Test script contains tests not present in testor.",
        prompt="Add new item to store", actions=c(Y="A", N="C"), 
        show.msg=TRUE, show.out=TRUE, items.new=x@items.new[x@tests.new & sect.map]
      )
      items.4 <- items.4 + browse_testor_items(
        title=paste0("Review ", length(which(x@tests.error & sect.map & !ignored(x@items.new))), " Corrupted Tests"), 
        detail=paste0(
          "Reference tests cannot be compared to new tests because errors occurred ",
          "while attempting comparison. Please review the error and contemplate using ",
          "a different comparison function with `testor_sect`."
        ),
        prompt="Overwrite item in store with new value", 
        actions=c(Y="A", N="B"), items.new=x@items.new[x@tests.error & sect.map],
        show.fail=x@tests.errorDetails[x@tests.error & sect.map], 
        items.ref=x@items.ref[x@items.new.map[x@tests.error & sect.map]]
      )
    }
    # The following allows review of the deleted items
    
    if(removed <- length(which(!ignored(x@items.ref[is.na(x@items.ref.map)])))) {
      print(H2("Missing Tests"))
      cat(
        "The following ", removed, " tests are in the reference testor but not ",
        "in the test file:\n", sep=""
      )
      lapply(as.list(x@items.ref)[is.na(x@items.ref.map) & !ignored(x@items.ref)], function(y) cat(deparse_prompt(y@call), sep="\n"))
      cat(text <- "Review tests prior to removal", "([Y]es, [N]o, [Q]uit, [H]elp)?\n")
      help <- paste0(
        "If you press N, tests no longer present in test file will be discarded; if you press Y ",
        "you will have an opportunity to review tests individually to see if you want to keep them ",
        "or not."
      )
      if(identical(testor_prompt(text, new.env(parent=x@base.env), help), "Y")) {
        items.3 <- browse_testor_items(
          title="Review Calls to Remove", detail="The following test exists in testor but not in the new test script.", 
          prompt="Remove item from store", actions=c(Y="C", N="B"), 
          items.ref=x@items.ref[is.na(x@items.ref.map) & !ignored(x@items.ref)]
    ) } }
    x@changes@removed <- c(
      length(which(is.na(x@items.ref.map) & !ignored(x@items.ref))) - length(!ignored(items.3)), 
      length(which(is.na(x@items.ref.map) & !ignored(x@items.ref)))
    )
    x@changes@failed <- c(
      length(which(itemsType(items.1) == "new" & !ignored(items.1))), 
      length(which(x@tests.status == "Fail" & !ignored(x@items.new)))
    )
    x@changes@new <- c(length(which(!ignored(items.2))), length(which(x@tests.new & !ignored(x@items.new))))
    x@changes@error <- c(
      length(which(itemsType(items.4) == "new" & !ignored(items.4))), 
      length(which(x@tests.status == "Error" & !ignored(x@items.new)))
    )
    items.ref <- x@items.new[x@tests.status == "Pass"] + items.1 + items.2 + items.3 + items.4
    items.ref <- healEnvs(items.ref, x) # repair the environment ancestry

    zero.env <- new.env(parent=parent.env(x@zero.env))
    testor <- new("testor", id=x@id, changes=x@changes, zero.env=zero.env)
    testor + items.ref
} )

# Manage The Interactive Portion of testor
# 
# Rationalizes the code to vet failed tests, added tests, and removed tests.
# Based on user input return a list of the testor_items that will eventually
# be stored.
# 
# @keywords internal
# @param title character 1 length current test types (failed, added, removed)
# @param prompt character 1 length what to prompt the user to do
# @param actions character 2 length containing c("A", "B", "C"), where "A"
#   means return value from new item list, "B" return value from old item
#   list (the original store) and "C" means return NULL.  The first value
#   corresponds to the action on user typing `Y`, the second the action on 
#   user typing `N`.
# @param show.msg logical whether to show stderr produced during evaluation
# @param show.out logical whether to show stdout produced during evaluation
# @param show.fail FALSE, or a testorItemsTestsErrors-class object if you want
#   to show the details of failure
# @param items.new the new testor_items
# @param items.ref the reference items
# @return a testor_items list

browse_testor_items <- function(
  title, prompt, detail, actions, show.out=FALSE, show.msg=FALSE, 
  show.fail=FALSE, items.new=NULL, items.ref=NULL
) {
  if(!is.null(items.ref) & !is.null(items.new) & (!identical(length(items.ref), length(items.new)))) {
    stop("Ref list must have the same number of items as new list, or be NULL")
  } else if (
    !is.character(actions) | !all(actions %in% c("A", "B", "C")) | length(actions) != length(unique(actions)) | 
    is.null(names(actions)) | !all(names(actions) %in% c("Y", "N"))
  ) {
    stop("`actions` input incorrect")
  } else if (!is(items.new, "testorItemsOrNULL")) {
    stop("`items.new` must be \"testorItems\" or NULL")
  } else if (!is(items.ref, "testorItemsOrNULL")) {
    stop("`items.ref` must be \"testor_items\" or NULL")
  } else if (!is.logical(show.out) || length(show.out) != 1L) {
    stop("Argument `show.out` must be a 1 length logical")
  } else if (!is.logical(show.msg) || length(show.msg) != 1L) {
    stop("Argument `show.msg` must be a 1 length logical")
  } else if (
    !is(show.fail, "testorItemsTestsErrors") && 
    !(is.logical(show.fail) && identical(length(show.fail), 1L))
  ) {
    stop("Argument `show.fail` must be a 1 length logical or a \"testorItemsTestsErrors\" object")
  } else if (!is.character(prompt) || length(prompt) != 1L) {
    stop("Argument `prompt` must be a 1 length character")
  } else if (!is.character(detail) || length(detail) != 1L) {
    stop("Argument `prompt` must be a 1 length character")
  }
  # set up local history

  hist.file <- tempfile()
  hist.con <- file(hist.file, "at")
  cat("## <testor> (original history will be restored on exit)\n", file=hist.con)
  loadhistory(showConnections()[as.character(hist.con), "description"])
  on.exit({close(hist.con); file.remove(hist.file);})

  # Browsing environments; note that the only time there ever is
  # a secondary environment is when both new and ref items are defined
  # and in that case, the secondary environment is always the reference
  # environment

  browse.eval.env <- new.env(parent=globalenv())
  if(!is.null(items.new)) {
    items.main <- items.new
  } else if (!is.null(items.ref)) {
    items.main <- items.ref
  } else {
    stop("Logic Error; contact package maintainer.")
  }
  base.env.pri <- parent.env(items.main@base.env)
  # Which items are we going to cycle through

  item.list <- new("testorItems", base.env=items.main@base.env)
  if(length(items.main) < 1L) return(item.list)
  
  # Start cycle

  screen_out <- function(txt, max.len=getOption("testor.test.out.lines"), file=stdout()) {
    if(!is.numeric(max.len) || !length(max.len) == 2 || max.len[[1]] < max.len[[2]])
      stop("Argument `max.len` must be a two length numeric vector with first value greater than second")
    if(out.len <- length(txt)) {
      cat(txt[1L:min(out.len, if(out.len > max.len[[1]]) max.len[[2]] else Inf)], sep="\n", file=file)
      if(out.len > max.len[[1]]) {
        cat("... truncated", out.len - max.len[[2]], "lines, review object directly if you wish to see all output\n", file=stderr())
  } } }
  new.opts <- append(valid.opts.def, c(U="[U]ndo"), after=2L)
  items.len <- length(items.main)
  if(!(all.ignored <- all(ignored(items.main)))) {
    print(H3(title))
    cat(
      detail, " For each item, choose whether to ", tolower(prompt), 
      " (", paste0(new.opts, collapse=", "), "):\n\n", sep=""
  ) }
  action <- NULL
  item_select <- function(action, idx) {   # Translate Y/N to an action
    switch(actions[[action]],                  
      A=items.new[idx],
      B=items.ref[idx],
      C=NULL
  ) }
  i <- 1L

  while(i <= items.len) {
    if(!all.ignored) {
      if(length(items.main[[i]]@comment)) cat(items.main[[i]]@comment, sep="\n")
      cat(deparse_prompt(items.main[[i]]@call), sep="\n")
      if(is(show.fail, "testorItemsTestsErrors") && !items.main[[i]]@ignore) {
        cat(as.character(show.fail[[i]]), sep="\n")
      } 
      if(show.msg) screen_out(items.main[[i]]@data@message, max.len=getOption("testor.test.msg.lines"), stderr())
      if(show.out) screen_out(items.main[[i]]@data@output)      
    }
    # Default to "Y" action for ignored items; this means new items and 
    # failed/error tests will get replaced with new value, whereas reference
    # items will always be omitted.  The latter might seem strange, but
    # at this point we're prioritizing being able to recreate tests that
    # come from the most current files, so reference tests just have to
    # make do with a substandard environment

    if(items.main[[i]]@ignore) {  
      item.list <- item.list + item_select("Y", i)
      i <- i + 1L
      next
    }
    # Create evaluation environment; these are really two nested environments,
    # with the parent environment containing the testorItem values and the child 
    # environment containing the actual testor items.  This is so that when
    # user evaluates `.new` or `.ref` they see the value, but then we can 
    # easily retrieve teh full object with the `get*` functions.
    
    var.list <- list()        
    var.sub.list <- list()
    if(!is.null(items.new)) {
      var.list <- c(var.list, list(.new=items.new[[i]]))
      var.sub.list <- c(var.sub.list, list(.new=items.new[[i]]@data@value))
    }
    if(!is.null(items.ref)) {
      var.list <- c(var.list, list(.ref=items.ref[[i]])) 
      var.sub.list <- c(var.sub.list, list(.ref=items.ref[[i]]@data@value))
    }
    browse.par.env <- list2env(var.list, parent=items.main[[i]]@env)
    browse.env <- list2env(var.sub.list, parent=browse.par.env)
    parent.env(browse.eval.env) <- browse.env

    env.sec <- if(!is.null(items.new) && !is.null(items.ref)) items.ref[[i]]@env else NULL
    assign("ls", testor_ls, base.env.pri)
    if(!is.null(env.sec)) {
      assign("ref", function(x) eval(substitute(x), env.sec), base.env.pri)
    } else {      
      assign("ref", function(x) message("`ref` is only active when there is an active secondary environment"), base.env.pri)
    }
    get.msg <- character()
    if(!is.null(items.new)) get.msg <- "`getTest(.new)`"
    if(!is.null(items.ref)) get.msg <- c(get.msg, "`getTest(.ref)`")
    help <- paste0(
      "Type ls() to see what objects are available to inspect, and use ",
      paste0(get.msg, collapse=" or "),
      " to see more details about the test (see documentation for `getTest` for details ",
      "on other accessor functions such as ", 
      paste0(paste0("`", names(getItemFuns), "`"), collapse=", "), ")."
    )
    prompt.val <- testor_prompt(prompt, browse.eval.env, help, new.opts, hist.con)
    if(identical(prompt.val, "U")) {
      if(i <= min(which(!ignored(items.main)))) {
        message("Nothing left to undo in \"", title,"\"")
        next
      } else {
        if(length(item.list)) item.list[[length(item.list)]] <- NULL
        i <- i - 1L
        next
    } }
    item <- item_select(prompt.val, i)
    item.list <- item.list + item
    i <- i + 1L
    if(i > items.len) {
      cat(
        "You have completed \"", title, "\", do you wish to continue (", 
        paste0(new.opts[-2L], collapse=", "), ")?", sep=""
      )
      prompt.val <- testor_prompt(
        "Continue", browse.eval.env, 
        "[Y]es to continue, [U]ndo to undo last decision, [Q]uit to exit and discard all changes.", 
        new.opts[-2L]
      )
      if(identical(prompt.val, "U")) {
        if(length(item.list)) item.list[[length(item.list)]] <- NULL
        i <- i - 1L
        next
  } } } 
  return(item.list)
}
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