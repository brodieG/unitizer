#' @include unitizer.R
#' @include misc.R
#' @include browse.struct.R
#' @include prompt.R

setGeneric("browse", function(x, ...) standardGeneric("browse"))

#' Browse unitizer
#' 
#' Here we are reviewing all the tests in the unitizer under three different lenses
#' \enumerate{
#'   \item tests that don't match the stored reference tests
#'   \item tests that don't exist in the reference tests
#'   \item tests that exist in the reference tests but no the new file
#' }
#' Because a lot of the logic for browsing these three types of situations is
#' shared, that logic has been split off into \code{`\link{reviewNext,unitizerBrowse-method}`}.
#' The key is that that function will return the items that are supposed to be
#' stored in the unitizer.  These items will either be new or reference ones
#' based on user decisions.
#' 
#' Unfortunately, in order to be able to use the same logic for tasks that are
#' not quite the same, a bit of contortion was needed.  In particular, the
#' user is always asked to input either Y, N, or Q, but the corresponding output
#' from \code{`\link{reviewNext,unitizerBrowse-method}`} is very different 
#' depending on what situation we're dealing with.
#' 
#' One important point is that by default the user input is defined as N.  In
#' all cases N means no change to the store, though again the interpretation is
#' different depending on the situation.  For example, if we add a test to the
#' test script, N means don't add it to the store.  If we remove a test, N means
#' keep it in the store.
#' 
#' @keywords internal
#' @param x the object to browse
#' @param prompt.on.quit whether to prompt for review even if there are no changes
#' @param env the environment to use as a parent to the environment to browse the tests

setMethod("browse", c("unitizer"), valueClass="unitizer",
  function(x, prompt.on.quit, ...) {

    # set up local history

    savehistory()    
    hist.file <- tempfile()
    hist.con <- file(hist.file, "at")
    cat("## <unitizer> (original history will be restored on exit)\n", file=hist.con)
    loadhistory(showConnections()[as.character(hist.con), "description"])
    
    # Revert history and trace on exit

    #curr.trace <- .Traceback
    on.exit( {
      close(hist.con); 
      file.remove(hist.file); 
      loadhistory();
      #assign(".Traceback", curr.trace, envir=getNamespace("base"))
    } )

    # Browse through tests that require user input, repeat so we give the user
    # an opportunity to adjust decisions before committing

    unitizer.browse <- browsePrep(x)      # Group tests by section and outcome for review
    unitizer.browse@hist.con <- hist.con  # User expression to this file for use in history

    if(!length(unitizer.browse)) {
      message("All tests passed; nothing to store.")
      return(TRUE)
    } else if(length(unitizer.browse)) {
      repeat {
        
        user.quit <- FALSE
        withRestarts(
          {
           # Interactively review all tests

            if(!done(unitizer.browse)) {
              unitizer.browse <- reviewNext(unitizer.browse)
              next
          } },
          earlyExit=function() user.quit <<- TRUE
        )
        # Nothing happened at all, so quit without even option for prompting
        
        if(
          !(
            something.happened <- length(unitizer.browse@mapping@item.id) && 
              any(!unitizer.browse@mapping@ignored)
          )
        ) {
          message("All tests passed.")
          invokeRestart("noSaveExit")
        }
        # Get summary of changes

        keep <- !unitizer.browse@mapping@ignored
        changes <- split(
          unitizer.browse@mapping@review.val[keep], 
          unitizer.browse@mapping@review.type[keep]
        )
        change.sum <- lapply(changes, function(x) c(sum(x == "Y"), length(x)))
        for(i in names(change.sum)) slot(x@changes, tolower(i)) <- change.sum[[i]]

        if(
          (length(x@changes) > 0L || something.happened) && 
          (prompt.on.quit || !user.quit)
        )
          print(H2("Finalize Unitizer"))

        if(length(x@changes) == 0L) {
          message(
            "You didn't accept any changes so there are no items to store."
          )
          if(!prompt.on.quit && user.quit) {  # on quick unitizer runs just allow quitting without prompt if no changes
            invokeRestart("noSaveExit")
          }
          valid.opts <- c(Y="[Y]es", B="[B]ack", R="[R]eview")
          nav.msg <- "Exit unitizer"
          nav.hlp <- paste0(
            "Pressing Y or Q will exit without saving the unitizer since ",
            "you didn't make any changes.  Pressing B or R will allow you to ",
            "review any of the decisions you made previously, provided you ",
            "actually had any to make."
          )
        } else {
          message("You are about to IRREVERSIBLY:")
          show(x@changes)
          valid.opts <- c(Y="[Y]es", N="[N]o", B="[B]ack", R="[R]eview")
          nav.msg <- "Update unitizer"
          nav.hlp <- paste0(
            "Pressing Y will replace the previous unitizer with a new one updated ",
            "with all the changes you approved, pressing R or B will allow you to ",
            "re-review your choices.  Pressing N or Q both quit without saving ",
            "changes to the unitizer"
          )
        }
        cat(nav.msg, " (", paste0(valid.opts, collapse=", "), ")?", sep="")
        user.input <- navigate_prompt(
          unitizer.browse, curr.id=max(unitizer.browse@mapping@item.id), 
          text=nav.msg, browse.env1=x@zero.env, help=nav.hlp,
          valid.opts=valid.opts
        )
        if(is(user.input, "unitizerBrowse")) {
          unitizer.browse <- user.input
          next
        } else if (identical(user.input, "Q") || identical(user.input, "N")) {
          invokeRestart("noSaveExit")
        } else if (identical(user.input, "Y")) {
          if(identical(nav.msg, "Exit unitizer")) {  # We don't actually want to over-write unitizer store in this case
            invokeRestart("noSaveExit")
          } else break
        }
        stop("Logic Error; unexpected user input, contact maintainer.")
    } }
    # Create the new unitizer

    items.user <- processInput(unitizer.browse)

    # By definition, "Pass" items were not reviewed so cannot be part of
    # `items.user`

    items.ref <- x@items.new[x@tests.status == "Pass"] + items.user
    items.ref <- healEnvs(items.ref, x) # repair the environment ancestry

    zero.env <- new.env(parent=parent.env(x@zero.env))
    unitizer <- new("unitizer", id=x@id, changes=x@changes, zero.env=zero.env)
    unitizer + items.ref
} )
setGeneric("reviewNext", function(x, ...) standardGeneric("reviewNext"))

#' Bring up Review of Next test
#' 
#' Generally we will go from one test to the next, where the next test is 
#' determined by the value of \code{`x@@last.id`}.  This means it is possible
#' to affect the browsing order by modifying \code{`x@@last.id`}.
#' 
#' This method is in charge of displaying all the output for review.
#' 
#' @keywords internal

setMethod("reviewNext", c("unitizerBrowse"), 
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
    x@last.reviewed <- curr.id

    curr.sec <- x@mapping@sec.id[[which(x@mapping@item.id == curr.id)]]
    curr.sub.sec <- x@mapping@sub.sec.id[[which(x@mapping@item.id == curr.id)]]
    cur.sub.sec.items <- x@mapping@sub.sec.id == curr.sub.sec
    curr.sub.sec.obj <- x[[curr.sec]][[curr.sub.sec]]
    id.rel <- x@mapping@item.id.rel[[which(x@mapping@item.id == curr.id)]]

    # Display Section Headers as Necessary

    valid.opts <- c(Y="[Y]es", N="[N]o", B="[B]ack", R="[R]eview")
    
    # Print Section title if appropriate, basically if not all the items are 
    # ignored, or alternatively if one of the ignored items produced new 
    # conditions

    if(
      !identical(last.reviewed.sec, curr.sec) && 
      !all(
        x@mapping@ignored[x@mapping@sec.id == curr.sec] & 
        !x@mapping@new.conditions[x@mapping@sec.id == curr.sec]
      ) &&
      length(unique(x@mapping@sec.id[!(x@mapping@ignored & !x@mapping@new.conditions)])) > 1L
    ) {
      print(H2(x[[curr.sec]]@section.title))  
    }
    if(        # Print sub-section title if appropriate
      (
        !identical(last.reviewed.sub.sec, curr.sub.sec) || 
        !identical(last.reviewed.sec, curr.sec)
      ) && !all(
        x@mapping@ignored[cur.sub.sec.items] & 
        !x@mapping@new.conditions[cur.sub.sec.items]
      )
    ) {
      print(H3(curr.sub.sec.obj@title))
      cat(
        curr.sub.sec.obj@detail, " ", curr.sub.sec.obj@prompt, " ", 
        "(", paste0(c(valid.opts, Q="[Q]uit", H="[H]elp"), collapse=", "),
        ")?\n\n", sep=""
      )
    }
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
    # Show test to screen, but only if the entire section is not ignored

    if(
      !all(
        x@mapping@ignored[cur.sub.sec.items] & 
        !x@mapping@new.conditions[cur.sub.sec.items]
      )
    ) {
      if(x@mapping@reviewed[[curr.id]]) {
        message(
          "You are re-reviewing a test; previous selection was: \"", 
          x@mapping@review.val[[curr.id]], "\""
      ) }
      if(length(item.main@comment)) cat(item.main@comment, sep="\n")
      cat(deparse_prompt(item.main@call), sep="\n")
      # If there are conditions that showed up in main that are not in reference
      # show the message, and set the trace if relevant

      if(
        !is.null(item.new) && !is.null(item.ref) && 
        x@mapping@new.conditions[[curr.id]] || curr.sub.sec.obj@show.msg
      ) {
        screen_out(
          item.main@data@message, 
          max.len=getOption("unitizer.test.msg.lines"), stderr()
        )
        set_trace(item.main@trace)
      }
      if(curr.sub.sec.obj@show.out) screen_out(item.main@data@output)    

      # If test failed, show details of failure

      if(
        is(curr.sub.sec.obj@show.fail, "unitizerItemsTestsErrors") && 
        !item.main@ignore
      ) {
        cat(
          c(
            "Test Failed Because:", 
            as.character(curr.sub.sec.obj@show.fail[[id.rel]])
          ), 
          sep="\n"
    ) } }
    # Need to add ignored tests as default action is N. Not clear if we also
    # need to set reviewed to TRUE (seems like we should not do so)

    if(x@mapping@ignored[[curr.id]]) {
      #x@mapping@reviewed[[curr.id]] <- TRUE
      x@mapping@review.val[[curr.id]] <- "Y"
      x@last.id <- curr.id      
      return(x)
    }
    # Create evaluation environment; these are really two nested environments,
    # with the parent environment containing the unitizerItem values and the child 
    # environment containing the actual unitizer items.  This is so that when
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

    # Functions to override
    
    env.sec <- if(!is.null(item.new) && !is.null(item.ref)) item.ref@env else NULL
    assign("ls", unitizer_ls, base.env.pri)
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
    if(!is.null(item.new)) get.msg <- "`(.new)`"
    if(!is.null(item.ref)) get.msg <- c(get.msg, "`getTest(.ref)`")
    
    # Options to navigate; when navigating the name of the game is set `@last.id`
    # to the non-ignored test just previous to the one you want to navigate to,
    # the loop will then advance you to that test

    help.prompt <- paste(
      "In addition to any valid R expression, you may type the following",
      "at the prompt (without backticks):"
    )
    help.opts <- c(
      "`B` to go Back to the previous test",
      "`R` to see a listing of all previously reviewed tests",
      "`ls()` to see what objects are available to inspect",
      paste0(collapse="",
        paste0(get.msg, collapse=" or "),
        "to see more details about the test (see documentation for `getTest` ",
        "for details on other accessor functions such as (",
        paste0(paste0("`", names(getItemFuns), "`"), collapse=", "), ")."
    ) )
    # navigate_prompt handles the B and R cases internally and modifies the
    # unitizerBrowse to be at the appropriate location; this is done as a function
    # because same logic is re-used elsewhere

    if(
      is(
        x.mod <- navigate_prompt(
          x=x, curr.id=curr.id, text=curr.sub.sec.obj@prompt, 
          browse.env1=browse.eval.env, browse.env2=new.env(parent=parent.env(base.env.pri)),
          valid.opts=valid.opts, help=c(help.prompt, as.character(UL(help.opts)))
        ),
        "unitizerBrowse"
      )
    ) {
      return(x.mod)  
    } else if (x.mod %in% c("Y", "N")) { # Actual user input
      x@mapping@reviewed[[curr.id]] <- TRUE
      x@mapping@review.val[[curr.id]] <- x.mod
      x@last.id <- curr.id
    } else if (identical(x.mod, "Q")) {
      invokeRestart("earlyExit")
    } else {
      stop("Logic Error: `unitizer_prompt` returned unexpected value; contact maintainer")
    }
    x
  }
)
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
  if(!(name %in% c(".new", ".ref"))) {
    stop("unitizer::get* functions may only be called on the test objects (`.new`, or `.ref`).")
  }
  if(!(is.environment(env))) stop("Argument `env` must be an environment")
  if(inherits(obj <- try(get(name, inherits=FALSE, envir=env), silent=TRUE), "try-error")) {
    stop("Requested test object `", name, "` is not defined for this test.")
  }
  if(!is(obj, "unitizerItem")) stop("Logic Error: retrieved object is not a `unitizerItem`; contact package maintainer.")
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
#' Intended for use exclusively within the \code{`unitizer`} interactive command 
#' line.  For example \code{getMsg(.new)} will retrieve any \file{stderr} that occurred
#' during test evaluation (for reference tests, use \code{getMsg(.ref)}.
#' 
#' @note these functions are only available at the \code{`unitizer`} prompt
#' 
#' @name getTest
#' @usage getTest(x)
#' @aliases getVal getConds getMsg getOut getAborted
#' @param x object to get additional data for (should be one of \code{`.new`}, \code{`.ref`})
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
#'     that typing \code{`obj`} and \code{getVal(`obj`)} at the \code{`unitizer`}
#'     prompt are equivalent
#'   \item \code{`reCall`}: will load the call used to generate the test
#'     on the prompt (not implemented yet).
#' }

getTest <- function(x) NULL # This will be overwritten 
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