#' @include unitizer.R
#' @include conditions.R
#' @include browse.struct.R
#' @include prompt.R

setGeneric("browseUnitizer", function(x, y, ...) standardGeneric("browseUnitizer"))

#' Browse unitizer
#'
#' Here we are reviewing all the tests in the unitizer under three different lenses
#' \enumerate{
#'   \item tests that don't match the stored reference tests
#'   \item tests that don't exist in the reference tests
#'   \item tests that exist in the reference tests but no the new file
#'   \item tests that passed (these are omitted )
#' }
#' Because a lot of the logic for browsing these three types of situations is
#' shared, that logic has been split off into \code{\link{reviewNext,unitizerBrowse-method}}.
#' The key is that that function will return the items that are supposed to be
#' stored in the unitizer.  These items will either be new or reference ones
#' based on user decisions.
#'
#' Unfortunately, in order to be able to use the same logic for tasks that are
#' not quite the same, a bit of contortion was needed.  In particular, the
#' user is always asked to input either Y, N, or Q, but the corresponding output
#' from \code{\link{reviewNext,unitizerBrowse-method}} is very different
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
#' @param y the derivative unitizerBrowse object of x; this needs to be passed in
#'   as an argument because the logic for generating it is different depending on
#'   whether we are using `unitize` or `review`.
#' @param prompt.on.quit whether to prompt for review even if there are no changes
#' @param a unitizer if the unitizer was modified, FALSE otherwise

setMethod("browseUnitizer", c("unitizer", "unitizerBrowse"),
  function(x, y, prompt.on.quit, force.update, ...) {
    unitizer <- withRestarts(
      browseUnitizerInternal(
        x, y, prompt.on.quit=prompt.on.quit, force.update=force.update
      ),
      unitizerQuitExit=unitizer_quit_handler
    )
    # Reset the parent env of zero env so we don't get all sorts of warnings related
    # to trying to store a package environment when we save this unitizer

    if(is(unitizer, "unitizer")) {
      parent.env(unitizer@zero.env) <- baseenv()
    }
    unitizer
  }
)
setGeneric("browseUnitizerInternal", function(x, y, ...) standardGeneric("browseUnitizerInternal"))
setMethod("browseUnitizerInternal", c("unitizer", "unitizerBrowse"), valueClass="unitizer",
  function(x, y, prompt.on.quit, force.update, ...) {
    # Browse through tests that require user input, repeat so we give the user
    # an opportunity to adjust decisions before committing

    if(!length(y)) {
      message("No tests to review.")
      return(TRUE)
    } else if(length(y)) {
      # Nothing happened at all, so quit without even option for prompting

      if(
        !(
          something.happened <- any(
            y@mapping@review.type != "Passed" & !y@mapping@ignored
          ) || (
            any(!y@mapping@ignored) && identical(y@mode, "review")  # Not sure this is
        ) )
      ) {
        message("All tests passed.")
        if(!force.update) {
          message("unitizer store unchanged")
          return(FALSE)
      } }
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
      y@hist.con <- hist.con  # User expression to this file for use in history

      # `repeat` loop allows us to keep going if at the last minute we decide
      # we are not ready to exit the unitizer

      first.time <- TRUE

      repeat {
        user.quit <- FALSE
        if(!user.quit) {

          # Now review each test, special handling required to ensure that the
          # test selection menu shows up as appropriate (i.e. starting off in
          # review mode, or we just reviewed a typically non-reviewed test)

          withRestarts(
            {
              if(!done(y)) {
                if(first.time && identical(y@mode, "review")) { # for passed tests, start by showing the list of tests
                  first.time <- FALSE
                  y@review <- TRUE
                } else {
                  review.prev <- y@review
                  y <- reviewNext(y)
                  if(!review.prev && y@review) next
                }
                if(y@review) {
                  y.tmp <- review_prompt(y, new.env(parent=x@base.env))
                  if(identical(y.tmp, "Q")) {
                    invokeRestart("earlyExit")
                  } else if(!is(y.tmp, "unitizerBrowse")) {
                    stop(
                      "Logic Error: review should return `unitizerBrowse`; contact ",
                      "maintainer."
                    )
                  } else y <- y.tmp
                }
                next
              } else {

              }
            },
            earlyExit=function() user.quit <<- TRUE
          )
        }
        # Get summary of changes

        keep <- !y@mapping@ignored
        changes <- split(
          y@mapping@review.val[keep] == "Y", y@mapping@review.type[keep]
        )
        change.sum <- lapply(
          changes,
          function(x) c(sum(x), length(x))
        )
        for(i in names(change.sum)) slot(x@changes, tolower(i)) <- change.sum[[i]]

        if(
          length(x@changes) > 0L || (
            something.happened && (prompt.on.quit || !user.quit)
          )
        ) {
          print(H2("Finalize Unitizer"))
          # Make sure we did not skip anything we were supposed to review

          if(identical(y@mode, "unitize")) {
            unreviewed <- sum(
              !y@mapping@reviewed & y@mapping@review.type != "Passed" &
              !y@mapping@ignored
            )
            if(unreviewed) {
              message(
                "You have ", unreviewed, " unreviewed tests; press `R` to see ",
                "which tests you have skipped, and then `U` to go to first ",
                "unreviewed."
        ) } } }
        # Prompt for user input if necessary to finalize

        if(length(x@changes) == 0L && !force.update) {
          message(
            "You didn't accept any changes so there are no items to store."
          )
          if(!prompt.on.quit && user.quit) {  # on quick unitizer runs just allow quitting without prompt if no changes
            message("unitizer store unchanged")
            return(FALSE)
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
          if(length(x@changes) > 0) {
            update.w.changes <- " updated with all the changes you approved, "
            show(x@changes)
          } else {
            if(!force.update) stop("Logic Error: should be in forced update mode; contact maintainer.")
            update.w.changes <- character()
            word_cat(
              "replace the existing unitizer with a reloaded version that",
              "contains the same tests.  If you are seeing this message it is",
              "because you chose to run in `force.update` mode.  Note that the",
              "reloaded version of the `unitizer` will not be completely",
              "identical to the currently stored one.  In particular sections",
              "and comments will reflect the latest source file, and test",
              "environments will be re-generated."
            )
          }
          valid.opts <- c(Y="[Y]es", N="[N]o", B="[B]ack", R="[R]eview")
          nav.msg <- "Update unitizer"
          nav.hlp <- paste0(
            "Pressing Y will replace the previous unitizer with a new one, ",
            update.w.changes, "pressing R or B will allow you to ",
            "re-review your choices.  Pressing N or Q both quit without saving ",
            "changes to the unitizer"
          )
        }
        word_cat(nav.msg, paste0("(", paste0(valid.opts, collapse=", "), ")?"))
        user.input <- navigate_prompt(
          y, curr.id=max(y@mapping@item.id) + 1L,
          text=nav.msg, browse.env1=x@zero.env, help=nav.hlp,
          valid.opts=valid.opts
        )
        if(is(user.input, "unitizerBrowse")) {
          y <- user.input
          next
        } else if (identical(user.input, "Q") || identical(user.input, "N")) {
          message("unitizer store unchanged")
          return(FALSE)
        } else if (identical(user.input, "Y")) {
          if(identical(nav.msg, "Exit unitizer")) {  # We don't actually want to over-write unitizer store in this case
            message("unitizer store unchanged")
            return(FALSE)
          } else break
        }
        stop("Logic Error; unexpected user input, contact maintainer.")
    } }
    # Create the new unitizer

    items.ref <- processInput(y)
    items.ref <- healEnvs(items.ref, x) # repair the environment ancestry

    zero.env <- new.env(parent=parent.env(x@zero.env))
    unitizer <- new("unitizer", id=x@id, changes=x@changes, zero.env=zero.env)
    unitizer <- unitizer + items.ref

    # Extract and re-map sections of tests we're saving as reference

    if(identical(y@mode, "review")) {
      # Need to re-use our reference sections so `refSections` works since we
      # will not have created any sections by parsing/evaluating tests.  This
      # is super hacky as we're partly using the stuff related to `items.new`,
      # and could cause problems further down the road if we're not careful

      x@sections <- x@sections.ref
      x@section.map <- x@section.ref.map
    }
    unitizer <- refSections(unitizer, x)

    unitizer
} )
setGeneric("reviewNext", function(x, ...) standardGeneric("reviewNext"))
#' Bring up Review of Next test
#'
#' Generally we will go from one test to the next, where the next test is
#' determined by the value of \code{x@@last.id}.  This means it is possible
#' to affect the browsing order by modifying \code{x@@last.id}.
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
    cur.sub.sec.items <- x@mapping@sub.sec.id == curr.sub.sec & x@mapping@sec.id == curr.sec
    curr.sub.sec.obj <- x[[curr.sec]][[curr.sub.sec]]
    id.rel <- x@mapping@item.id.rel[[which(x@mapping@item.id == curr.id)]]

    # Display Section Headers as Necessary

    valid.opts <- c(Y="[Y]es", N="[N]o", B="[B]ack", R="[R]eview")

    # Pre compute whether sections are effectively ignored or not; these will
    # control whether stuff gets shown to screen or not

    ignore.passed <- !identical(x@mode, "review") &&
      is(curr.sub.sec.obj, "unitizerBrowseSubSectionPassed") &&
      !x@inspect.all
    ignore.sec <- all(
      (
        x@mapping@ignored[x@mapping@sec.id == curr.sec] &
        !x@mapping@new.conditions[x@mapping@sec.id == curr.sec]
      ) | (
        x@mapping@review.type[x@mapping@sec.id == curr.sec] == "Passed" &
        !identical(x@mode, "review")
    ) ) && !x@inspect.all
    ignore.sub.sec <- (
      all(
        x@mapping@ignored[cur.sub.sec.items] &
        !x@mapping@new.conditions[cur.sub.sec.items]
      ) || ignore.passed
    ) && !x@inspect.all
    multi.sect <- length(
      unique(x@mapping@sec.id[!(x@mapping@ignored & !x@mapping@new.conditions)])
    ) > 1L

    # Print Section title if appropriate, basically if not all the items are
    # ignored, or alternatively if one of the ignored items produced new
    # conditions

    if(!identical(last.reviewed.sec, curr.sec) && !ignore.sec && multi.sect) {
      print(H2(x[[curr.sec]]@section.title))
    }
    if(        # Print sub-section title if appropriate
      (
        !identical(last.reviewed.sub.sec, curr.sub.sec) ||
        !identical(last.reviewed.sec, curr.sec)
      ) && !ignore.sub.sec
    ) {
      print(H3(curr.sub.sec.obj@title))
      word_cat(
        curr.sub.sec.obj@detail,
        if(!all(x@mapping@ignored[cur.sub.sec.items]) || x@inspect.all) {
          paste0(
            " ", curr.sub.sec.obj@prompt, " ",
            "(", paste0(c(valid.opts, Q="[Q]uit", H="[H]elp"), collapse=", "),
            ")?\n"
        ) }
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
    # Show test to screen, but only if the entire section is not ignored, and
    # not passed tests and requesting that those not be shown

    if(!ignore.sub.sec) {
      if(x@mapping@reviewed[[curr.id]] && !identical(x@mode, "review")) {
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

      # If test failed, show details of failure; note this should mean there must
      # be a `.new` and a `.ref`

      if(
        is(curr.sub.sec.obj@show.fail, "unitizerItemsTestsErrors") &&
        !item.main@ignore
      ) {
        cat(
          c(
            "Test Failed Because:",
            as.character(curr.sub.sec.obj@show.fail[[id.rel]])
          ),
          sep="\n", file=stderr()
        )
        # Display output, not super elegant as we only doing val diff and cond
        # diff for now, and cond diff assumes message output corresponds to
        # condition, which may or may not be the case, plus it is taken as is
        # instead of formatted as the val output is.

        val.diff <- identical(unname(x@mapping@tests.result[curr.id, "value"]), FALSE)
        cond.diff <- identical(
          unname(x@mapping@tests.result[curr.id, "conditions"]), FALSE
        ) && length(item.new@data@conditions) == 1L &&
          length(item.ref@data@conditions) == 1L

        if(val.diff || cond.diff) cat("@@ .new @@\n")
        if(val.diff) obj_chr_out(obj_capt(item.new@data@value), ".new")
        if(cond.diff)
          obj_chr_out(item.new@data@message, ".new", file=stderr())
        if(val.diff || cond.diff) cat("@@ .ref @@\n")
        if(val.diff) obj_chr_out(obj_capt(item.ref@data@value), ".ref")
        if(cond.diff)
          obj_chr_out(item.ref@data@message, ".ref", file=stderr())
    } }
    # Need to add ignored tests as default action is N, though note that ignored
    # tests are treated specially in `healEnvs` and are either included or removed
    # based on what happens to the subsequent non-ignored test.

    if(!x@inspect.all) {
      if(x@mapping@ignored[[curr.id]] || ignore.passed) {
        x@last.id <- curr.id
        return(x)
      }
    }
    # Create evaluation environment; these are really two nested environments,
    # with the parent environment containing the unitizerItem values and the child
    # environment containing the actual unitizer items.  This is so that when
    # user evaluates `.new` or `.ref` they see the value, but then we can
    # easily retrieve the full object with the `get*` functions.

    var.list <- list()
    if(!is.null(item.new)) {
      var.list <- c(var.list, list(.NEW=item.new, .new=item.new@data@value))
    }
    if(!is.null(item.ref)) {
      var.list <- c(var.list, list(.REF=item.ref, .ref=item.ref@data@value))
    }
    browse.env <- list2env(var.list, parent=item.main@env)
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
    # Options to navigate; when navigating the name of the game is set `@last.id`
    # to the non-ignored test just previous to the one you want to navigate to,
    # the loop will then advance you to that test

    help.prompt <- paste(
        "In addition to any valid R expression, you may type the following",
        "at the prompt (without backticks):\n"
      )
    help.opts <- c(
      "`B` to go Back to the previous test",
      "`R` to see a listing of all previously reviewed tests",
      "`ls()` to see what objects are available to inspect",
      if(!is.null(item.new))
        "`.new` for the current value, or `.NEW` for the full test object",
      if(!is.null(item.ref))
        "`.ref` for the reference value, or `.REF` for the full reference object"
    )
    # navigate_prompt handles the B and R cases internally and modifies the
    # unitizerBrowse to be at the appropriate location; this is done as a function
    # because same logic is re-used elsewhere

    if(
      is(
        x.mod <- navigate_prompt(
          x=x, curr.id=curr.id, text=curr.sub.sec.obj@prompt,
          browse.env1=browse.eval.env, browse.env2=new.env(parent=parent.env(base.env.pri)),
          valid.opts=valid.opts,
          help=c(help.prompt, paste0(as.character(UL(help.opts)), collapse="\n"))
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
