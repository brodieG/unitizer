#' @include unitizer.R
#' @include conditions.R
#' @include browse.struct.R
#' @include prompt.R

setGeneric(
  "browseUnitizer", function(x, y, ...) standardGeneric("browseUnitizer")
)

#' Browse unitizer
#'
#' Here we are reviewing all the tests in the unitizer under three different
#' lenses
#' \enumerate{
#'   \item tests that don't match the stored reference tests
#'   \item tests that don't exist in the reference tests
#'   \item tests that exist in the reference tests but no the new file
#'   \item tests that passed (these are omitted )
#' }
#' Because a lot of the logic for browsing these three types of situations is
#' shared, that logic has been split off into
#' \code{\link{reviewNext,unitizerBrowse-method}}. The key is that that function
#' will return the items that are supposed to be stored in the unitizer.  These
#' items will either be new or reference ones based on user decisions.
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
#' @param y the derivative unitizerBrowse object of x; this needs to be passed
#'   in as an argument because the logic for generating it is different
#'   depending on whether we are using `unitize` or `review`.
#' @param prompt.on.quit whether to prompt for review even if there are no
#'   changes
#' @return a unitizer if the unitizer was modified, FALSE otherwise

setMethod("browseUnitizer", c("unitizer", "unitizerBrowse"),
  function(x, y, force.update, ...) {
    browse.res <- withRestarts(
      browseUnitizerInternal(x, y, force.update=force.update),
      unitizerQuitExit=unitizer_quit_handler
    )
    # Reset the parent env of zero env so we don't get all sorts of warnings
    # related to trying to store a package environment when we save this
    # unitizer

    if(is(unitizer, "unitizer")) {
      parent.env(unitizer@zero.env) <- baseenv()
    }
    unitizer
  }
)
setGeneric(
  "browseUnitizerInternal",
  function(x, y, ...) standardGeneric("browseUnitizerInternal")
)
setMethod(
  "browseUnitizerInternal", c("unitizer", "unitizerBrowse"),
  valueClass="unitizerBrowseResult"
  function(x, y, force.update, ...) {
    # Browse through tests that require user input, repeat so we give the user
    # an opportunity to adjust decisions before committing

    update <- FALSE
    prompt.on.quit <-
      x@eval.time > getOption("unitizer.prompt.b4.quit.time", 10)

    something.happened <- any(
      y@mapping@review.type != "Passed" & !y@mapping@ignored
    ) || (
      any(!y@mapping@ignored) && identical(y@mode, "review")  # Not sure this is
    )

    if(!length(y)) {
      message("No tests to review.")
    } else if(!something.happened && !force.update) {
      message("All tests passed, `unitizer` store unchanged.")
    } else if(!something.happened && force.update) {
      message(
        "No changes to `untizer`, but re-saving anyway as we are in ",
        "\"force.update\" mode"
      )
      update <- TRUE
    } else {
      # set up local history

      savehistory()
      hist.file <- tempfile()
      hist.con <- file(hist.file, "at")
      cat(
        "## <unitizer> (original history will be restored on exit)\n",
        file=hist.con
      )
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
        re.eval <- 0L

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
                      "Logic Error: review should return `unitizerBrowse`; "
                      "contact maintainer."
                    )
                  } else y <- y.tmp
                }
                next
              } else {
                # wtf? intended to be NULL??
              }
            },
            earlyExit=function(mode="quit") {
              if(
                !is.character(mode) ||
                !isTRUE(mode %in% c("quit", "reeval", "reevalall"))
              )
                stop(
                  "Logic Error: unexpected early exit restart value; contact ",
                  "maintainer"
                )
              user.quit <<- TRUE
              if(identical(mode, "reeval")) {
                re.eval <<- 1L
              } else if (identical(mode, "reevalall")) {
                re.eval <<- 2L
        } } ) }
        # Get summary of changes

        keep <- !y@mapping@ignored
        changes <- split(
          y@mapping@review.val[keep] == "Y", y@mapping@review.type[keep]
        )
        change.sum <- lapply(
          changes,
          function(x) c(sum(x), length(x))
        )
        for(i in names(change.sum))
          slot(x@changes, tolower(i)) <- change.sum[[i]]

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

        if(length(x@changes) == 0L && !force.update && !re.eval) {
          message(
            "You didn't accept any changes so there are no items to store."
          )
          # on quick unitizer runs just allow quitting without prompt if no changes

          if(!prompt.on.quit && user.quit) {
            message("unitizer store unchanged")
            break
          }
          valid.opts <- c(Y="[Y]es", P="[P]revious"Ï, B="[B]rowse")
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
            if(!force.update)
              stop(
                "Logic Error: should be in forced update mode; contact ",
                "maintainer."
              )
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
          valid.opts <- c(Y="[Y]es", N="[N]o", P="[P]revious", B="[B]rowse")
          nav.msg <- "Update unitizer"
          nav.hlp <- paste0(
            "Pressing Y will replace the previous unitizer with a new one, ",
            update.w.changes, "pressing R or B will allow you to ",
            "re-review your choices.  Pressing N or Q both quit without saving ",
            "changes to the unitizer"
          )
        }
        word_cat(nav.msg, paste0("(", paste0(valid.opts, collapse=", "), ")?"))
        if(!y@human && !user.quit) {  # quitting user doesn't allow us to register humanity...
          if(y@navigating)
            stop(
              "Logic Error: should only get here in `auto.accept` mode, ",
              "contact maintainer"
            )
          message("Auto-accepting changes...")
          update <- TRUE
          break
        } else {
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
            if(re.eval) message("Re-evaluation disabled")
            re.eval <- 0L
            break
          } else if (identical(user.input, "Y")) {
            # We don't actually want to over-write unitizer store in this case

            if(identical(nav.msg, "Exit unitizer")) {
              message("unitizer store unchanged")
            } else {
              udpate <- TRUE
            }
            break
          }
          stop("Logic Error; unexpected user input, contact maintainer.")
        }
    } }
    # Create the new unitizer

    unitizer <- if(update) {
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
      refSections(unitizer, x)
    } else {
      x
    }
    # Return structure

    new(
      "unitizerBrowseResult", unitizer=unitizer, re.eval=re.eval, updated=update
    )
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
      last.reviewed.sec <-
        x@mapping@sec.id[[which(x@mapping@item.id == x@last.reviewed)]]
      last.reviewed.sub.sec <-
        x@mapping@sub.sec.id[[which(x@mapping@item.id == x@last.reviewed)]]
      furthest.reviewed <- if(length(which(x@mapping@reviewed)))
        max(which(x@mapping@reviewed)) else 0L
    } else {
      last.reviewed.sec <- last.reviewed.sub.sec <- furthest.reviewed <- 0L
    }
    x@last.reviewed <- curr.id

    curr.sec <- x@mapping@sec.id[[which(x@mapping@item.id == curr.id)]]
    curr.sub.sec <- x@mapping@sub.sec.id[[which(x@mapping@item.id == curr.id)]]
    cur.sub.sec.items <-
      x@mapping@sub.sec.id == curr.sub.sec & x@mapping@sec.id == curr.sec
    curr.sub.sec.obj <- x[[curr.sec]][[curr.sub.sec]]
    id.rel <- x@mapping@item.id.rel[[which(x@mapping@item.id == curr.id)]]

    # Display Section Headers as Necessary

    valid.opts <- c(
      Y="[Y]es", N="[N]o", P="[P]revious", B="[B]rowse", YY="", YYY="", YYYY="",
      NN="", NNN="", NNNNN="", R="[R]e-eval", RR=""
    )
    # Pre compute whether sections are effectively ignored or not; these will
    # control whether stuff gets shown to screen or not

    ignore.passed <- !identical(x@mode, "review") &&
      is(curr.sub.sec.obj, "unitizerBrowseSubSectionPassed") &&
      !x@inspect.all

    ignore.sec <- all(
      (       # ignored and no errors
        x@mapping@ignored[x@mapping@sec.id == curr.sec] &
        !x@mapping@new.conditions[x@mapping@sec.id == curr.sec]
      ) | (   # passed and not in review mode
        x@mapping@review.type[x@mapping@sec.id == curr.sec] == "Passed" &
        !identical(x@mode, "review")
      ) | (   # auto.accept
        x@mapping@reviewed[x@mapping@sec.id == curr.sec] &
        !x@navigating
      )
    ) && !x@inspect.all

    ignore.sub.sec <- (
      all(
        (
          x@mapping@ignored[cur.sub.sec.items] &
          !x@mapping@new.conditions[cur.sub.sec.items]
        ) | (
          x@mapping@reviewed[cur.sub.sec.items] & !x@navigating
        )
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
            "(",
            paste0(
              c(valid.opts[nchar(valid.opts) > 0], Q="[Q]uit", H="[H]elp"),
              collapse=", "
            ),
            ")?\n\n"
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
        if(nchar(item.main@data@message))
          screen_out(
            item.main@data@message,
            max.len=getOption("unitizer.test.msg.lines"), stderr()
          )
        if(length(item.main@trace)) set_trace(item.main@trace)
      }
      if(curr.sub.sec.obj@show.out && nchar(item.main@data@output))
        screen_out(item.main@data@output)

      # If test failed, show details of failure; note this should mean there must
      # be a `.new` and a `.ref`

      if(
        is(curr.sub.sec.obj@show.fail, "unitizerItemsTestsErrors") &&
        !item.main@ignore
      ) {
        summary(curr.sub.sec.obj@show.fail[[id.rel]])
        eval(  # must eval to make sure that correct methods are available when outputing failures to screen
          call("show", curr.sub.sec.obj@show.fail[[id.rel]]),
          if(is.environment(item.main@env)) item.main@env else base.env.pri
        )
    } }
    # Need to add ignored tests as default action is N, though note that ignored
    # tests are treated specially in `healEnvs` and are either included or removed
    # based on what happens to the subsequent non-ignored test.

    if(!x@inspect.all) {
      if(
        x@mapping@ignored[[curr.id]] || ignore.passed ||
        (x@mapping@reviewed[[curr.id]] && !x@navigating)  # reviewed items are skipped unless we're actively navigating to support `auto.accept`
      ) {
        x@last.id <- curr.id
        return(x)
      }
    }
    # If we get past this point, then we will need some sort of human input, so
    # we mark the browse object

    x@human <- TRUE

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
      "`P` to go to the previous test",
      "`B` to see a listing of all tests",
      "`ls()` to see what objects are available to inspect",
      if(!is.null(item.new))
        "`.new` for the current value, or `.NEW` for the full test object",
      if(!is.null(item.ref))
        "`.ref` for the reference value, or `.REF` for the full reference object",
      "`YY` or `NN` to apply same choice to all remaining unreviewed items in sub-section",
      "`YYY` or `NNN` to apply same choice to all remaining unreviewed items in section",
      "`YYYY` or `NNNN` to apply same choice to all remaining unreviewed items in unitizer",
      "`R` to re-evalute the `unitizer`; used typically after you re-`install` the package you are testing via the `unitizer` prompt",
      "`RR` to re-evaluate all loaded `unitizers` (relevant for `unitize_dir`)"
    )
    # navigate_prompt handles the P and B cases internally and modifies the
    # unitizerBrowse to be at the appropriate location; this is done as a function
    # because same logic is re-used elsewhere

    if(
      is(
        x.mod <- navigate_prompt(
          x=x, curr.id=curr.id, text=curr.sub.sec.obj@prompt,
          browse.env1=browse.eval.env,
          browse.env2=new.env(parent=parent.env(base.env.pri)),
          valid.opts=valid.opts,
          help=c(
            help.prompt, paste0(as.character(UL(help.opts)), collapse="\n")
        ) ),
        "unitizerBrowse"
      )
    ) {
      return(x.mod)
    } else if (isTRUE(grepl("^RR?$", x.mod))) {           # Re-eval
      if(identical(nchar(x.mod), 1L)) {
        message("This `unitizer` will be re-evaluated on exit")
        invokeRestart("earlyExit", "reeval")
      } else if(identical(nchar(x.mod), 1L)) {
        message("All loaded `unitizer`s will be re-evaluated on exit")
        invokeRestart("earlyExit", "reevalall")
      }
      stop("Logic Error: unknown re-eval mode; contact maintainer.")

    } else if (isTRUE(grepl("^(Y|N)\\1{0,3}$", x.mod))) { # Yes No handling
      act <- substr(x.mod, 1L, 1L)
      act.times <- nchar(x.mod)
      rev.ind <- if(act.times == 1L) {
        curr.id
      } else {
        rev.ind.tmp <- if (act.times == 2L) {
          cur.sub.sec.items                # all items in sub section
        } else if (act.times == 3L) {
          x@mapping@sec.id == curr.sec     # all items in sub-section
        } else if (act.times == 4L) {
          TRUE                             # all items
        } else
          stop("Logic Error: unexpected number of Y/N; contact maintainer.")

        # exclude already reviewed items as well as ignored items as well as
        # passed items (unless in review mode for last one)

        indices <- which(
          rev.ind.tmp & !x@mapping@reviewed & !x@mapping@ignored &
          (x@mapping@review.type != "Passed" & !identical(x@mode, "review"))
        )
        if(length(indices)) {
          show(x[indices])
          help.mx <- rbind(
            c("Add New", "Keep New", "Drop Ref", "Drop New", "Keep New"),
            c("Drop New", "Keep Ref", "Keep Ref", "Keep New", "Keep Ref")
          )
          rownames(help.mx) <- c("[Y]es", "[N]o")
          colnames(help.mx) <- c(
            "*New*", "*Failed*", "*Removed*", "*Passed*", "*Corrupted*"
          )
          help.txt <- capture.output(print(as.data.frame(help.mx), quote=TRUE))
          help <- paste0(
            paste0(
              "The effect of 'Y' or 'N' depends on what type of test you ",
              "are reviewing.  Consult the following table for details:\n\n"
            ),
            paste0(help.txt, collapse="\n")
          )
          prompt <- paste0(
            "Choose '", act, "' for the ", length(indices),
            " test", if(length(indices) > 1L) "s", " shown above"
          )
          cat(prompt, " ([Y]es, [N]o)?\n", sep="")
          act.conf <- unitizer_prompt(
            prompt, new.env(parent=parent.env(base.env.pri)), help,
            valid.opts=c(Y="[Y]es", N="[N]o")
          )
          if(identical(act.conf, "Q")) invokeRestart("earlyExit")
          if(identical(act.conf, "N")) return(x)
        }
        indices
      }
      if(!any(rev.ind)) stop("Logic Error: no tests to accept/reject")

      x@mapping@reviewed[rev.ind] <- TRUE
      x@mapping@review.val[rev.ind] <- act
      x@last.id <- max(rev.ind)
    } else if (identical(x.mod, "Q")) {
      invokeRestart("earlyExit")
    } else {
      stop(
        "Logic Error: `unitizer_prompt` returned unexpected value; ",
        "contact maintainer"
      )
    }
    x
  }
)
