#' @include unitizer.R
#' @include conditions.R
#' @include browse.struct.R
#' @include prompt.R

setGeneric(
  "browseUnitizer", function(x, y, ...) standardGeneric("browseUnitizer")
)

## Browse unitizer
##
## Here we are reviewing all the tests in the unitizer under three different
## lenses
## \enumerate{
##   \item tests that don't match the stored reference tests
##   \item tests that don't exist in the reference tests
##   \item tests that exist in the reference tests but no the new file
##   \item tests that passed (these are omitted )
## }
## Because a lot of the logic for browsing these three types of situations is
## shared, that logic has been split off into
## \code{\link{reviewNext,unitizerBrowse-method}}. The key is that that function
## will return the items that are supposed to be stored in the unitizer.  These
## items will either be new or reference ones based on user decisions.
##
## Unfortunately, in order to be able to use the same logic for tasks that are
## not quite the same, a bit of contortion was needed.  In particular, the
## user is always asked to input either Y, N, or Q, but the corresponding output
## from \code{\link{reviewNext,unitizerBrowse-method}} is very different
## depending on what situation we're dealing with.
##
## One important point is that by default the user input is defined as N.  In
## all cases N means no change to the store, though again the interpretation is
## different depending on the situation.  For example, if we add a test to the
## test script, N means don't add it to the store.  If we remove a test, N means
## keep it in the store.
##
## @keywords internal
## @param x the object to browse
## @param y the derivative unitizerBrowse object of x; this needs to be passed
##   in as an argument because the logic for generating it is different
##   depending on whether we are using `unitize` or `review`.
## @return a unitizer if the unitizer was modified, FALSE otherwise

setMethod("browseUnitizer", c("unitizer", "unitizerBrowse"),
  function(x, y, force.update, ...) {

    if(identical(y@mode, "review") && (!isTRUE(y@interactive) || force.update)) {
      # nocov start
      stop(
        "Internal Error: attempt to enter unitizer in review mode in ",
        "non-interactive state or in force.update mode,  which should not be ",
        "possible, contact maintainer."
      )
      # nocov end
    }
    browse.res <- browseUnitizerInternal(x, y, force.update=force.update)
    x@global$resetInit()  # reset state

    # Need to store our `unitizer`

    if(browse.res@updated) {
      attempt <- try(store_unitizer(browse.res@unitizer))
      if(inherits(attempt, "try-error"))
        meta_word_msg(
          "Unable to store '", getTarget(browse.res@unitizer, "'"),
          trail.nl=FALSE
        )
    } else {
      meta_word_cat("unitizer unchanged.")
    }
    # Note how we don't actually return the result unitizer, but rather the
    # original one since that one will be re-used  in `unitize_browse` if it
    # isn't re-evaled, and the one stored here isn't in correct format for that
    # anymore.  Also note that `x` is actually modified since we mess with the
    # environments in `browseUnitizerInternal`

    x@updated <- browse.res@updated
    x@bookmark <- browse.res@bookmark
    browse.res@unitizer <- x
    browse.res
  }
)
setGeneric(
  "browseUnitizerInternal",
  function(x, y, ...) standardGeneric("browseUnitizerInternal")
)
setMethod(
  "browseUnitizerInternal", c("unitizer", "unitizerBrowse"),
  valueClass="unitizerBrowseResult",
  function(x, y, force.update, ...) {
    # Browse through tests that require user input, repeat so we give the user
    # an opportunity to adjust decisions before committing

    quit.time <- getOption("unitizer.prompt.b4.quit.time")
    if(is.null(quit.time)) quit.time <- 10
    update <- FALSE
    update.reeval <- FALSE
    slow.run <- x@eval.time > quit.time

    something.happened <- any(
      y@mapping@review.type != "Passed" & !y@mapping@ignored
    ) || (
      any(!y@mapping@ignored) && (
        identical(y@mode, "review") || y@start.at.browser
      )
    )
    if(!length(y)) {
      meta_word_cat("Empty unitizer; no tests to review.", trail.nl=FALSE)
    } else if(!something.happened && !force.update) {
      # nocov start shouldn't be possible to get here as this gets filtered out
      # by the review requirement in `unitize_browse`
      stop(
        "Internal error: All tests passed, unitizer store unchanged, you ",
        "should not be able to reach this point; contact maintainer."
      )
      # note we could just issue a message here and continue and everything
      # would be fine, which is what we did before we made this an internal
      # error
      # nocov end
    } else {
      # Check we if we requested a re-eval and if so set the id where we were
      # before re-eval

      if(!is.null(x@bookmark)) {
        cand.match <- which(x@bookmark@call == x@items.new.calls.deparse)
        cand.match.len <- length(cand.match)
        if(!cand.match.len || x@bookmark@id > cand.match.len) {
          meta_word_msg(
            cc(
              "Unable to find test you toggled re-eval from; starting ",
              "from beginning."
          ) )
        } else {
          match.id <- cand.match[x@bookmark@id]
          id.map <-
            which(y@mapping@item.id.orig == match.id & !y@mapping@item.ref)
          if(!length(id.map) == 1L) {
            # nocov start
            stop(
              "Internal Error: unable to find bookmarked test; contact ",
              "maintainer."
            )
            # nocov end
          }
          y@last.id <- y@mapping@item.id[id.map] - 1L
          y@jumping.to <- TRUE
        }
      }
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
                if(
                  first.time &&
                  (identical(y@mode, "review") || y@start.at.browser)
                ) {
                  # for passed tests, start by showing the list of tests
                  first.time <- FALSE
                  y@review <- 0L
                } else {
                  # we use y@review as delayed counter so that if user choses
                  # to review a normally unreviewed test, we can force the
                  # browse menu _after_ the first review by setting y@review
                  # to -1L

                  y <- reviewNext(y, x)
                  if(y@review) {
                     y@review <- y@review + 1L
                    next
                } }
                if(identical(y@review, 0L)) {
                  y.tmp <- review_prompt(y, new.env(parent=x@base.env))
                  if(identical(y.tmp, "Q")) {
                    invokeRestart("earlyExit")
                  } else if(!is(y.tmp, "unitizerBrowse")) {
                    # nocov start
                    stop(
                      "Internal Error: review should return `unitizerBrowse`; ",
                      "contact maintainer."
                    )
                    # nocov end
                  } else y <- y.tmp
                }
                # Automatically increment review counter since `review_prompt`
                # is called directly instead of within `reviewNext`

                y@review <- y@review + 1L
                next
            } },
            # a bit lazy to use a restart here, but this simplifies the logic
            # of being able to effectively have quit pathways from functions
            # called by this function, as well as functions called by functions
            # called by this function.

            earlyExit=function(mode="quit", extra=NULL) {
              if(identical(mode, "quit")) {
                user.quit <<- TRUE
                if(is(extra, "unitizerBrowse"))
                  y <<- extra
              } else stop(  # nocov start
                "Internal Error: unexpected early exit restart value; contact ",
                "maintainer"
              )             # nocov end
        } ) }
        # Get summary of changes

        keep <- !y@mapping@ignored
        changes <- split(
          y@mapping@review.val[keep] != y@mapping@review.def[keep],
          y@mapping@review.type[keep]
        )
        change.sum <- lapply(
          changes,
          function(x) c(sum(x), length(x))
        )
        for(i in names(change.sum))
          slot(x@changes, tolower(i)) <- change.sum[[i]]

        # Finalize depending on situation

        if(y@interactive.error) {
          meta_word_msg(
            "User input required to proceed, but we are in non-interactive ",
            "mode.", sep=""
          )
          break
        } else if(!y@human && !user.quit && y@auto.accept) {
          # quitting user doesn't allow us to register humanity...
          if(y@navigating || y@re.eval)
            stop(   # nocov start
              "Internal Error: should only get here in `auto.accept` mode, ",
              "contact maintainer"
            )       # nocov end
          meta_word_msg("Auto-accepting changes...", trail.nl=FALSE)
          update <- TRUE
          break
        } else if(
          length(x@changes) > 0L || (
            something.happened && (slow.run || !user.quit)
          ) || y@re.eval || force.update || y@force.up
        ) {
          print(H2("Finalize Unitizer"))

          # default update status; this can be modified if we cancel on exit
          # reeval update required to store last.id and must be tracked
          # separately so we can toggle it on or off without modifying overall
          # update decision; also, need to know if we started off in re.eval
          # mode since that tells us we activated re-eval while viewing tests
          # and not at the end
          #
          # Note, this is a nested repeat; there is an outer repeat that handles
          # individual test review, and this repeat handles the final prompt
          # to exit

          re.eval.started <- !!y@re.eval  # Were we already in re-eval mode?

          repeat {
            update <- length(x@changes) || force.update || y@force.up

            # Make sure we did not skip anything we were supposed to review

            unrevavail <- 0L
            if(identical(y@mode, "unitize")) {
              unreviewed <- unreviewed(y)
              unrevavail  <- length(unreviewed)
              if(unrevavail) {
                meta_word_cat(
                  "You have ", unrevavail, " unreviewed tests; press ",
                  "`B` to browse tests, `U` to go to first unreviewed test.\n",
                  sep=""
                )
            } }
            valid.opts <- c(
              Y="[Y]es", N=if(update) "[N]o", P="[P]rev", B="[B]rowse",
              U=if(unrevavail) "[U]nreviewed",  R="[R]erun", RR="",
              O=if(!length(x@changes) || (force.update || y@force.up))
                "f[O]rce" else "",
              QQ=if(y@multi) "[QQ]uit All"
            )
            if(!length(x@changes) && (force.update || y@force.up))
              meta_word_msg(
                "Running in `force.update` mode so `unitizer` will be re-saved",
                "even though there are no changes to record (see `?unitize`",
                "for details).", sep=" "
              )
            if(update) {
              tar <- getTarget(x)
              wd <- if(file.exists(tar)) get_package_dir(tar) else
                if(file.exists(dirname(tar)))
                  get_package_dir(dirname(tar)) else ""

              tar.final <- if(length(wd)) relativize_path(tar, wd=wd) else
                relativize_path(tar)

              if(!length(x@changes)) {
                meta_word_msg(
                  "You are about to update '", tar.final, "' with re-evaluated ",
                  "but otherwise unchanged tests.", sep=""
                )
              } else {
                meta_word_msg(
                  "You will IRREVERSIBLY modify '", tar.final, "'",
                  if(length(x@changes)) " by", ":", sep="", trail.nl=FALSE
                )
              }
            } else {
              meta_word_cat(
                "You made no changes to the unitizer so there is no need to",
                "update it.  While unnecessary, you can force an update by",
                "typing O at the prompt.", sep=" "
              )
            }
            if(length(x@changes) > 0) {
              meta_word_msg(
                as.character(x@changes, width=getOption("width") - 2L)
              )
            }
            # Can this be rationalized with the logic in `reviewNext`?

            actions <- character()
            if(update) {
              actions <- c(actions, "update unitizer")
              nav.hlp <- paste0(
                "Pressing Y will replace the previous unitizer with a new ",
                "one, pressing P or B will allow you to re-review your ",
                "choices.  Pressing N or Q both quit without saving changes to ",
                "the unitizer."
              )
            } else if(!length(x@changes)) {
              nav.hlp <- paste0(
                "Pressing Y will exit without saving the unitizer since you ",
                "did not make any changes.  Pressing P or B will allow you to ",
                "review any of the decisions you made previously, provided you ",
                "actually had any to make."
              )
            }
            if(y@re.eval) {
              if(identical(y@re.eval, 1L)) {
                actions <- c(actions, "re-run unitizer")
              } else if(identical(y@re.eval, 2L)) {
                actions <- c(actions, "re-run all loaded unitizers")
              } else stop("Internal Error: unexpected re-run value") # nocov
              nav.hlp <- paste0(
                nav.hlp,
                "\n\nAdditionally, pressing Y will cause re-running of ",
                "unitizers as per your input"
              )
            }
            if(!length(actions)) actions <- "exit unitizer"
            nav.msg <- cap_first(paste0(actions, collapse= " and "))
            meta_word_cat(
              nav.msg,
              paste0("(",
                paste0(valid.opts[nchar(valid.opts) > 0L], collapse=", "),
                ")?"
              ),
              sep=" "
            )
            user.input <- navigate_prompt(
              y, curr.id=max(y@mapping@item.id) + 1L,
              text=nav.msg, browse.env1=x@zero.env, help=nav.hlp,
              valid.opts=valid.opts
            )
            if(is(user.input, "unitizerBrowse")) {
              y <- user.input
              y@review <- y@review + 1L
              loop.status <- "n"
              break
            } else if (isTRUE(grepl("^RR?$", user.input))) {      # Re-eval
              y <- toggleReeval(y, user.input)
              next
            } else if (isTRUE(grepl("^O$", user.input))) { # Force update
              y <- toggleForceUp(y)
              next
            } else if (
              grepl("^[QN]$", user.input) || identical(user.input, "QQ")
            ) {
              update <- FALSE
              meta_word_msg("Changes discarded.", trail.nl=FALSE)
              if(y@re.eval)
                meta_word_msg("Re-evaluation disabled.", trail.nl=FALSE)
              y@re.eval <- 0L
              loop.status <- "b"
              if(identical(user.input, "QQ")) y@multi.quit <- TRUE
              break
            } else if (identical(user.input, "Y")) {
              loop.status <- "b"
              break
            }
            stop("Internal Error: unhandled user action") # nocov
          }
          switch(  # needed to handle multi level break
            loop.status, b=break, n=next,
            stop("Internal Error: invalid loop status, contact maintainer.")# nocov
          )
        } else {
          meta_word_msg("No changes recorded.", trail.nl=FALSE)
          break
        }
    } }
    # Create the new unitizer; note we re-use the same zero and base envs as
    # the original `unitizer` as otherwise we end up with incosistencies when
    # we try to re-use the original `unitizer` without reloading in the context
    # of `unitize_dir`

    items.ref <- processInput(y)
    items.ref <- healEnvs(items.ref, x) # repair the environment ancestry

    # Need to reconcile state.new / state.ref with items.ref here

    state.merged <- mergeStates(items.ref, x@state.new, x@state.ref)

    # Instantiate new unitizer and add selected items as reference items

    unitizer <- new(
      "unitizer", id=x@id, changes=x@changes, zero.env=x@zero.env,
      base.env=x@base.env, test.file.loc=x@test.file.loc,
      state.ref=state.merged$states
    )
    unitizer <- unitizer + state.merged$items

    # Extract and re-map sections of tests we're saving as reference

    if(!length(x@sections)) {
      if(!identical(y@mode, "review"))
        stop("Internal Error: should only get here in review mode") # nocov
      # Need to re-use our reference sections so `refSections` works since we
      # will not have created any sections by parsing/evaluating tests.  This
      # is super hacky as we're partly using the stuff related to `items.new`,
      # and could cause problems further down the road if we're not careful

      x@sections <- x@sections.ref
      x@section.map <- x@section.ref.map
    }
    unitizer <- refSections(unitizer, x)

    # If `re.eval.started` set, means we asked for re-eval while browsing tests
    # so we want to restart there; translate a browse id to a bookmark so we can
    # look it up later

    id.cur <- y@last.id
    bookmark <- if(
      y@re.eval && re.eval.started && !y@mapping@item.ref[[id.cur]]
    ) {
      id.map <- y@mapping@item.id.orig[[id.cur]]
      call.dep <- x@items.new.calls.deparse[id.map]
      call.dep.id <- x@items.new.calls.deparse.id[id.map]
      new("unitizerBrowseBookmark", call=call.dep, id=call.dep.id)
    }
    # Return structure

    new(
      "unitizerBrowseResult", unitizer=unitizer, re.eval=y@re.eval,
      updated=update, interactive.error=y@interactive.error,
      data=as.data.frame(y), bookmark=bookmark, multi.quit=y@multi.quit
    )
} )
setGeneric("reviewNext", function(x, ...) standardGeneric("reviewNext"))
# Bring up Review of Next test
#
# Generally we will go from one test to the next, where the next test is
# determined by the value of \code{x@@last.id}.  This means it is possible
# to affect the browsing order by modifying \code{x@@last.id}.
#
# This method is in charge of displaying all the output for review.
#
# @keywords internal

setMethod("reviewNext", c("unitizerBrowse"),
  function(x, unitizer, ...) {
    browsed <- x@browsing
    jumping <- x@jumping.to
    x@browsing <- x@jumping.to <- FALSE
    last.id <- x@last.id
    curr.id <- x@last.id + 1L
    x@last.id <- curr.id

    if(x@last.reviewed) {
      last.reviewed.sec <-
        x@mapping@sec.id[[which(x@mapping@item.id == x@last.reviewed)]]
      last.reviewed.sub.sec <-
        x@mapping@sub.sec.id[[which(x@mapping@item.id == x@last.reviewed)]]
      furthest.reviewed <- if(length(which(x@mapping@reviewed)))
        max(which(x@mapping@reviewed)) else 0L
      last.id.rel <-
        x@mapping@item.id.rel[[which(x@mapping@item.id == x@last.reviewed)]]
    } else {
      last.reviewed.sec <- last.reviewed.sub.sec <- furthest.reviewed <-
        last.id.rel <- 0L
    }
    x@last.reviewed <- curr.id

    curr.sec <- x@mapping@sec.id[[which(x@mapping@item.id == curr.id)]]
    curr.sub.sec <- x@mapping@sub.sec.id[[which(x@mapping@item.id == curr.id)]]
    cur.sub.sec.items <-
      x@mapping@sub.sec.id == curr.sub.sec & x@mapping@sec.id == curr.sec
    curr.sub.sec.obj <- x[[curr.sec]][[curr.sub.sec]]
    if(last.id.rel)
      last.sub.sec.obj <- x[[last.reviewed.sec]][[last.reviewed.sub.sec]]
    id.rel <- x@mapping@item.id.rel[[which(x@mapping@item.id == curr.id)]]

    # Display Section Headers as Necessary

    valid.opts <- c(
      Y="[Y]es", N="[N]o", P="[P]rev", B="[B]rowse", YY="", YYY="", YYYY="",
      NN="", NNN="", NNNN="", O="",
      if(identical(x@mode, "unitize")) c(R="[R]erun", RR=""),
      if(x@multi) c(QQ="[QQ]uit All")
    )
    # Pre compute whether sections are effectively ignored or not; these will
    # control whether stuff gets shown to screen or not

    ignore.passed <- !identical(x@mode, "review") &&
      is(curr.sub.sec.obj, "unitizerBrowseSubSectionPassed") &&
      !x@inspect.all && !x@start.at.browser

    ignore.sec <- all(
      (       # ignored and no errors
        x@mapping@ignored[x@mapping@sec.id == curr.sec] &
        !x@mapping@new.conditions[x@mapping@sec.id == curr.sec]
      ) | (   # passed and not in review mode
        x@mapping@review.type[x@mapping@sec.id == curr.sec] == "Passed" &
        (!identical(x@mode, "review") || !x@start.at.browser)
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

    # Used to track whether the previous thing displayed is an expression or
    # meta info

    prev.is.expr <- TRUE

    # Print Section title if appropriate, basically if not all the items are
    # ignored, or alternatively if one of the ignored items produced new
    # conditions, or if we just got here via a browse statement

    if(
      (
        !identical(last.reviewed.sec, curr.sec) && !ignore.sec ||
        browsed || jumping
      ) && multi.sect
    ) {
      prev.is.expr <- FALSE
      print(H2(x[[curr.sec]]@section.title))
    }
    if(        # Print sub-section title if appropriate
      (
        !identical(last.reviewed.sub.sec, curr.sub.sec) ||
        !identical(last.reviewed.sec, curr.sec)
      ) && !ignore.sub.sec || browsed || jumping
    ) {
      prev.is.expr <- FALSE
      print(H3(curr.sub.sec.obj@title))
      rev.count <- sum(!x@mapping@ignored[cur.sub.sec.items])

      prompt.txt <- paste(
        if(rev.count > 1L) {
          sprintf(curr.sub.sec.obj@detail.p, rev.count)
        } else curr.sub.sec.obj@detail.s,
        if(rev.count || x@inspect.all)
          paste0(
            sprintf(curr.sub.sec.obj@prompt, if(rev.count > 1L) "s" else ""),
            " ", "(",
            paste0(
              c(valid.opts[nchar(valid.opts) > 0], Q="[Q]uit", H="[H]elp"),
              collapse=", "
            ),
            ")?\n"
      ) )
      meta_word_cat(prompt.txt)
    }
    # Retrieve actual tests objects

    item.new <- if(!is.null(curr.sub.sec.obj@items.new))
      curr.sub.sec.obj@items.new[[id.rel]]
    item.ref <- if(!is.null(curr.sub.sec.obj@items.ref))
      curr.sub.sec.obj@items.ref[[id.rel]]

    # Assign main object (always new if present), and set up global setting
    # indices; always use indices.init if don't have new items.

    if(is.null(item.new)) {
      item.main <- item.ref
      base.env.pri <- parent.env(curr.sub.sec.obj@items.ref@base.env)
      new.glob.indices <- x@global$indices.init
    } else {
      item.main <- item.new
      base.env.pri <- parent.env(curr.sub.sec.obj@items.new@base.env)
      new.glob.indices <- item.new@glob.indices
    }
    # PROBLEM HERE: in "pass mode" we want the reference state, not the new
    # state, but the default behavior appears to be to bind to the new state

    if(!identical(x@global$indices.last, new.glob.indices))
      x@global$reset(new.glob.indices)

    # Show test to screen, but only if the entire section is not ignored, and
    # not passed tests, and requesting that those not be shown, and not elected
    # to review a test that isn't usually reviewed (x@review)

    diffs <- NULL

    if(!ignore.sub.sec || x@review == 0L) {
      if(x@mapping@reviewed[[curr.id]] && !identical(x@mode, "review")) {
        prev.is.expr <- FALSE
        meta_word_msg(
          "You are re-reviewing a test; previous selection was: \"",
          x@mapping@review.val[[curr.id]], "\"", sep=""
      ) }
      if(jumping) {
        prev.is.expr <- FALSE
        meta_word_msg(
          sep="",
          "Jumping to test #", x@mapping@item.id.ord[[curr.id]], " because ",
          "that was the test under review when test re-run was requested.",
          if(!is.null(unitizer@bookmark) && unitizer@bookmark@parse.mod)
            cc(
              " Note that since the test file was modified we cannot guarantee ",
              "the jump is to the correct test."
            )
        )
      }
      if(length(item.main@comment)) {
        if(prev.is.expr && x@mapping@ignored[last.id]) cat("\n")
        cat(
          word_comment(
            item.main@comment,
            color=unitizer@global$unitizer.opts[["unitizer.color"]]
          ), sep="\n"
        )
        cat("\n")
      }
      parsed.call <- try(parse(text=item.main@call.dep)[[1L]])
      if(inherits(parsed.call, "try-error")) {
        # nocov start
        stop("Internal Error: malformed call stored; contact maintainer.")
        # nocov end
      }
      cat(deparse_prompt(parsed.call), sep="\n")
      history_write(x@hist.con, item.main@call.dep)

      # show the message, and set the trace if relevant; options need to be
      # retrieved from unitizer object since they get reset

      out.std <- out.err <- FALSE
      if(
        (curr.sub.sec.obj@show.out || x@review == 0L) &&
        sum(nchar(item.main@data@output))
      ) {
        screen_out(
          item.main@data@output,
          max.len=unitizer@global$unitizer.opts[["unitizer.test.out.lines"]]
        )
        out.std <- TRUE
      }
      if(
        !is.null(item.new) && !is.null(item.ref) &&
        x@mapping@new.conditions[[curr.id]] || curr.sub.sec.obj@show.msg ||
        x@review == 0L
      ) {
        if(length(item.main@data@message) && nchar(item.main@data@message)) {
          screen_out(
            item.main@data@message,
            max.len=unitizer@global$unitizer.opts[["unitizer.test.msg.lines"]],
            stderr()
          )
          out.err <- TRUE
        }
        if(length(item.main@trace)) set_trace(item.main@trace)
      }
      # If test failed, show details of failure; note this should mean there
      # must be a `.new` and a `.ref`

      state.comp <- FALSE
      if(
        is(curr.sub.sec.obj@show.fail, "unitizerItemsTestsErrors") &&
        !item.main@ignore
      ) {
        cat("\n")
        err.obj <- curr.sub.sec.obj@show.fail[[id.rel]]
        err.obj@.fail.context <-
          unitizer@global$unitizer.opts[["unitizer.test.fail.context.lines"]]

        diffs <- as.Diffs(err.obj)

        # Extract specific state based on indices and attach the to the objects;
        # these objects will be discarded so we don't need to worry about
        # nulling them out

        item.new@state <- unitizerGlobalStateExtract(
          unitizer@state.new, item.new@glob.indices
        )
        item.ref@state <- unitizerGlobalStateExtract(
          unitizer@state.ref, item.ref@glob.indices
        )
        state.comp <- all.equal(item.ref@state, item.new@state, verbose=FALSE)
        if(!isTRUE(state.comp)) {
          diffs@state <- new(
            "unitizerItemTestsErrorsDiff", err=FALSE,
            txt="State mismatch:",
            txt.alt="State mismatch; see `.DIFF$state` for details.",
            show.diff=FALSE,
            diff=diffPrint(
              item.ref@state, item.new@state,
              tar.banner=quote(.REF$state),
              cur.banner=quote(.NEW$state)
        ) ) }
        # must eval to make sure that correct methods are available when
        # outputing failures to screen

        eval(
          call("show", diffs),
          if(is.environment(item.main@env)) item.main@env else base.env.pri
        )
        # Reset the diff to show state details in future

        if(!is.null(diffs@state)) diffs@state@show.diff <- TRUE

      } else if (out.std || out.err) cat("\n")
    }
    # Need to add ignored tests as default action is N, though note that ignored
    # tests are treated specially in `healEnvs` and are either included or removed
    # based on what happens to the subsequent non-ignored test.

    if(!x@inspect.all) {
      if(
        x@mapping@ignored[[curr.id]] || ignore.passed ||
        (x@mapping@reviewed[[curr.id]] && !x@navigating)
      ) {
        # reviewed items are skipped unless we're actively navigating to support
        # `auto.accept`
        return(x)
      }
    }
    # If we get past this point, then we will need some sort of human input, so
    # we mark the browse object

    if(!x@interactive) {   # can't proceed in non-interactive
      x@interactive.error <- TRUE
      invokeRestart("earlyExit", extra=x)
    }
    x@human <- TRUE

    # Create evaluation environment; these are really two nested environments,
    # with the parent environment containing the unitizerItem values and the
    # child environment containing the actual unitizer items.  This is so that
    # when user evaluates `.new` or `.ref` they see the value, but then we can
    # easily retrieve the full object with the `get*` functions.

    var.list <- c(
      if(!is.null(item.new))
        list(.NEW=item.new, .new=item.new@data@value[[1L]]),
      if(!is.null(item.ref))
        list(.REF=item.ref, .ref=item.ref@data@value[[1L]]),
      if(!is.null(diffs)) {
        c(
          list(.DIFF=diffs),
          if(!is.null(diffs@value)) list(.diff=diffs@value@diff)
      ) }
    )
    browse.env <- list2env(var.list, parent=item.main@env)
    browse.eval.env <- new.env(parent=browse.env)

    # Functions to override

    env.sec <- if(!is.null(item.new) && !is.null(item.ref))
      item.ref@env else NULL
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
    # More details for help in failure case
    help.extra.1 <- help.extra.2 <- ""
    if(identical(tolower(curr.sub.sec.obj@title), "failed")) {
      fails <- x@mapping@tests.result[curr.id, ]
      fail.name <- names(fails)[!fails]
      help.extra.1 <- if(length(fail.name) > 1L) {
        paste0(
          "mismatches in test ",
          paste0(head(fail.name, -1L), collapse=", "), ", and ",
          tail(fail.name, 1L)
        )
      } else if(length(fail.name) == 1L) {
        sprintf("mismatch in test %s", fail.name)
      } else {
        # nocov start
        stop(
          "Internal Error: test failures must have populated @tests.results ",
          "values; contact maintainer."
        )
        # nocov end
      }
      if("conditions" %in% fail.name) {
        help.extra.2 <- cc(
          "\n\nYou can retrieve individual conditions from the `conditionList` ",
          "objects inside the test objects; for example, use ",
          "`.NEW$conditions[[1L]]` to get first condition from new evaluation."
        )
      }
    }
    # Options to navigate; when navigating the name of the game is set `@last.id`
    # to the non-ignored test just previous to the one you want to navigate to,
    # the loop will then advance you to that test

    help.prompt <- paste0(
      "Reviewing test #", curr.id, " (type: ", tolower(curr.sub.sec.obj@title),
      "). ", sprintf(curr.sub.sec.obj@help, help.extra.1, help.extra.2),
      "\n\nIn addition to any valid R expression, you may type the following ",
      "at the prompt (without backticks):\n\n"
    )
    help.opts <- c(
      "`P` to go to the previous test",
      "`B` to see a listing of all tests",
      "`ls()` to see what objects are available to inspect",
      if(!is.null(item.new))
        "`.new` for the current value, or `.NEW` for the full test object",
      if(!is.null(item.ref))
        paste0(
          "`.ref` for the reference value, or `.REF` for the full reference ",
          "object"
        ),
      if(!is.null(item.new) && !is.null(item.ref))
        paste0(
          "`.diff` for a diff between `.new` and `.ref`, and `.DIFF`  for the ",
          "differences between all components in `.NEW` and `.REF`."
        ),
      paste0(
        "`YY`/`NN`, `YYY`/`NNN`, `YYYY`/`NNNN` to apply same choice to all ",
        "remaining unreviewed items in, respectively, the sub-section, ",
        "section, or unitizer"
      ),
      if(identical(x@mode, "unitize")) {
        c(
          paste0(
            "`R` to re-run the unitizer or `RR` to re-run all loaded ",
            "unitizers; used typically after you re-`install` the package you ",
            "are testing via the unitizer prompt"
          ),
          paste0(
            "`O` to f[O]rce update of store even when there are no accepted ",
            "changes"
        ) )
      },
      if(x@multi)
        paste0(
          "`QQ` to quit this unitizer and interrupt review of other queued  ",
          "unitizers"
        )
    )
    # navigate_prompt handles the P and B cases internally and modifies the
    # unitizerBrowse to be at the appropriate location; this is done as a
    # function because same logic is re-used elsewhere

    repeat {   # repeat needed just for re-eval toggle
      if(
        is(
          x.mod <- navigate_prompt(
            x=x, curr.id=curr.id, text=sprintf(curr.sub.sec.obj@prompt, ""),
            browse.env1=browse.eval.env,
            browse.env2=new.env(parent=parent.env(base.env.pri)),
            valid.opts=valid.opts,
            help=help.prompt, help.opts=help.opts
          ),
          "unitizerBrowse"
        )
      ) {
        return(x.mod)
      } else if (isTRUE(grepl("^RR?$", x.mod))) {           # Re-eval
        x <- toggleReeval(x, x.mod)
        Sys.sleep(0.3)  # so people can see the toggle message
        invokeRestart("earlyExit", extra=x)
      } else if (isTRUE(grepl("^O$", x.mod))) {             # Force update
        x <- toggleForceUp(x)
        next
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
          } else {
            # nocov start
            stop("Internal Error: unexpected number of Y/N; contact maintainer.")
            # nocov end
          }

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
                "are reviewing.  Consult the following table for details:\n"
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
              valid.opts=c(Y="[Y]es", N="[N]o"), global=x@global,
              browse.env=new.env(parent=parent.env(base.env.pri))
            )
            if(identical(act.conf, "Q")) invokeRestart("earlyExit", extra=x)
            if(identical(act.conf, "N")) {
              x@last.id <- x@last.id - 1L  # Otherwise we advance to next test
              return(x)
            }
          }
          indices
        }
        if(!any(rev.ind)) {
          stop("Internal Error: no tests to accept/reject") # nocov
        }

        x@mapping@reviewed[rev.ind] <- TRUE
        x@mapping@review.val[rev.ind] <- act
        x@last.id <- max(rev.ind)
      } else if (identical(x.mod, "Q")) {
        invokeRestart("earlyExit", extra=x)
      } else if (identical(x.mod, "QQ")) {
        x@multi.quit <- TRUE
        invokeRestart("earlyExit", extra=x)
      } else {
        # nocov start
        stop(
          "Internal Error: `unitizer_prompt` returned unexpected value; ",
          "contact maintainer"
        )
        # nocov end
      }
      break
    }
    x
  }
)
# Re-eval toggling, only b/c we need to do it in a couple of places'
# @keywords internal

setGeneric("toggleReeval", function(x, ...) standardGeneric("toggleReeval"))
setMethod("toggleReeval", "unitizerBrowse",
  function(x, y, ...) {
    re.status <- if(x@re.eval) "OFF" else "ON"
    re.mode <- switch(
      nchar(y), "this unitizer", "all loaded unitizers"
    )
    meta_word_msg("Toggling re-run mode", re.status, "for", re.mode, sep=" ")
    x@re.eval <- if(x@re.eval) 0L else nchar(y)
    x
})
setGeneric("toggleForceUp", function(x, ...) standardGeneric("toggleForceUp"))
setMethod("toggleForceUp", "unitizerBrowse",
  function(x, ...) {
    re.status <- if(x@force.up) "OFF" else "ON"
    meta_word_msg("Toggling force update mode", re.status, sep=" ")
    x@force.up <- !x@force.up
    x
})


