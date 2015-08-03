#' Runs The Basic Stuff
#'
#' Used by both \code{\link{unitize}} and \code{\link{review}}
#' to launch the interactive interface for reviewing tests.
#'
#' Right now we distinguish in what mode we're running based on whether
#' \code{test.file} is NULL (review mode) vs. not (unitize mode), which isn't
#' very elegant, but whatevs.  This has implications for the parsing / evaluation
#' step, as well as how the \code{unitizerBrowse} object is constructed.  Otherwise
#' stuff is mostly the same.
#'
#' Cleary there is a trade-off in increased code complexity to handle both types
#' of code, vs duplication.  Not ideal, but tasks are so closely related and
#' there is so much common overhead, that the central function makes sense.
#' Also, since unfortunately we're relying on side-effects for some features, and
#' \code{on.exit} call for safe operation, it is difficult to truly modularize.
#'
#' @keywords internal
#' @inheritParams unitize
#' @param mode character(1L) one of "review" or "unitize"
#' @param test.files character location of test files
#' @param store.ids list of store ids, same length as \code{test.files}

unitize_core <- function(
  test.files, store.ids, state, pre, post, history, interactive.mode,
  force.update, auto.accept, mode
) {
  # - Validation / Setup -------------------------------------------------------

  if(!is.chr1(mode) || !mode %in% c("unitize", "review"))
    stop("Logic Error: incorrect value for `mode`; contact maintainer")
  if(mode == "review") {
    if(!all(is.na(test.files)))
      stop(
        "Logic Error: `test.files` must be NA in review; contact maintainer"
      )
    if(length(auto.accept))
      stop("Logic Error: auto-accepts not allowed in review mode")
    if(!identical(state, "off")
    )
      stop("Logic Error: state must be disabled in review mode")
  }
  if(mode == "unitize") {
    if(
      !is.character(test.files) || any(is.na(test.files)) ||
      !all(file_test("-f", test.files))
    )
      stop(
        "Logic Error: `test.files` must all point to valid files in unitize ",
        "mode; contact maintainer"
      )
    test.files <- try(normalizePath(test.files, mustWork=TRUE))
    if(inherits(test.files, "try-error"))
      stop("Logic Error: unable to normalize test files; contact maintainer")
  }
  if(length(test.files) != length(store.ids))
    stop(
      "Logic Error: mismatch in test file an store lengths; contact maintainer"
    )
  if(!length(test.files))
    stop("Logic Error: expected at least one test file; contact maintainer.")
  if(!is.TF(interactive.mode))
    stop("Argument `interactive.mode` must be TRUE or FALSE")
  if(!is.TF(force.update)) stop("Argument `force.update` must be TRUE or FALSE")

  # Validate state; note that due to legacy code we disassemble state into the
  # par.env and other components

  state <- is.valid_state(state)
  if(!is(state, "unitizerState"))
    stop(
      "Argument `state` could not be interpreted as a `unitizerState`; see ",
      "prior errors"
    )
  par.env <- state@par.env
  reproducible.state <- vapply(
    setdiff(slotNames(state), "par.env"), slot, integer(1L), object=state
  )
  # auto.accept

  auto.accept.valid <- character()
  if(is.character(auto.accept)) {
    if(length(auto.accept)) {
      auto.accept.valid <-
        tolower(levels(new("unitizerBrowseMapping")@review.type))

      if(any(is.na(auto.accept)))
        stop("Argument `auto.accept` contains NAs but should not")
      auto.accept <- unique(tolower(auto.accept))
      if(!all(auto.accept %in% auto.accept.valid))
        stop(
          "Argument `auto.accept` must contain only values in ",
          deparse(tolower(auto.accept.valid))
        )
    }
  } else stop("Argument `auto.accept` must be character")
  if(length(auto.accept) && !length(test.files))
    stop("Argument `test.files` must be specified when using `auto.accept`")

  # validate and convert pre and post load folders to character; this doesn't
  # check that they point to real stuff

  test.dir <- if(length(test.files)) dirname(test.files[[1L]])
  pre <- validate_pre_post(pre, test.dir)
  post <- validate_pre_post(post, test.dir)

  # Make sure history is kosher

  if(nchar(history)) {
    test.con <- try(file(history, "at"))
    if(inherits(test.con, "try-error"))
      stop(
        "Argument `history` must be the name of a file that can be opened in ",
        "\"at\" mode"
      )
    close(test.con)
  } else history <- tempfile()

  # Make sure nothing untoward will happen if a test triggers an error

  check_call_stack()

  # Create parent directories for untizer stores if needed, doing now so that
  # we can later ensure that store ids are being specified on an absolute basis,
  # and also so we can prompt the user now

  dir.names <- vapply(
    store.ids,
    function(x) {
      if(is.character(x) && !is.object(x) && !file_test("-d", dirname(x))) {
        dirname(x)
      } else NA_character_
    },
    character(1L)
  )
  dir.names.clean <- Filter(Negate(is.na), unique(dir.names))
  if(length(dir.names.clean)) {
    dir.word <-
      paste0("director", if(length(dir.names.clean) > 1L) "ies" else "y")
    word_cat(
      "In order to proceed unitizer must create the following ", dir.word,
      ":\n"
    )
    print(UL(dir.names.clean))
    prompt <- paste0("Create ", dir.word)
    word_cat(prompt, "?", sep="")

    pick <- unitizer_prompt(prompt, valid.opts=c(Y="[Y]es", N="[N]o"))
    if(!identical(pick, "Y"))
      stop("Cannot proceed without creating directories.")
    if(!all(dir.created <- dir.create(dir.names.clean, recursive=TRUE))) {
      stop(
        "Cannot proceed, failed to create the following directories:\n",
        paste0(" - ", dir.names.clean[!dir.created], collapse="\n")
  ) } }
  # Ensure directory names are normalized, but only if dealing with char objects

  norm.attempt <- try(
    store.ids <- lapply(
      store.ids,
      function(x) {
        if(is.character(x) && !is.object(x)) {
          file.path(normalizePath(dirname(x), mustWork=TRUE), basename(x))
        } else x
  } ) )
  if(inherits(norm.attempt, "try-error"))
    stop(
      "Logic Error: some `store.ids` could not be normalized; contact ",
      "maintainer."
    )
  # - Global Controls ----------------------------------------------------------

  # Store a copy of the unitizer options, though make sure to validate them
  # first (note validation is potentially a bit duplicative since some of the
  # params would have been pulled from options); we store the opts because they
  # get nuked by `options_zero` but we still need some of the unitizer ones

  opts <- options()
  opts.untz <- opts[grep("^unitizer\\.", names(opts))]
  validate_options(opts.untz)

  # Initialize new tracking object; this will also record starting state and
  # store unitizer options; open question of how exposed we want to be to
  # user manipulation of options

  global <- unitizerGlobal$new(
    enable.which=reproducible.state,
    unitizer.opts=opts.untz             # this may no longer be necessary now that we don't unload unitizer options
  )
  if(is.null(par.env)) {
    global$shimFuns()
    par.env <- global$par.env
  }
  on.exit(
    {
      reset_and_unshim(global)
      word_msg(
        "Unexpectedly exited before storing unitizer; tests were not saved or",
        "changed."
      )
    },
    add=TRUE
  )
  gpar.frame <- par.env

  # Set the zero state if needed; `seach.path` should be done first so that we
  # can disable options if there is a conflict there; WARNING, there is some
  # fragility here since it is possible using these functions could modify
  # global (see `options` example)

  seed.dat <- getOption("unitizer.seed")  # get seed before 'options_zero'

  if(identical(global$status@search.path, 2L))
    search_path_trim(
      global=global,
      keep.ns=global$unitizer.opts[["unitizer.namespace.keep"]],
      keep.path=global$unitizer.opts[["unitizer.search.path.keep"]]
    )
  if(global$ns.opt.conflict@conflict) global$ns.opt.conflict@file <- ""  # indicate conflict happened prior to test eval

  if(identical(global$status@options, 2L)) options_zero()
  if(identical(global$status@random.seed, 2L)) {
    if(inherits(try(do.call(set.seed, seed.dat)), "try-error")) {
      stop(
        "Unable to set random seed; make sure `getOption('unitizer.seed')` ",
        "is a list of possible arguments to `set.seed`."
  ) } }
  if(identical(global$status@working.directory, 2L)) {
    if(
      length(unique(dirname(test.files)) == 1L) &&
      length(par.dir <- get_package_dir(test.files[[1L]]))
    ) {
      setwd(par.dir)
    } else
      warning(
        "Test files do not appear to be part of a package; leaving working ",
        "directory unchanged.", immediate.=TRUE
      )
  }
  # - Parse / Load -------------------------------------------------------------

  # Handle pre-load data

  over_print("Preloads...")
  pre.load.frame <- source_files(pre, gpar.frame)
  if(!is.environment(pre.load.frame))
    stop("Argument `pre` could not be interpreted:\n", pre.load.frame)

  global$state("init")  # mark post pre-load state

  util.frame <- new.env(parent=pre.load.frame)
  assign("quit", unitizer_quit, util.frame)
  assign("q", unitizer_quit, util.frame)

  over_print("Loading unitizer data...")
  eval.which <- seq_along(store.ids)
  valid <- rep(TRUE, length(eval.which))
  unitizers <- new("unitizerObjectList")

  # - Evaluate / Browse --------------------------------------------------------

  # Parse, and use `eval.which` to determine which tests to evaluate

  if(identical(mode, "unitize")) {
    over_print("Parsing tests...")
    tests.parsed <- lapply(
      test.files,
      function(x) {
        over_print(paste("Parsing", x))
        parse_tests(x, comments=TRUE)
  } ) }
  over_print("")

  while(
    (length(eval.which) || mode == identical(mode, "review")) && length(valid)
  ) {
    active <- intersect(eval.which, which(valid)) # kind of implied in `eval.which` after first loop

    # Load / create all the unitizers; note loading envs with references to
    # namespace envs can cause state to change so we need to record it here;
    # also, `global` is attached to the `unitizer` here

    unitizers[active] <- load_unitizers(
      store.ids[active], test.files[active], par.frame=util.frame,
      interactive.mode=interactive.mode, mode=mode, global=global
    )
    global$state()
    valid <- vapply(as.list(unitizers), is, logical(1L), "unitizer")

    # Now evaluate, whether a unitizer is evaluated or not is a function of
    # the slot @eval, set just above as they are loaded

    if(identical(mode, "unitize"))
      unitizers[valid] <- unitize_eval(
        tests.parsed=tests.parsed[valid], unitizers=unitizers[valid],
        global=global
      )
    # Gather user input, and store tests as required.  Any unitizers that
    # the user marked for re-evaluation will be re-evaluated in this loop

    unitizers[valid] <- unitize_browse(
      unitizers=unitizers[valid],
      mode=mode,
      interactive.mode=interactive.mode,
      force.update=force.update,
      auto.accept=auto.accept,
      history=history,
      global=global
    )
    eval.which.valid <- which(
      vapply(as.list(unitizers[valid]), slot, logical(1L), "eval")
    )
    eval.which <- which(valid)[eval.which.valid]
    if(identical(mode, "review")) break
  }
  # - Finalize -----------------------------------------------------------------

  on.exit(NULL)
  reset_and_unshim(global)

  post.res <- source_files(post, gpar.frame)  # return env on success, char on error
  if(!is.environment(post.res))
    word_msg(
      "`unitizer` evaluation succeed, but `post` steps had errors:",
      post.res
    )

  return(as.list(unitizers))
}
#' Evaluate User Tests
#'
#' @param tests.parsed a list of expressions
#' @param unitizers a list of \code{unitizer} objects of same length as \code{tests.parsed}
#' @param which integer which of \code{unitizer}s to actually eval, all get
#'   summary status displayed to screen
#' @return a list of unitizers
#' @keywords internal

unitize_eval <- function(tests.parsed, unitizers, global) {
  test.len <- length(tests.parsed)
  if(!identical(test.len, length(unitizers)))
    stop(
      "Logic Error: parse data and unitizer length mismatch; contact ",
      "maintainer."
    )
  # Set up display stuff

  num.digits <- as.integer(ceiling(log10(test.len + 1L)))
  tpl <- paste0("%", num.digits, "d/", test.len)

  # Loop through all unitizers, evaluating the ones that have been marked with
  # the `eval` slot for evaluation, and resetting that slot to FALSE

  for(i in seq(length=test.len)) {
    test.dat <- tests.parsed[[i]]
    unitizer <- unitizers[[i]]

    if(unitizer@eval) {
      # reset global settings if active to just after pre-loads (DO WE NEED TO
      # CHECK WHETHER THIS MODE IS ENABLED, OR IS IT HANDLED INERNALLY?)

      tests <- new("unitizerTests") + test.dat
      if(test.len > 1L)
        over_print(
          paste0(sprintf(tpl, i), " ", basename(unitizer@test.file.loc), ": ")
        )
      unitizers[[i]] <- unitizer + tests
      global$resetInit()
      if(
        global$ns.opt.conflict@conflict && !length(global$ns.opt.conflict@file)
      )
        global$ns.opt.conflict@file <- basename(unitizer@test.file.loc)
    } else {
      unitizers[[i]] <- unitizer
    }
    unitizers[[i]]@eval <- FALSE
    glob.opts <- Filter(Negate(is.null), lapply(global$tracking@options, names))
    glob.opts <- if(!length(glob.opts)) character(0L) else unique(unlist(glob.opts))

    no.track <- c(
      unlist(
        lapply(global$unitizer.opts[["unitizer.opts.asis"]], grep, glob.opts)
      ),
      match(
        names(global$unitizer.opts[["unitizer.opts.base"]]), glob.opts, nomatch=0L
    ) )
    unitizers[[i]]@state.new <- unitizerCompressTracking(
      global$tracking, glob.opts[no.track]
    )
  }
  unitizers
}
#' Run User Interaction And \code{unitizer} Storage
#'
#' @keywords internal
#' @inheritParams unitize_core
#' @param unitizers list of \code{unitizer} objects
#' @param force.update whether to store unitizer

unitize_browse <- function(
  unitizers, mode, interactive.mode, force.update, auto.accept, history, global
) {
  # - Prep ---------------------------------------------------------------------

  if(!length(unitizers)) {
    message("No tests to review")
    return(unitizers)
  }
  over_print("Prepping Unitizers...")

  hist.obj <- history_capt(history)
  on.exit(history_release(hist.obj))

  # Get summaries

  test.len <- length(unitizers)
  summaries <- summary(unitizers, silent=TRUE)
  totals <- vapply(as.list(summaries), slot, summaries[[1L]]@totals, "totals")
  to.review <- colSums(totals[-1L, , drop=FALSE]) > 0L  # First row will be passed

  # Determine implied review mode (all tests passed in a particular unitizer,
  # but user may still pick it to review)

  review.mode <- ifelse(
    !to.review & !force.update & test.len > 1L, "review", mode
  )
  untz.browsers <- mapply(
    browsePrep, as.list(unitizers), review.mode,
    MoreArgs=list(hist.con=hist.obj$con, interactive=interactive.mode),
    SIMPLIFY=FALSE
  )
  # Decide what to keep / override / etc.
  # Apply auto-accepts, if any (shouldn't be any in "review mode")

  # NOTE: are there issues with auto-accepts when we run this function more
  # than once, where previous choices are over-written by the auto-accepts?
  # maybe auto-accepts only get applied first time around?

  eval.which <- integer(0L)

  if(length(auto.accept)) {
    over_print("Applying auto-accepts...")
    for(i in seq_along(untz.browsers)) {
      auto.accepted <- 0L
      for(auto.val in auto.accept) {
        auto.type <- which(
          tolower(untz.browsers[[i]]@mapping@review.type) == auto.val
        )
        untz.browsers[[i]]@mapping@review.val[auto.type] <- "Y"
        untz.browsers[[i]]@mapping@reviewed[auto.type] <- TRUE
        auto.accepted <- auto.accepted + length(auto.type)  # not used?
      }
      if(auto.accepted) untz.browsers[[i]]@auto.accept <- TRUE
  } }
  # Browse, or fail depending on interactive mode

  reviewed <- int.error <- logical(test.len)
  over_print("")

  # Re-used message

  # - Interactive --------------------------------------------------------------

  if(test.len > 1L) show(summaries)
  quit <- FALSE

  # If any conflicts in state tracking are detected, alert user and give them
  # a chance to bail out

  if(global$ns.opt.conflict@conflict) {
    many <- length(global$ns.opt.conflict@namespaces)
    word_msg(
      "`unitizer` was unable to run with `options` state tracking enabled ",
      "starting with ",
      if(!nchar(global$ns.opt.conflict@file)) "the first test file" else
        paste0("test file \"", global$ns.opt.conflict@file, "\""),
      " because in order to do we would have had to unload the following ",
      "namespace", if(many > 1L) "s", " in contravention of ",
      "`getOption(unitizer.namespace.keep)`: ",
      paste0(deparse(global$ns.opt.conflict@namespaces, width=500L), sep=""),
      ".", sep=""
    )
    if(interactive.mode) {
      word_msg(
        "You may proceed normally but be aware that option state was not ",
        "managed starting with the file in question, and option state will ",
        "not be managed during review, or restored to original values after ",
        "`unitizer` completes evaluation.  You may quit `unitizer` now to ",
        "avoid any changes.  See state vignette for more details.", sep=""
      )
      proceed <- "Do you wish to proceed despite compromised state tracking"
      word_cat(proceed, "([Y]es, [N]o)?\n")
      prompt <- unitizer_prompt(
        "Do you wish to proceed despite compromised state tracking",
        valid.opts=c(Y="[Y]es", N="[N]o"),
        exit.condition=exit_fun, valid.vals=seq.int(test.len),
        hist.con=hist.obj$con, help=help
      )
      if(prompt %in% c("N", "Q") && confirm_quit(unitizers)) quit <- TRUE
    } else {
      stop(
        "Unable to proceed in non-interactive mode; set options state ",
        "tracking to a value less than or equal to search path state tracking ",
        "or see vignette for other workarounds."
  ) } }

  if(!quit) {
    if(identical(mode, "review") || any(to.review) || force.update) {
      # We have fairly different treatment for a single test versus multi-test
      # review, so the logic gets a little convoluted (keep eye out for)
      # `test.len > 1L`, but this obviates the need for multiple different calls
      # to `browseUnitizers`

      # Additional convolution introduced given the need to handle the
      # possibility of auto.accepts in non-interactive mode, and since for ease
      # of implementation we chose to do auto.accepts through `browseUnitizer`,
      # we need to add some hacks to handle that outcome since by design
      # originally the browse stuff was never meant to handle non-interactive
      # use...

      first.time <- TRUE
      repeat {
        prompt <- paste0(
          "\nType number of unitizer to review",
          if(any(to.review))
            ", 'A' to review all that require review",
          if(any(summaries@updated))
            ", 'R' to re-evaluate all updated"
        )
        help.opts <- c(
          paste0(deparse(seq.int(test.len)), ": unitizer number to review"),
          if(any(to.review)) "A: Review all `unitzers` that require review (*)",
          "AA: Review all tests",
          if(any(summaries@updated)) "R: Re-evaluate all updated unitizers ($)",
          "RR: Re-evaluate all tests",
          "Q: quit"
        )
        help <- c(
          "Available options:",
          paste0(as.character(UL(help.opts)), collapse="\n")
        )
        if(!first.time) {
          if(!interactive.mode)
            stop(
              "Logic Error: looping for user input in non-interactive mode, ",
              "contact maintainer."
            )
          show(summaries)
        }
        first.time <- FALSE
        eval.which <- integer(0L)

        if(test.len > 1L) {
          pick.num <- integer()
          pick <- if(interactive.mode) {
            word_cat(prompt)
            unitizer_prompt(
              "Pick a unitizer or an option",
              valid.opts=c(
                A=if(any(to.review)) "[A]ll",
                R=if(any(summaries@updated)) "[R]e-eval",
                AA="", RR=""
              ),
              exit.condition=exit_fun, valid.vals=seq.int(test.len),
              hist.con=hist.obj$con, help=help
            )
          } else {
            # in non.interactive mode, review all, this will apply auto.accepts
            # if successfull
            "A"
          }
          if(identical(pick, "Q")) {
            if(confirm_quit(unitizers)) break else next
          } else if(identical(pick, "A")) {
            pick.num <- which(to.review & !summaries@updated)
          } else if(identical(pick, "AA")) {
            pick.num <- seq.int(test.len)
          } else if(identical(pick, "R")) {
            eval.which <- which(summaries@updated)
          } else if(identical(pick, "RR")) {
            eval.which <- seq.int(test.len)
          } else {
            pick.num <- as.integer(pick)
            if(!pick.num %in% seq.int(test.len)) {
              word_msg(
                "Input not a valid unitizer; choose in ",
                deparse(seq.int(test.len))
              )
              next
          } }
        } else pick.num <- 1L

        for(i in pick.num) {
          print(
            H1(
              paste0(
                "unitizer for: ", getName(unitizers[[i]]), collapse=""
          ) ) )
          if(identical(untz.browsers[[i]]@mode, "unitize")) show(summaries[[i]])  # summaries don't really work well in review mode if the tests are not evaluated
          browse.res <- browseUnitizer(
            unitizers[[i]], untz.browsers[[i]],
            force.update=force.update,  # annoyingly we need to force update here as well as for the unreviewed unitizers
          )
          summaries@updated[[i]] <- browse.res@updated
          unitizers[[i]] <- browse.res@unitizer
          int.error[[i]] <- browse.res@interactive.error

          # Check to see if any need to be re-evaled, and if so, mark unitizers
          # and return

          eval.which <- unique(
            c(
              eval.which,
              if(identical(browse.res@re.eval, 1L)) {
                i
              } else if(identical(browse.res@re.eval, 2L)) seq.int(test.len)
          ) )
        }
        # - Non-interactive Issues -----------------------------------------------
        if(any(int.error)) {
          if(interactive.mode)
            stop(
              "Logic Error: should not get here in interactive mode; contact ",
              " maintainer"
            )
          # Problems during non-interactive review; we only allow this as a
          # mechanism for allowing auto-accepts in non-interactive mode

          for(i in which(int.error)) {
            untz <- unitizers[[i]]
            delta.show <- untz@tests.status != "Pass" & !ignored(untz@items.new)
            word_msg(
              paste0(
                "  * ",
                format(paste0(untz@tests.status[delta.show], ": ")),
                untz@items.new.calls.deparse[delta.show],
                collapse="\n"
              ),
              "\nin '", relativize_path(untz@test.file.loc), "'\n",
              sep=""
            )
          }
          stop(
            "Newly evaluated tests do not match unitizer (",
            paste(
              names(summaries@totals), summaries@totals, sep=": ", collapse=", "
            ),
            "); see above for more info, or run in interactive mode"
          )
        }
        # - Simple Outcomes / no-review ------------------------------------------

        if(identical(test.len, 1L) || length(eval.which) || !interactive.mode)
          break
      }
    } else {
      message("All tests passed; nothing to review.")
    }
  } else eval.which <- integer(0L)  # we quit, so don't want to re-evalute anything

  # Set eval status before return

  if(length(eval.which)) {
    for(i in eval.which) unitizers[[i]]@eval <- TRUE
  } else for(i in seq_along(unitizers))  unitizers[[i]]@eval <- FALSE  # this one may not be necessary

  unitizers
}
#' Check Not Running in Undesirable Environments
#'
#' Make sure not running inside withCallingHandlers / withRestarts / tryCatch
#' or other potential issues; of course this isn't foolproof if someone is using
#' a variation on those functions, but also not the end of the world if it isn't
#' caught
#'
#' @keywords internal

check_call_stack <- function() {
  call.stack <- sys.calls()
  if(
    any(
      vapply(
        call.stack, FUN.VALUE=logical(1L),
        function(x)
          is.symbol(x[[1]]) &&
          as.character(x[[1]]) %in%
          c("withCallingHandlers", "withRestarts", "tryCatch")
    ) )
  ) warning(
    "It appears you are running unitizer inside an error handling function such ",
    "as `withCallingHanlders`, `tryCatch`, or `withRestarts`.  This is strongly ",
    "discouraged as it may cause unpredictable behavior from unitizer in the ",
    "event tests produce conditions / errors.  We strongly recommend you re-run ",
    "your tests outside of such handling functions.", immediate.=TRUE
  )
  restarts <- computeRestarts()
  restart.names <- vapply(restarts, `[[`, character(1L), 1L)
  if("unitizerQuitExit" %in% restart.names)
    stop(
      "`unitizerQuitExit` restart is already defined; unitizer relies on this ",
      "restart to restore state prior to exit, so unitizer will not run if it is ",
      "defined outside of `unitize`.  If you did not define this restart contact ",
      "maintainer."
    )
}
#' Helper function for validations
#'
#' @keywords internal

validate_pre_post <- function(what, test.dir) {
  which <- deparse(substitute(what))
  stopifnot(
    which %in% c("pre", "post"),
    (is.character(test.dir) && length(test.dir) == 1L) || is.null(test.dir)
  )
  if(
    !is.null(what) && !identical(what, FALSE) && !is.character(what)
  )
    stop(
      simpleError(
        message=paste0(
          "Argument `", which, "` must be NULL, FALSE, or character"
        ),
        call=sys.call(-1L)
    ) )
  if(is.null(what) && !is.null(test.dir)) {
    tmp <- file.path(test.dir, sprintf("_%s", which))
    if(file_test("-d", tmp)) tmp else character(0L)
  } else if (is.character(what)) {
    what
  } else character(0L)
}


#' Helper function for global state stuff
#'
#' Maybe this should be a global method?
#'
#' @keywords internal

reset_and_unshim <- function(global) {
  stopifnot(is(global, "unitizerGlobal"))
  glob.clear <- try(global$resetFull())
  glob.unshim <- try(global$unshimFuns())
  success.clear <- !inherits(glob.clear, "try-error")
  success.unshim <-  !inherits(glob.unshim, "try-error")
  if(!success.clear)
    word_msg(
      "Failed restoring global settings to original state; you may want",
      "to restart your R session to ensure all global settings are in a",
      "reasonable state."
    )
  if(!success.unshim)
    word_msg(
      "Failed unshimming library/detach/attach; you may want to restart",
      "your R session to reset them to their original values (or you",
      "can `untrace` them manually)"
    )
  success.clear && success.unshim
}
#' Prompt to Quit if Enough Time Spent on Evaluation
#' @keywords internal

confirm_quit <- function(unitizers) {
  stopifnot(is(unitizers, "unitizerList"))
  if(
    length(unitizers) &&
    Reduce(`+`, lapply(as.list(unitizers), slot, "eval.time")) >
      unitizers[[1L]]@global$unitizer.opts[["unitizer.prompt.b4.quit.time"]]
  ) {
    word_cat("Are you sure you want to quit?")
    ui <- unitizer_prompt("Quit", valid.opts=c(Y="[Y]es", N="[N]o"))
    return(!identical(ui, "N"))
  }
  TRUE
}
