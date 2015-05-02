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
  test.files, store.ids, interactive.mode, par.env,
  search.path.clean, search.path.keep, force.update,
  auto.accept, pre.load, mode
) {
  # - Validation / Setup -------------------------------------------------------

  if(
    !is.character(mode) || length(mode) != 1L ||
    !mode %in% c("unitize", "review")
  )
    stop("Logic Error: incorrect value for `mode`; contact maintainer")
  if(mode == "review") {
    if(!all(is.na(test.files)))
      stop(
        "Logic Error: `test.files` must be NA in review; contact maintainer"
      )
    if(length(auto.accept))
      stop("Logic Error: auto-accepts not allowed in review mode")
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
    norm.attempt <-
      normalizePath(test.files <- normalizePath(test.files), mustWork=TRUE)
    if(inherits(norm.attempt, "try-error"))
      stop("Logic Error: unable to normalize test files; contact maintainer")
  }
  if(length(test.files) != length(store.ids))
    stop(
      "Logic Error: mismatch in test file an store lengths; contact maintainer"
    )
  if(!length(test.files))
    stop("Logic Error: expected at least one test file; contact maintainer.")
  if(
    !is.logical(interactive.mode) || length(interactive.mode) != 1L ||
    is.na(interactive.mode)
  )
    stop("Argument `interactive.mode` must be TRUE or FALSE")
  if(
    !is.logical(force.update) || length(force.update) != 1L ||
    is.na(force.update)
  )
    stop("Argument `force.update` must be TRUE or FALSE")
  if(!is.null(par.env) && !is.environment(par.env))
    stop("Argument `par.env` must be NULL or an environment.")
  if(
    !is.logical(search.path.clean) || length(search.path.clean) != 1L ||
    is.na(search.path.clean)
  )
    stop("Argument `search.path.clean` must be TRUE or FALSE.")
  if(!is.character(search.path.keep))
    stop("Argument `search.path.keep` must be character()")
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
  if(length(auto.accept) && (!interactive.mode))
    stop("Argument `auto.accept` must be empty in non-interactive mode")
  if(length(auto.accept) && is.null(test.file))
    stop("Argument `test.file` must be specified when using `auto.accept`")
  if(
    !is.null(pre.load) && !identical(pre.load, FALSE) && !is.list(pre.load) &&
    !is.character(pre.load) && length(pre.load) != 1L
  )
    stop("Argument `pre.load` must be NULL, FALSE, a list, or character(1L)")
  if(is.null(pre.load)) {
     pre.load  <- if(length(test.files))
      file.path(dirname(test.files[[1L]]), "helper") else list()
  }
  quit.time <- getOption("unitizer.prompt.b4.quit.time", 10)
  if(!is.numeric(quit.time) || length(quit.time) != 1L || quit.time < 0)
    stop(
      "Logic Error: unitizer option `unitizer.prompt.b4.quit.time` ",
      "is miss-specified"
    )
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
      "In order to proceed `unitizer` must create the following ", dir.word,
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

  norm.attempt < try(
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
  # reset global vars used for search path manip

  reset_packenv()

  # - Parse / Load -------------------------------------------------------------

  # Parse, and use `eval.which` to determine which tests to evaluate

  if(identical(mode, "unitize")) {
    over_print("Parsing tests...")
    tests.parsed <- lapply(
      test.files,
      function(x) {
        over_print(paste("Parsing", x))
        parse_tests(x, comments=interactive.mode)
  } ) }
  # Clean up search path

  search.path.setup <- search.path.trim <- FALSE
  if((is.null(par.env) || isTRUE(search.path.clean)) && !tracingState()) {
    warning(
      "Tracing is disabled, but must be enabled to run in a clean environment ",
      "or with a clean search path.  If you want these features re-enable ",
      "tracing with `tracingState(TRUE)`.  See \"Reproducible Tests\" ",
      "vignette for details.  Running on existing search path with ",
      "`.GlobalEnv` as parent.",
      immediate.=TRUE
    )
    par.env <- .GlobalEnv
    search.path.clean <- FALSE
  } else if(is.null(par.env) || isTRUE(search.path.clean)) {
    over_print("Search Path Setup...")
    if(!isTRUE(search.path.setup <- search_path_setup())) {
      if(is.null(par.env))
        warning(
          "Unable to run in clean environment, running in .GlobalEnv",
          immediate.=TRUE
        )
      if(isTRUE(search.path.clean))
        warning(
          "Unable to run with clean search path; using existing.",
          immediate.=TRUE
        )
    } else {
      on.exit(search_path_unsetup(), add=TRUE)
    }
  }
  if(isTRUE(search.path.clean)) {
    if(isTRUE(search.path.trim <- search_path_trim(keep=search.path.keep))) {
      on.exit(search_path_restore(), add=TRUE) # this also runs search_path_unsetup()
    }
    on.exit(search_path_unsetup(), add=TRUE)
  }
  # Load / create all the unitizers

  # Retrieve or create unitizer environment

  gpar.frame <- if(is.null(par.env)) pack.env$zero.env.par else par.env

  # Handle pre-load data

  over_print("Preloads...")
  pre.load.frame <- try(pre_load(pre.load, gpar.frame))
  if(inherits(pre.load.frame, "try-error") || !is.environment(pre.load.frame))
    stop("Argument `pre.load` could not be interpreted")

  util.frame <- new.env(parent=pre.load.frame)
  assign("quit", unitizer_quit, util.frame)
  assign("q", unitizer_quit, util.frame)

  over_print("Loading unitizer data...")
  eval.which <- seq_along(store.ids)
  unitizers <- new("unitizerObjectList")

  # - Evaluate / Browse --------------------------------------------------------

  check_call_stack()  # Make sure nothing untoward will happen if a test triggers an error

  while(
    length(eval.which) || mode == "review"
  ) {
    # Load unitizers

    unitizers[eval.which] <- unitize_load(
      store.ids[eval.which], test.files[eval.which], util.frame, mode
    )
    # Now evaluate, whether a `unitizer` is evaluated or not is a function of
    # the slot @eval, set just above as they are loaded

    unitizers <- unitize_eval(tests.parsed=tests.parsed, unitizers=unitizers)

    # Gather user input, and store tests as required.  Any `unitizer`s that
    # the user marked for re-evaluation will be re-evaluated in this loop

    unitizers <- unitize_browse(
      unitizers=unitizers,
      mode=mode,
      interactive.mode=interactive.mode,
      force.update=force.update,
      auto.accept=auto.accept
    )
    eval.which <- which(vapply(as.list(unitizers), slot, logical(1L), "eval"))
  }
  # - Finalize -----------------------------------------------------------------

  on.exit(NULL)
  if(search.path.trim) search_path_restore()        # runs _unsetup() as well
  else if (search.path.setup) search_path_unsetup()
  return(as.list(unitizers))
}
#' Load Unitizers
#'
#' @keywords internal

unitize_load <- function(store.ids, test.files, frame, mode) {
  unitizers <- new(
    "unitizerObjectList",
    .items=lapply(
      seq_along(store.ids),
      function(i) {
        if(is(store.ids[[i]], "unitizer")) {
          unitizer <- upgrade(store.ids[[i]], frame, test.files[[i]])
          store.ids[[i]] <- unitizer@id
        } else {
          unitizer <- try(
            load_unitizer(store.ids[[i]], frame, test.files[[i]])
          )
          if(inherits(unitizer, "try-error"))
            stop("Unable to load `unitizer`; see prior errors.")
        }
        if(!is(unitizer, "unitizer"))
          stop("Logic Error: expected a `unitizer` object; contact maintainer.")
        unitizer@eval <- identical(mode, "unitize") #awkward, shouldn't be done this way
        unitizer
  } ) )
}

#' Evaluate User Tests
#'
#' @param tests.parsed a list of expressions
#' @param unitizers a list of \code{unitizer} objects of same length as \code{tests.parsed}
#' @param which integer which of \code{unitizer}s to actually eval, all get
#'   summary status displayed to screen
#' @return a list of unitizers
#' @keywords internal

unitize_eval <- function(tests.parsed, unitizers) {
  on.exit(                                   # In case interrupted or some such
    message(
      "Unexpectedly exited before storing `unitizer`; tests were not saved or ",
      "changed."
    )
  )
  test.len <- length(tests.parsed)
  if(!identical(test.len, length(unitizers)))
    stop(
      "Logic Error: parse data and unitizer length mismatch; contact ",
      "maintainer."
    )
  # Loop through all unitizers, evaluating the ones that have been marked with
  # the `eval` slot for evaluation, and resetting that slot to FALSE

  for(i in seq.int(test.len)) {
    test.dat <- tests.parsed[[i]]
    unitizer <- unitizers[[i]]

    if(unitizer@eval) {
      tests <- new("unitizerTests") + test.dat
      unitizers[[i]] <- unitizer + tests
      if(test.len > 1L)
        over_print(paste0("Evaluated: ", unitizer@test.file.loc, "\n"))
    } else {
      unitizers[[i]] <- unitizer
    }
    unitizers[[i]]@eval <- FALSE
  }
  on.exit()
  unitizers
}
#' Run User Interaction And \code{unitizer} Storage
#'
#' @inheritParams unitize_core
#' @param unitizers list of \code{unitizer} objects
#' @param prompt.on.quit logical(1L) wh
#' @param force.update whether to store unitizer
#'

unitize_browse <- function(
  unitizers, mode, interactive.mode, force.update, auto.accept
) {
  # - Prep ---------------------------------------------------------------------

  if(!length(unitizers)) {
    message("No tests to review")
    return(unitizers)
  }
  over_print("Prepping Unitizers...")
  hist.obj <- history_capt()
  on.exit(history_release(hist.obj))

  # Get summaries

  test.len <- length(unitizers)
  summaries <- summary(unitizers, silent=TRUE)
  totals <- vapply(as.list(summaries), slot, summaries[[1L]]@totals, "totals")
  to.review <- colSums(totals[-1L, , drop=FALSE]) > 0L  # First row will be passed

  # Determine implied review mode (all tests passed in a particular `unitizer`,
  # but user may still pick it to review)

  review.mode <- ifelse(!to.review & test.len > 1L, "review", mode)
  untz.browsers <- mapply(
    browsePrep, as.list(unitizers), review.mode,
    MoreArgs=list(hist.con=hist.obj$con), SIMPLIFY=FALSE
  )
  # Decide what to keep / override / etc.
  # Apply auto-accepts, if any (shouldn't be any in "review mode")

  # NOTE: are there issues with auto-accepts when we run this function more
  # than once, where previous choices are over-written by the auto-accepts?
  # maybe auto-accepts only get applied first time around?

  auto.accepted <- 0L

  if(length(auto.accept)) {
    over_print("Applying auto-accepts...")
    for(i in seq_along(untz.browsers)) {
      for(auto.val in auto.accept) {
        auto.type <- which(
          tolower(untz.browsers[[i]]@mapping@review.type) == auto.val
        )
        untz.browsers[[i]]@mapping@review.val[auto.type] <- "Y"
        untz.browsers[[i]]@mapping@reviewed[auto.type] <- TRUE
        auto.accepted <- auto.accepted + length(auto.type)
  } } }

  # - Non-interactive ----------------------------------------------------------

  # Browse, or fail depending on interactive mode

  reviewed <- updated <- logical(test.len)
  over_print("")
  if(!interactive.mode) {
    if(any(to.review)) {
      for(i in which(to.review)) {
        untz <- unitizers[[i]]
        delta.show <- untz@tests.status != "Pass" & !ignored(untz@items.new)
        message(
          paste0(
            "* ",
            format(paste0(untz@tests.status[delta.show], ": ")),
            untz@items.new.calls.deparse[delta.show],
            collapse="\n"
          ),
          "in test file '", untz@test.file.loc, "'\n",
      ) }
      stop(
        "Newly generated tests do not match unitizer (",
        paste(
          names(summaries@totals), summaries@totals, sep=": ", collapse=", "
        ),
        "); see above for more info, or run in interactive mode"
      )
    }
  } else {
  # - Interactive --------------------------------------------------------------

    if(test.len > 1L) show(summaries)
    if(identical(mode, "review") || any(to.review)) {
      # We have fairly different treatment for a single test versus multi-test
      # review, so the logic gets a little convoluted (keep eye out for)
      # `test.len > 1L`, but this obviates the need for multiple different calls
      # to `browseUnitizers`

      prompt <- paste0(
        "\nType number of unitizer to review",
        if(any(to.review))
          ", 'A' to review all that require review",
        if(any(updated))
          ", 'R' to re-evaluate all updated"
      )
      repeat {
        eval.which <- integer(0L)
        updated <- vapply(as.list(unitizers), slot, logical(1L), "updated")

        if(test.len > 1L) {
          pick.num <- integer()
          pick <- unitizer_prompt(
            "Pick a `unitizer` or an option",
            valid.opts=c(A="[A]ll", "[R]e-eval"),
            exit.condition=exit_fun, valid.vals=seq.int(test.len),
            hist.con=hist.obj$con
          )
          if(identical(pick, "Q")) {
            if(
              Reduce(`+`, lapply(as.list(unitizers), slot, "eval.time")) >
              getOption("unitizer.prompt.b4.quit.time", 10)
            ) {
              ui <- try(simple_prompt("Are you sure you want to quit?"))
              if(identical(ui, "N")) next
            }
            break
          } else if(identical(pick, "A")) {
            pick.num <- which(to.review & !updated)
          } else if(identical(pick, "R")) {
            eval.which <- which(updated)
          } else {
            pick.num <- as.integer(pick)
            if(!pick.num %in% seq.int(test.len)) {
              word_msg(
                "Input not a valid `unitizer`; choose in ",
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
          show(summaries[[i]])
          cat("\n")
          browse.res <- browseUnitizer(
            unitizers[[i]], untz.browsers[[i]],
            force.update=force.update,  # annoyingly we need to force update here as well as for the unreviewed unitizers
          )
          updated[[i]] <- browse.res@updated
          unitizers[[i]] <- browse.res@unitizer

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
        if(identical(test.len, 1L) || length(eval.which)) break
        summaries <- summary(unitizers)
      }
    } else {
      message("All tests passed; nothing to review.")
    }
    # Set eval status before return

    if(length(eval.which)) {
      for(i in eval.which) unitizers[[i]]@eval <- TRUE
    } else for(i in seq_along(unitizers))  unitizers[[i]]@eval <- FALSE  # this one may not be necessary

    # Force update stuff if needed; need to know what has already been stored

    if(any(force.update & !to.review & !updated)) {
      stop("Need to implement force for non-review tests")
    }
  }
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
    "discouraged as it may cause unpredictable behavior from `unitizer` in the ",
    "event tests produce conditions / errors.  We strongly recommend you re-run ",
    "your tests outside of such handling functions.", immediate.=TRUE
  )
  restarts <- computeRestarts()
  restart.names <- vapply(restarts, `[[`, character(1L), 1L)
  if("unitizerQuitExit" %in% restart.names)
    stop(
      "`unitizerQuitExit` restart is already defined; `unitizer` relies on this ",
      "restart to restore state prior to exit, so `unitizer` will not run if it is ",
      "defined outside of `unitize`.  If you did not define this restart contact ",
      "maintainer."
    )
}
