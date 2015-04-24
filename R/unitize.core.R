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

# break up into several pieces
# - validate
# - load
# - parse
# - eval
# - browse
# - store?
#
# Pieces should be separate:
# * The eval piece should be separate so that failures on anything else can be
#   managed with `try-catch`.
# * The validate piece also so it only has to be run once.
#
# Use the same logic to `unitize` one file vs many; basically, one file is just
# a degenerate version of dir.
#
# Do we want to load all the unitzers at once? Potentially end up using a lot
# of memory if we do it this way.  But if we don't it will be difficult to do
# this in a way that survives the failure of a single unitizer.  Maybe we need
# a "one-at-a-time" mode for large unitizers.
#
# What structure allows us to split up eval, and browse, but also re-eval from
# browse as needed?  Can browse call `eval`, or do we keep getting deeper and
# deeper into this two step recursion?  Maybe best way to handle is some
# restart with instructions on what to re-run?  So all this would be
# coordinated from the wrapper function?  Do we even need a restart?  We can
# just return early with instructions.
#
# Browse should be able to write to disk as otherwise failure elsewhere could
# lead to a lot of lost work

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
  if(
    mode == "unitize" &&
    (
      !is.character(test.files) || any(is.na(test.files)) ||
      !all(file_test("-f", test.files))
    )
  )
    stop(
      "Logic Error: `test.files` must all point to valid files in unitize ",
      "mode; contact maintainer"
    )
  if(length(test.files) != length(store.ids))
    stop(
      "Logic Error: mismatch in test file an store lengths; contact maintainer"
    )
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

  pre.load.frame <- try(pre_load(pre.load))
  if(inherits(pre.load.frame, "try-error"))
    stop("Argument `pre.load` could not be interpreted; see previous errors")

  start.time <- proc.time()
  quit.time <- getOption("unitizer.prompt.b4.quit.time", 10)
  if(!is.numeric(quit.time) || length(quit.time) != 1L || quit.time < 0)
    stop(
      "Logic Error: unitizer option `unitizer.prompt.b4.quit.time` ",
      "is miss-specified"
    )
  # reset global vars used for search path manip

  reset_packenv()

  # - Parse / Load -------------------------------------------------------------

  # Parse, and use `eval.which` to determine which tests to evaluate

  if(mode == "unitize") {
    over_print("Parsing tests...")
    tests.parsed <- lapply(
      test.files,
      function(x) {
        over_print(paste("Parsing", x))
        parse_tests(x, comments=interactive.mode)
    } )
    eval.which <- seq_along(test.files)  # first pass eval all the unitizers (duh)
  } else {
    eval.which <- integer()
  }
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

  over_print("Loading unitizer data...")
  gpar.frame <- if(is.null(par.env)) pack.env$zero.env.par else par.env
  parent.env(pre.load.frame) <- gpar.frame

  util.frame <- new.env(parent.frame=pre.load.frame)
  assign("quit", unitizer_quit, util.frame)
  assign("q", unitizer_quit, util.frame)

  stop("Make Sure unitizer location is recorded")
  unitizers <- new(
    "unitizerObjectList",
    .items=lapply(
      seq_along(store.ids),
      function(i) {
        if(is(store.ids[[i]], "unitizer")) {
          unitizer <- upgrade(store.ids[[i]], util.frame)
          store.ids[[i]] <- unitizer@id
        } else {
          unitizer <- try(
            load_unitizer(store.ids[[i]], util.frame, test.files[[i]])
          )
          if(inherits(unitizer, "try-error"))
            stop("Unable to load `unitizer`; see prior errors.")
        }
        if(!is(unitizer, "unitizer"))
          stop("Logic Error: expected a `unitizer` object; contact maintainer.")
        unitizer
  } ) )
  # - Evaluate / Browse --------------------------------------------------------

  # Now evaluate

  wd <- getwd()                              # in case user changes it through tests

  check_call_stack()  # Make sure nothing untoward will happen if a test triggers an error

  while(length(eval.which) || mode == "review") {

    # Evaluate and display `unitizer` status

    unitizers <- unitize_eval(
      tests.parsed=tests.parsed, unitizers=unitizers, eval.which=eval.which
    )
    # Make sure our tracing didn't get messed up in some way
    stop("need to handle this")

    # if((isTRUE(search.path.clean) || is.null(par.env)) && !search_path_check()) {
    #   search_path_restore()
    #   search.path.restored <- TRUE
    # }
    # Handle non-interactive mode


    # `eval.which` tells us what unitizes we need to re-eval if we made changes
    # to our source code, but more than that would be good to be able to book-
    # mark a particular test?  Becomes complicated because that might mean there
    # are a bunch of unreviewed tests, etc.

    eval.which <- unitize_browse(
      unitizers=unitizers,               # make sure file path is included in \code{unitizer} so browse can update file
      mode=mode,
      interactive.mode=interactive.mode,
      force.update=force.update,
      auto.accept=auto.accept            # need to think about how auto-accept is done here; maybe should even be done in eval mode or as separate function
      prompt.on.quit=tot.time > quit.time
    )
  }
  # - Finalize -----------------------------------------------------------------

  message("Passed Tests")
  on.exit(NULL)
  if(search.path.trim) search_path_restore()        # runs _unsetup() as well
  else if (search.path.setup) search_path_unsetup()
}
#' Evaluate User Tests
#'
#' @param tests.parsed a list of expressions
#' @param unitizers a list of \code{unitizer} objects of same length as \code{tests.parsed}
#' @param which integer which of \code{unitizer}s to actually eval, all get
#'   summary status displayed to screen
#' @return a list of unitizers
#' @keywords internal

unitize_eval <- function(tests.parsed, unitizers, eval.which) {
  on.exit(                                   # In case interrupted or some such
    message(
      "Unexpectedly exited before storing `unitizer`; tests were not saved or ",
      "changed."
    )
  )
  if(
    !identical(test.len, length(unitizers)) || !is.integer(eval.which) ||
    any(is.na(eval.which) || any(eval.which < 1L)  || any(eval.which > test.len)
  )
    stop(
      "Logic Error: parse data and unitizer length mismatch; contact ",
      "maintainer."
    )
  # Loop through all unitizers, evaluating each

  res <- vector("list", test.len)

  # Run through unitizers

  for(i in seq.int(test.len)) {
    test.dat <- tests.parsed[[i]]
    unitizer <- unitizers[[i]]

    # Evaluate the parsed calls

    if(i %in% eval.which) {
      tests <- new("unitizerTests") + test.dat
      res[[i]] <- unitizer + tests
    } else {
      res[[i]] <- unitizer
    }
    over_print(paste0("Completed: ", unitizer@test.file.loc, "\n"))
  }
  on.exit()
  res
}
#' Run User Interaction And \code{unitizer} Storage
#'
#' @inheritParams unitize_core
#' @param unitizers list of \code{unitizer} objects
#' @param prompt.on.quit logical(1L) wh
#' @param force.update whether to store unitizer
#'

unitize_browse <- function(
  unitizers, mode, force.update, auto.accept, prompt.on.quit
) {
  # - Prep ---------------------------------------------------------------------

  eval.which <- integer()  # default is to not re-eval anything

  if(!length(unitizers)) {
    message("No tests to review")
    return(eval.which)
  }
  over_print("Prepping Unitizers...")
  untz.browsers <- lapply(as.list(unitizers), browsePrep, mode=mode)

  # Decide what to keep / override / etc.
  # Apply auto-accepts, if any (shouldn't be any in "review mode")

  # NOTE: are there issues with auto-accepts when we run this function more
  # than once, where previous choices are over-written by the auto-accepts?
  # maybe auto-accepts only get applied first time around?

  warning("Sort out iterative auto-accept issue", immediate.=TRUE)

  test.len <- length(untizers)
  to.review <- integer(test.len)
  auto.accepted <- 0L

  if(length(auto.accept)) {
    over_print("Applying auto-accepts...")
    for(i in seq_along(untz.browsers)) {
      untz.browsers[[i]] <- untz.browsers[[i]]
      for(auto.val in auto.accept) {
        auto.type <- which(
          tolower(untz.browsers[[i]]@mapping@review.type) == auto.val
        )
        untz.browsers[[i]]@mapping@review.val[auto.type] <- "Y"
        untz.browsers[[i]]@mapping@reviewed[auto.type] <- TRUE
        auto.accepted <- auto.accepted + length(auto.type)
    } }
    to.review[[i]] <- sum(
      !untz.browsers[[i]]@mapping@reviewed & !untz.browsers[[i]]@mapping@ignored
    )
  }
  # List the result

  summaries <- summary(unitizers, silent=TRUE)

  # - Non-interactive ----------------------------------------------------------

  # Browse, or fail depending on interactive mode

  if(!interactive.mode) {
    if(!sum(to.review)) {
      for(i in which(to.review > 0L)) {
        untz <- unitizers[[i]]
        delta.show <- untz@tests.status != "Pass" & !ignored(untz@items.new)
        message(
          paste0(
            "* "
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
    if(identical(mode, "review") || sum(to.review)) {
      # We have fairly different treatment for a single test versus multi-test
      # review, so the logic gets a little convoluted (keep eye out for)
      # `test.len > 1L`, but this obviates the need for multiple different calls
      # to `browseUnitizers`

      prompt <- paste0(
        "Type number of unitizer to review, or 'A' to review all that require ",
        "review (those with '*' ahead of their number)"
      )
      reviewed <- logical(test.len)
      repeat {
        if(test.len > 1L) {
          pick <- try(
            simple_prompt(prompt, c("A", "Q", seq.int(test.len)), attempts=10L)
          )
          if(inherits(pick, "try-error")) {
            message(
              "Error occurred while waiting for unitizer selection, aborting"
            )
            break
          } else if(identical(pick, "Q")) {
            stop("INTERNAL: need to handle unreviewed unitizers?")
          } else if(identical(pick, "A")) {
            stop("Need Review-all mode")
          } else {
            pick.num <- as.integer(pick)
            if(!pick.num %in% seq.int(test.len))
              stop(
                "Logic Error: invalid unitizer selected somehow; contact ",
                "maintainer."
              )
        } }
        # `browseUnitizer` returns `unitizer`, along with

        stop("update unitizer to return Re-eval requests")

        browse.res <- browseUnitizer(
          unitizers[[pick.num]], untz.browsers[[pick.num]],
          force.update=force.update  # annoyingly we need to force update here as well as for the unreviewed unitizers
        )
        unitizers[[pick.num]] <- browse.res$unitizer # can't be Fd creating a new class for return val
        reviewed[[pick.num]] <- TRUE

        if(!is.null(browse.res$reeval)) {
          eval.which <- if(identical(browse.res$reeval, "R")) {
            pick.num
          } else if(identical(browse.res$reeval, "RR")) {
            seq.int(test.len)
          } else stop("Logic Error: invalid re-eval value; contact maintainer.")
          break
        }
        if(identical(test.len, 1L)) break else show(summaries)
      }
    }
    # Force update stuff if needed; need to know what has already been stored

    if(!to.review && !auto.accepted) {
      message("All tests passed")
    } else if (!to.review) {
      message("All tests passed or auto-accepted")
    } else {

    }
  }





  print(H1(paste0("unitizer for: ", getName(unitizer), collapse="")))





  # Now manual accepts
  print(H1(paste0("unitizer for: ", u.name, collapse="")))

  # -  Finalize ------------------------------------------------------------------

  # Restore search path

  if(!search.path.restored) {
    if(search.path.trim) search_path_restore()          # runs _unsetup() as well
    else if (search.path.setup) search_path_unsetup()
  }
  # Finalize

  store_unitizer(unitizer, store.id, wd)
  on.exit(NULL)
  invisible(unitizer)
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

