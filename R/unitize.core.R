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
#' @param preload.frame environment to use as parent frame, should contain pre
#'   loaded stuff if any, note that this environment is destructively modified
#'   by changing the parent environment, so do not re-use it (this environment
#'   should never be accessed outside of \code{unitizer} so this should be okay)
#' @keywords internal

unitize_core <- function(
  test.file, store.id, interactive.mode, par.env,
  search.path.clean, search.path.keep, force.update=FALSE,
  auto.accept=character(0L), preload.frame=new.env()
) {
  # -  Setup / Load ------------------------------------------------------------

  start.time <- proc.time()
  quit.time <- getOption("unitizer.prompt.b4.quit.time", 10)
  non.interactive <- getOption("unitizer.non.interactive", FALSE)  # need to rationalize this with `interactive.mode` param
  reset_packenv()                                                  # reset global vars used for search path manip

  if(!is.logical(interactive.mode) || length(interactive.mode) != 1L || is.na(interactive.mode))
    stop("Argument `interactive.mode` must be TRUE or FALSE")
  if(!is.logical(force.update) || length(force.update) != 1L || is.na(force.update))
    stop("Argument `force.update` must be TRUE or FALSE")

  if(is.null(test.file) && is.null(store.id))
    stop(
      "Logic Error: `test.file` and `store.id` cannot both be NULL; contact ",
      " maintainer."
    )
  if(!is.null(test.file)) {   # unitize
    if(
      !is.character(test.file) || length(test.file) != 1L ||
      !file_test("-f", test.file)
    )
      stop("Argument `test.file` must be a valid path to a file")
    u.name <- basename(test.file)
  } else {
    if(is.character(store.id) && length(store.id) == 1L) {
      u.name <- store.id
    } else {
      u.name <- if(is(x, "unitizer")) x@id else x
      u.name <- try(as.character(u.name), silent=TRUE)
      if(inherits(u.name, "try-error")) u.name <- "<unknown>"
  } }
  if(is.null(store.id)) {
    store.id <- if(!grepl("\\.[rR]$", test.file)) {
      paste0(test.file, ".unitizer")
    } else {
      sub("\\.[rR]$", ".unitizer", test.file)
  } }
  print(H1(paste0("unitizer for: ", u.name, collapse="")))

  if(!is.numeric(quit.time) || length(quit.time) != 1L || quit.time < 0)
    stop("Logic Error: unitizer option `unitizer.prompt.b4.quit.time` is miss-specified")
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
  if(!is.environment(preload.frame))
    stop(
      "Logic Error: `preload.frame` should be an environment; this is an ",
      "internal error, contact maintainer."
    )

  # Retrieve or create unitizer environment (note that the search path trimming)
  # happens later.  Also note that pack.env$zero.env can still be tracking the
  # top package under .GlobalEnv

  over_print("Loading unitizer data...")
  gpar.frame <- if(is.null(par.env)) pack.env$zero.env.par else par.env
  parent.env(preload.frame) <- gpar.frame

  if(is(store.id, "unitizer")) {
    unitizer <- upgrade(store.id, preload.frame)   # note zero.env is set-up further down
    store.id <- unitizer@id
  } else {
    unitizer <- try(load_unitizer(store.id, preload.frame))
    if(inherits(unitizer, "try-error")) stop("Unable to load `unitizer`; see prior errors.")
  }
  if(!is(unitizer, "unitizer")) stop("Logic Error: expected a `unitizer` object; contact maintainer.")

  # Make sure not running inside withCallingHandlers / withRestarts / tryCatch
  # or other potential issues; of course this isn't foolproof if someone is using
  # a variation on those functions, but also not the end of the world if it isn't
  # caught.

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

  # Setup the new unitizer

  unitizer@id <- store.id
  parent.env(unitizer@zero.env) <- preload.frame
  assign("quit", unitizer_quit, unitizer@zero.env)
  assign("q", unitizer_quit, unitizer@zero.env)

  wd <- getwd()                              # in case user changes it through tests
  on.exit(                                   # In case interrupted or some such
    message(
      "Unexpectedly exited before storing `unitizer`; ",
      "tests were not saved or changed."
    ),
    add=TRUE
  )
  # Clean up search path

  search.path.setup <- search.path.trim <- FALSE
  if((is.null(par.env) || isTRUE(search.path.clean)) && !tracingState()) {
    warning(
      "Tracing is disabled, but must be enabled to run in a clean environment ",
      "or with a clean search path.  If you want these features re-enable tracing ",
      "with `tracingState(TRUE)`.  See \"Reproducible Tests\" vignette for details.  ",
      "Running on existing search path with `.GlobalEnv` as parent.",
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
      on.exit(search_path_restore(), add=TRUE) # note this also runs search_path_unsetup()
    }
    on.exit(search_path_unsetup(), add=TRUE)
  }

  # -  Parse / Eval ------------------------------------------------------------

  # Parse and evaluate test file, but only if we're in `unitize` mode, as implied
  # by the `test.file`

  search.path.restored <- FALSE
  if(!is.null(test.file)) {
    over_print("Parsing tests...")
    tests.parsed <- parse_tests(test.file, comments=interactive.mode)

    if(!length(tests.parsed)) {
      over_print("")
      message("No tests in ", test.file, "; nothing to do here.")
      on.exit(NULL)
      if(search.path.trim) search_path_restore()        # runs _unsetup() as well
      else if (search.path.setup) search_path_unsetup()

      return(invisible(unitizer))
    }
    # Evaluate the parsed calls

    tests <- new("unitizerTests") + tests.parsed
    unitizer <- unitizer + tests

    # Make sure our tracing didn't get messed up in some way

    if((isTRUE(search.path.clean) || is.null(par.env)) && !search_path_check()) {
      search_path_restore()
      search.path.restored <- TRUE
    }
    # Summary view of deltas and changes

    unitizer.summary <- summary(unitizer)
    cat("\n")

    if(!interactive.mode) {
      if(!passed(unitizer.summary)) {  # Passed tests are first column
        delta.show <- unitizer@tests.status != "Pass" & !ignored(unitizer@items.new)
        message(
          paste0(
            format(paste0(unitizer@tests.status[delta.show], ": ")),
            unitizer@items.new.calls.deparse[delta.show],
            collapse="\n"
          ),
          "\n"
        )
        stop(
          "Newly generated tests do not match unitizer (",
          paste(
            c(colnames(unitizer.summary@data), "Deleted"),
            c(tail(unitizer.summary@data, 1L), unitizer.summary@dels),
            sep=": ", collapse=", "
          ),
          "); see above for more info, or run in interactive mode"
        )
      }
      message("Passed Tests")
      on.exit(NULL)
      if(!search.path.restored) {
        if(search.path.trim) search_path_restore()        # runs _unsetup() as well
        else if (search.path.setup) search_path_unsetup()
      }
      return(invisible(unitizer))
    }
  }
  cat("\r")

  # -  Browse ------------------------------------------------------------------

  # Group tests by section and outcome for review; note that the `unitizer.browse`
  # object carries info about what mode it is in and that is used by subsequent
  # functions

  if(is.null(test.file)) {
    unitizer.browse <- browsePrep(unitizer, mode="review")
  } else {
    unitizer.browse <- browsePrep(unitizer, mode="unitize")
  }
  tot.time <- (proc.time() - start.time)[["elapsed"]]

  # Decide what to keep / override / etc.

  if(length(auto.accept)) {  # Apply auto-accepts, if any
    for(auto.val in auto.accept) {
      auto.type <- which(
        tolower(unitizer.browse@mapping@review.type) == auto.val
      )
      unitizer.browse@mapping@review.val[auto.type] <- "Y"
      unitizer.browse@mapping@reviewed[auto.type] <- TRUE
  } }
  # Now manual accepts

  unitizer <- browseUnitizer(
    unitizer, unitizer.browse, prompt.on.quit=tot.time > quit.time && !length(auto.accept),
    force.update=force.update
  )
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
