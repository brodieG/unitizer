#' Unitize an R Test Script
#'
#' Turns a standard R script into unit tests by evaluating the expressions and
#' storing them along with their resuls.  Re-running \code{`unitize`} then
#' checks that the values remain unchanged.
#'
#' You can run \code{`unitize`} from the command line, or you can place one or
#' more \code{`unitize`} calls in an R file and source that.
#'
#' \code{`review`} allows you to review the contents of an existing
#' \code{`unitizer`} throught the interactive \code{`unitizer`} interaface.
#'
#' See "unitizer" vignette and demos for details and examples.
#'
#' @export
#' @aliases review
#' @seealso \code{`\link{get_store}`}
#' @param test.file path to the file containing tests
#' @param store.id a folder to store the \code{`unitizer`} objects in; will auto-
#'   generate to a folder at the same location as the test file with the same
#'   name as the testfile, except ending in \code{`.unitizer`} instead of
#'   \code{`.R`}.  This is the default option, you can create custom
#'   \code{`unitizer`} stores as well (see vignette and \code{`\link{get_store}`}).
#' @param x for \code{`review`} only, either a \code{`unitizer`} or something that,
#'   when passed to \code{`\link{get_store}`}, will retrieve a unitizer (i.e.
#'   equivalent to what would get passed in \code{`store.id`}).
#' @param interactive.mode logical(1L) whether to run in interactive mode
#' @param env.clean TRUE or environment, if TRUE tests are run in a clean
#'   environment, if an environment they are run with that environment as the
#'   parent environment.
#' @param search.path.clean logical(1L) if TRUE all items on the search path that
#'   are not part of a clean R session are detached prior to running tests.  Note
#'   namespaces for detached packages remain loaded.  Additionally, the search
#'   path is restored to its initial state upon exiting \code{`unitizer`} so any
#'   packages added/removed, or objects attached/detached from search path are
#'   restored to original state.  This feature is somewhat experimental and is
#'   disabled by default, though this will likely change in the future.  See
#'   "Reproducible Tests" vignette for details.
#' @param search.path.keep character any additional items on the search path
#'   to keep attached; has no effect unless \code{`search.path.clean`} is TRUE
#' @return the \code{`unitizer`} object, invisibly.  If running in interactive
#'   mode, then the returned \code{`unitizer`} will be modified as per user
#'   input in the interactive session.

unitize <- function(
  test.file, store.id=sub("\\.[Rr]$", ".unitizer", test.file),
  interactive.mode=interactive(), env.clean=TRUE,
  search.path.clean=getOption("unitizer.search.path.clean"),
  search.path.keep=c("tools:rstudio", "package:unitizer")
) {
  if(!is.character(test.file) || length(test.file) != 1L || !file_test("-f", test.file))
    stop("Argument `test.file` must be a valid path to a file")
  if(!is.logical(interactive.mode) || length(interactive.mode) != 1L || is.na(interactive.mode))
    stop("Argument `interactive.mode` must be TRUE or FALSE")

  print(H1(paste0("unitizer for: ", test.file, collapse="")))
  invisible(
    unitizer_core(
      test.file, store.id, interactive.mode, env.clean, search.path.clean,
      search.path.keep
  ) )
}
#' @export

review <- function(
  x, env.clean=TRUE, search.path.clean=getOption("unitizer.search.path.clean"),
  search.path.keep=c("tools:rstudio", "package:unitizer")
) {
  u.name <- if(is.character(x) && length(x) == 1L) {
    x
  } else {
    u.name <- if(is(x, "unitizer")) x@id else x
    u.name <- try(as.character(u.name), silent=TRUE)
    if(inherits(u.name, "try-error")) u.name <- "<unknown>"

  }
  print(H1(paste0("unitizer for: ", u.name, collapse="")))
  invisible(
    unitizer_core(
      test.file=NULL, store.id=x, interactive.mode=TRUE, env.clean=env.clean,
      search.path.clean=search.path.clean, search.path.keep=search.path.keep
    )
  )
}
#' Runs The Basic Stuff
#'
#' Used by both \code{`\link{unitize}`} and \code{`\link{review,unitizer-method}`}
#' to launch the interactive interface for reviewing tests.
#'
#' Right now we distinguish in what mode we're running based on whether
#' \code{`test.file`} is NULL (review mode) vs. not (unitize mode), which isn't
#' very elegant, but whatevs.  This has implications for the parsing / evaluation
#' step, as well as how the \code{`unitizerBrowse`} object is constructed.  Otherwise
#' stuff is mostly the same.
#'
#' Cleary there is a trade-off in increased code complexity to handle both types
#' of code, vs duplication.  Not ideal, but tasks are so closely related and
#' there is so much common overhead, that the central function makes sense.
#' Also, since unfortunately we're relying on side-effects for some features, and
#' `on.exit` call for safe operation, it is difficult to truly modularize.
#'
#' @keywords internal

unitizer_core <- function(
  test.file, store.id, interactive.mode, env.clean,
  search.path.clean, search.path.keep
) {
  # -  Setup / Load ------------------------------------------------------------

  start.time <- proc.time()
  quit.time <- getOption("unitizer.prompt.b4.quit.time", 10)
  non.interactive <- getOption("unitizer.non.interactive", FALSE)  # need to rationalize this with `interactive.mode` param
  reset_packenv()                                                  # reset global vars used for search path manip

  if(!is.numeric(quit.time) || length(quit.time) != 1L || quit.time < 0)
    stop("Logic Error: unitizer option `unitizer.prompt.b4.quit.time` is miss-specified")
  if(!is.logical(non.interactive) || length(non.interactive) != 1L)
    stop("Logic Error: unitizer option `unitizer.non.interactive` is miss-specified")
  if(!isTRUE(env.clean) && !is.environment(env.clean))
    stop("Argument `env.clean` must be TRUE or an environment.")
  if(
    !is.logical(search.path.clean) || length(search.path.clean) != 1L ||
    is.na(search.path.clean)
  )
    stop("Argument `search.path.clean` must be TRUE or FALSE.")
  if(!is.character(search.path.keep))
    stop("Argument `search.path.keep` must be character()")

  # Retrieve or create unitizer environment (note that the search path trimming)
  # happens later.  Also note that pack.env$zero.env can still be tracking the
  # top package under .GlobalEnv

  over_print("Loading unitizer data...")
  par.frame <- if(isTRUE(env.clean)) pack.env$zero.env.par else env.clean

  if(is(store.id, "unitizer")) {
    unitizer <- upgrade(store.id, par.frame)   # note zero.env is set-up further down
    store.id <- unitizer@id
  } else {
    unitizer <- try(load_unitizer(store.id, par.frame))
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
  parent.env(unitizer@zero.env) <- par.frame
  list2env(getItemFuns, unitizer@zero.env)     # functions for accessing unitizerItem contents
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
  if((isTRUE(env.clean) || isTRUE(search.path.clean)) && !tracingState()) {
    warning(
      "Tracing is disabled, but must be enabled to run in a clean environment ",
      "or with a clean search path.  If you want these features re-enable tracing ",
      "with `tracingState(TRUE)`.  See \"Reproducible Tests\" vignette for details.  ",
      "Running on existing search path with `.GlobalEnv` as parent.",
      immediate.=TRUE
    )
    env.clean <- .GlobalEnv
    search.path.clean <- FALSE
  } else if(isTRUE(env.clean) || isTRUE(search.path.clean)) {
    over_print("Search Path Setup...")
    if(!isTRUE(search.path.setup <- search_path_setup())) {
      if(isTRUE(env.clean))
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
    if(inherits(try(tests.parsed <- parse_with_comments(test.file)), "try-error")) {
      warning(
        "Unable to parse `test.file`; see prior error for details.  ",
        "Proceeding without comment parsing", immediate.=TRUE
      )
      if(inherits(try(tests.parsed <- parse(test.file)), "try-error"))
        stop("Could not parse `test.file`; see prior error for details.")
    }
    if(!length(tests.parsed)) {
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

    if((isTRUE(search.path.clean) || isTRUE(env.clean)) && !search_path_check()) {
      search_path_restore()
      search.path.restored <- TRUE
    }
    # Summary view of deltas and changes

    unitizer.summary <- summary(unitizer)
    cat("\n")

    if(!interactive.mode || non.interactive) {
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

  # Group tests by section and outcome for review

  if(is.null(test.file)) {
    unitizer.browse <- browsePrep(unitizer, mode="review")
  } else {
    unitizer.browse <- browsePrep(unitizer, mode="unitize")
  }
  # Interactively decide what to keep / override / etc.

  tot.time <- (proc.time() - start.time)[["elapsed"]]
  unitizer <- browseUnitizer(
    unitizer, unitizer.browse, prompt.on.quit=tot.time > quit.time,
    show.passed=is.null(test.file)  # this means we're in review mode
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
