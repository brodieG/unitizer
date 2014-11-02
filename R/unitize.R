#' Unitize an R Test Script
#'
#' Turns a standard R script into unit tests by evaluating the expressions and
#' storing them along with their resuls.  Re-running \code{`unitize`} then
#' checks that the values remain unchanged.  See "unitizer" vignette for more
#' details.
#'
#' You can run \code{`unitize`} from the command line, or you can place one or
#' more \code{`unitize`} calls in an R file and source that.
#'
#' @export
#' @seealso \code{`\link{get_store}`}
#' @param test.file path to the file containing tests
#' @param store.id a folder to store the \code{`unitizer`} objects in; will auto-
#'   generate to a folder at the same location as the test file with the same
#'   name as the testfile, except ending in \code{`.unitizer`} instead of \code{`.R`}
#' @param interactive.mode logical(1L) whether to run in interactive mode
#' @param env.clean TRUE or environment, if TRUE tests are run in a clean
#'   environment, if an environment they are run with that environment as the
#'   parent.
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

unitize <- function(
  test.file, store.id=sub("\\.[Rr]$", ".unitizer", test.file),
  interactive.mode=interactive(), env.clean=TRUE,
  search.path.clean=getOption("unitizer.search.path.clean"),
  search.path.keep=c("tools:rstudio", "package:unitizer")
) {
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
  if(!is.logical(search.path.clean) || length(search.path.clean) != 1L)
    stop("Argument `search.path.clean` must be logical(1L)")
  if(!is.character(search.path.keep))
    stop("Argument `search.path.keep` must be character()")
  if(!is.character(test.file) || length(test.file) != 1L || !file_test("-f", test.file))
    stop("Argument `test.file` must be a valid path to a file")

  print(H1(paste0("unitizer for: ", test.file, collapse="")))

  over_print("Loading unitizer data.")
  # Retrieve or create unitizer environment (note that the search path trimming)
  # happens later.  Also note that pack.env$zero.env can still be tracking the
  # top package under .GlobalEnv

  par.frame <- if(isTRUE(env.clean)) pack.env$zero.env.par else env.clean
  unitizer <- try(load_unitizer(store.id, par.frame))

  if(inherits(unitizer, "try-error")) stop("Unable to load `unitizer`; see prior errors.")
  if(!is(unitizer, "unitizer")) return(unitizer)  # most likely because we upgraded and need to re-run

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
  # Parse the test file

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
    return(invisible(TRUE))
  }
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
    over_print("Search Path Setup.")
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
  # Evaluate the parsed calls

  tests <- new("unitizerTests") + tests.parsed
  unitizer <- unitizer + tests

  # Make sure our tracing didn't get messed up in some way

  search.path.restored <- FALSE
  if((isTRUE(search.path.clean) || isTRUE(env.clean)) && !search_path_check()) {
    search_path_restore()
    search.path.restored <- TRUE
  }

  # Summary view of deltas and changes

  browser()

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
    return(invisible(TRUE))
  }
  # Interactively decide what to keep / override / etc.

  tot.time <- (proc.time() - start.time)[["elapsed"]]
  unitizer <- withRestarts(
    browse(
      unitizer, env=new.env(par.frame),  # need to remove env=, doesn't do anything, but don't want to do it right now
      prompt.on.quit=tot.time >  quit.time
    ),
    noSaveExit=function() {
      message("Unitizer store was not modified.")
      FALSE
    },
    unitizerQuitExit=unitizer_quit_handler
  )
  # There are two conditions where return value the previous statement isn't a
  # unitizer, 1. if the restart is invoked, 2. if all tests passed in which case
  # there is nothing to do.

  if(!search.path.restored) {
    if(search.path.trim) search_path_restore()          # runs _unsetup() as well
    else if (search.path.setup) search_path_unsetup()
  }
  if(!is(unitizer, "unitizer")) {
    on.exit(NULL)
    return(invisible(TRUE))
  }
  # Reset the parent env of zero env so we don't get all sorts of warnings related
  # to trying to store a package environment

  parent.env(unitizer@zero.env) <- baseenv()

  # Finalize

  if(!identical((new.wd <- getwd()), wd)) setwd(wd)  # Need to do this in case user code changed wd
  success <- try(set_store(store.id, unitizer))
  setwd(new.wd)
  if(!inherits(success, "try-error")) {
    message("unitizer updated")
    on.exit(NULL)
  }
  return(invisible(success))
}
