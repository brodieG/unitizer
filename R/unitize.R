#' Run Tests and Compare Against Stored Results
#' 
#' The objective of this function is to allow seamless creation of unit tests
#' from a file that contains informal tests (i.e. lines of code that need to 
#' be run and reviewed to determine whether they executed appropriately).
#' The workflow consists of two parts.  First:
#' \enumerate{
#'   \item create a file with informal tests (expressions)
#'   \item evaluate file through this function
#'   \item review each expression to confirm it evaluates appropriately
#'   \item store results
#' }
#' Then:
#' \enumerate{
#'   \item modify the code you are testing
#'   \item run this function to see if any of your tests broke
#'   \item interactively add tests or update tests
#'   \item cycle back to 1.
#' }
#' There are two main benefits to this approach over the standard approach:
#' \enumerate{
#'   \item it is trivial to create tests for expressions that return very
#'     complex objects (e.g. imagine trying to write a test to confirm that
#'     \code{`\link{lm}`} behavior isn't changing) because the test is the
#'     expression itself
#'   \item there is very little overhead to writing the tests since
#'     they are pretty much a command that any user of your code would
#'     type out as is
#' }
#' Some will rightly argue that #2 here is actually a drawback since it isn't
#' immediately obvious from looking at the test as written what the result
#' of the test should be, or closely related, it isn't possible to assert
#' what the result of the test should be.  These are fair criticisms, but
#' the main objective of this function is to enable #1.  Additionally, if
#' you want assertions, there is nothing preventing you from using:
#' \code{identical(my_fun(my_test_input), TRUE)}
#' as an \dQuote{informal} test in your test file.
#' 
#' @section Test Storage:
#' 
#' The default behavior for this function is to store test results in a 
#' \file{.rds} file in the same location as the \file{.R} file containing
#' the test expressions.  You can easily modify the location of the stored
#' tests through the \code{`store.id`} parameter by specifying a new file
#' system location.
#' 
#' If you wish to store your tests outside of your filesystem (e.g. MySQL
#' database), you can do so by creating methods for \code{`\link{get_store}`}
#' and \code{`\link{set_store}`}.  This function uses the character methods
#' for those generic functions.  See documention for \code{`\link{get_store}`}
#' for more details.
#' 
#' @section What Constitutes at Test:
#' 
#' Any expression present in the test file with the exception of assignment
#' operations and a few special cases are tests.  So, for example in:
#' \code{
#' a <- 5
#' b <- 6
#' a + b
#' }
#' Only the third line is considered a test.  The other two lines will be
#' evaluated but they will not be treated as tests.  If you wish for an
#' assignment expression to be treated as test you can just wrap it in
#' parentheses:
#' \code{
#' (a <- 5)        # this is considered a test
#' b <- 6          # this is not
#' a / b           # this is considered a test
#' }
#' 
#' @section Test Evaluation:
#' 
#' Very roughly speaking, tests are evaluated as they would be if you source
#' the test file from the command line.  There is however one major difference:
#' each test is evaluated in its own environment.  The illusion works because
#' each new test environment has for parent the lexically preceding test's
#' environment, so when a test is evaluated, it has access to all previously
#' defined objects because R descends through all parent environments looking
#' for an object if it isn't present in the evaluation environment.
#' 
#' The illusion, isn't perfect though.  Consider:
#' \code{
#'   a <- function() b()
#'   b <- function() TRUE
#'   a()
#' }
#' In this case, when we evaluate `a()` we must step back two environments
#' to find `a`, but that's okay.  The problem is that once inside `a`, we must
#' now evaluate `b()`, but `b` is defined in a child environment, not a parent
#' environment so R's object lookup fails.
#' 
#' Now, the above example actually works because as noted in "What Constitutes
#' a Test", environments are only defined for tests, an neither the `a` or `b`
#' assignments are tests, so both `a` and `b` are assigned to the environment
#' of the `a()` call.  However, this really breaks:
#' \code{
#'   a <- function() b()
#'   NULL
#'   b <- function() TRUE
#'   a()
#' }
#' Since NULL is a valid test, `a` is assigned to the NULL test environment,
#' and `b` is assigned to the `a()` call test environment, and the illusion
#' is shattered.
#' 
#' If you are getting weird "object not found" errors when you run your tests,
#' but the same code doesn't generate those errors when run directly in the
#' command line, the illusion could be failing you.  Make sure that you assign
#' all the variables necessary right ahead of the test so they will all get
#' stored in the same environment.
#' 
#' @section Result Capture:
#' 
#' In order to properly capture output, this function must mess with streams
#' and options.  In particular, it will do the following:
#' \enumerate{
#'   \item temporarily set \code{options(warn=1L)} during expression evaluation
#'   \item temporarily set \code{options(error=null)} during expression evaluation
#'   \item use \code{`\link{capture.output}`} to capture any output to \file{stdout}
#'   \item use \code{\link{sink}(type="message")} to capture output to \file{stderr}
#' }
#' This should all be transparent to the user, unless the user is also attempting
#' to modify these settings in the test expressions.  The problematic interaction are
#' around the \code{`\link{options}`} function.  If the user sets \code{options(warn=1)} 
#' with the hopes that setting will persist beyond the execution of the test scripts,
#' that will not happen.  If the user sets \code{options(error=recover)} or some
#' such in a test expression, and that expression throws an error, you will be
#' thrown into recovery mode with no visibility of \file{stderr} or \file{stdout},
#' which makes for pretty challenging debugging.
#' 
#' If the function is run with message diversion already activated, then it will
#' not capture any messages produced by the test expressions.  If one of the test
#' expressions enables message capture, then this function will stop capturing
#' messages from that point on.  If after this another test expression disables
#' message capture, this function will start capturing messages starting with the
#' subsequent test expression (i.e. any message output between the expression self
#' disabling message output and the end of the expression will go to \file{stdout}).
#' 
#' @section Error Handling:
#' Because \code{`unitize`} evaluates test expressions within a call to
#' \code{`\link{withCallingHandlers}`}, there are some limitations on successfully
#' running \code{`unitize`} inside your own error handling calls.  In particular,
#' \code{`unitize`} will not work properly if run inside a \code{`\link{tryCatch}`} 
#' statement. If test expressions throw conditions, the internal 
#' \code{`\link{withCallingHandlers}`} will automatically hand over control to 
#' your \code{`\link{tryCatch}`} statement without an opportunity to complete 
#' \code{`unitize`} computations.  Unfortunately there does not seem to be a 
#' way around this since we have to use \code{`\link{withCallingHandlers}`}
#' so that test statements after non-aborting conditions are run.
#' 
#' @section Browsing:
#' 
#' \code{`unitizer`} provides an interactive environment to browse the results of
#' the tests.  This environment is designed to closely mimic the R command
#' line, but it is not the R command line.  The most obvious difference is that
#' the prompt is different, but there are more subtle differences you need to
#' be aware of.
#' 
#' You will be prompted after each test to accept the test, reject it, or do
#' something else.  At the promopt, you can evaluate expressions like you
#'  
#' @export
#' @seealso \code{`\link{get_store}`}
#' @param test.file path to the file containing tests
#' @param store.id R object describing store location; typically a path to a 
#'   .rds file; set to NULL to auto-generate a location on the file.system

unitize <- function(test.file, store.id=sub("\\.[Rr]$", ".rds", test.file)) {

  # Retrieve or create unitizer environment

  par.frame <- parent.frame()
  if(!is.character(test.file) || length(test.file) != 1L || !file_test("-f", test.file)) 
    stop("Argument `test.file` must be a valid path to a file")

  print(H1(paste0("unitizer for: ", test.file, collapse="")))

  if(inherits(try(unitizer <- get_store(store.id)), "try-error")) {
    stop("Unable to retrieve/create `unitizer` at location ", store.id, "; see prior errors for details.")
  }
  if(identical(unitizer, FALSE)) {
    unitizer <- new("unitizer", id=store.id, zero.env=new.env(parent=par.frame))
  } else if(!is(unitizer, "unitizer")){
    if(!identical(class(store.id), "character")) stop("Logic Error: `get_store.", class(store.id)[[1]], "` did not return a unitizer")
    stop("Logic Error: `get_store` did not return a unitizer; contact maintainer.")
  } else if(inherits(try(validObject(unitizer, complete=TRUE)), "try-error")) {
    if(
      inherits(version.compare <- try(unitizer@version < packageVersion("unitizer"), silent=TRUE), "try-error")
    ) {
      stop("Invalid `unitizer`; contact package maintainer")
    } else if (isTRUE(version.compare)) {
      message(
        "You are attempting to load a `unitizer` generated by an older version (", 
        unitizer@version, ") of `unitizer` vs. the currently loaded one (", packageVersion("unitizer"), 
        "). Do you wish to attempt to upgrade your `unitizer` to the new version ([Y]es, [N]o)?"
      )
      help <- paste0("Pressing Y will upgrade your `unitizer` to the newest version if possible")
      act <- unitizer_prompt(
        "Upgrade unitizer", new.env(par.frame), help, 
        valid.opts=c(Y="[Y]es", N="[N]o")
      )
      msg <- paste0(
        "If you don't wish to / can't automatically upgrade your unitizer you can re-",
        "run the tests with the version of unitizer you used to generate them in ",
        "the first place to ensure they still pass, manually delete the ",
        "corresponding RDS, and re-run the tests with the new version of `unitizer`."
      )
      if(identical(act, "Y")) {
        if(
          inherits(try(unitizer <- upgrade(unitizer)), "try-error") ||
          inherits(try(validObject(unitizer, complete=TRUE)), "try-error")
        ) {
          stop("Unable to upgrade. ", msg)
        }
        success <- try(set_store(store.id, unitizer))
        setwd(new.wd)
        if(!inherits(success, "try-error")) 
          message("unitizer upgraded; please re-run tests")
        return(invisible(success))
      } else {
        stop("Cannot proceed with out of date `unitizer`. ", msg)
      } 
    } else {
      stop("Logic Error: unitizer appears corrupted in some way; contact maintainer.")
    }
  }
  # Make sure not running inside withCallingHandlers / withRestarts / tryCatch

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
    "event tests produce conditions / errors.  We strongly recommend you re-run",
    "your tests outside of such handling functions."
  )
  # Setup the new unitizer

  unitizer@id <- store.id
  parent.env(unitizer@zero.env) <- par.frame
  list2env(getItemFuns, unitizer@zero.env)     # functions for accessing unitizerItem contents
  test.env <- new.env(parent=unitizer@items.new@base.env)
  wd <- getwd()                              # in case user changes it through tests
  on.exit(                                   # In case interrupted or some such 
    message(
      "Unexpectedly exited before storing results; ",
      "tests were not saved or changed."
  ) )
  # Parse the test file

  if(inherits(try(tests.parsed <- parse_with_comments(test.file)), "try-error")) {
    stop("Unable to parse `test.file`; see prior error for details.")
  }
  if(!length(tests.parsed)) {
    message("No tests in ", test.file, "; nothing to do here.")
    return(invisible(TRUE))
  }  
  # Evaluate the parsed calls

  tests <- new("unitizerTests") + tests.parsed
  unitizer <- unitizer + tests

  # Summary view of deltas and changes

  print(unitizer.summary <- summary(unitizer))
  cat("\n")

  if(!interactive()) {
    if(any(tail(unitizer.summary, 1L)[, -1L] > 0L)) {  # Passed tests are first column
      stop("Newly generated tests do not match unitizer; please run in interactive mode to see why")
  } }
  # Interactively decide what to keep / override / etc.
    
  unitizer <- withRestarts(
    browse(unitizer, env=new.env(par.frame)),
    earlyExit=function() {
      message("User quit without storing any tests")
      FALSE
    }
  )
  on.exit(NULL)  # main failure points are now over so don't need to alert on failure
  
  # There are two conditions where return value the previous statement isn't a
  # unitizer, 1. if the restart is invoked, 2. if all tests passed in which case
  # there is nothing to do.

  if(!is(unitizer, "unitizer")) return(invisible(TRUE))

  if(!identical((new.wd <- getwd()), wd)) setwd(wd)  # Need to do this in case user code changed wd
  success <- try(set_store(store.id, unitizer))
  setwd(new.wd)
  if(!inherits(success, "try-error")) message("unitizer updated")
  return(invisible(success))
}
