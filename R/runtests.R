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
#' Because \code{`runtests`} evaluates test expressions within a call to
#' \code{`\link{withCallingHandlers}`}, there are some limitations on successfully
#' running \code{`runtests`} inside your own error handling calls.  In particular,
#' \code{`runtests`} will not work properly if run inside a \code{`\link{tryCatch}`} 
#' statement. If test expressions throw conditions, the internal 
#' \code{`\link{withCallingHandlers}`} will automatically hand over control to 
#' your \code{`\link{tryCatch}`} statement without an opportunity to complete 
#' \code{`runtests`} computations.  Unfortunately there does not seem to be a 
#' way around this since we have to use \code{`\link{withCallingHandlers}`}
#' so that test statements after non-aborting conditions are run.
#'  
#' @export
#' @seealso \code{`\link{get_store}`}
#' @param test.file path to the file containing tests
#' @param store.id R object describing store location; typically a path to a 
#'   .rds file; set to NULL to auto-generate a location on the file.system

runtests <- function(test.file, store.id=sub("\\.[Rr]$", ".rds", test.file)) {

  # Retrieve or create testor environment

  par.frame <- parent.frame()
  if(!is.character(test.file) || length(test.file) != 1L || !file_test("-f", test.file)) stop("Argument `test.file` must be a valid path to a file")
  if(inherits(try(testor <- get_store(store.id)), "try-error")) {
    stop("Unable to retrieve/create `testor` at location ", store.id, "; see prior errors for details.")
  }
  if(identical(testor, FALSE)) {
    testor <- new("testor", id=store.id, zero.env=new.env(parent=par.frame))
  } else if(!is(testor, "testor")){
    if(!identical(class(store.id), "character")) stop("Logic Error: `get_store.", class(store.id)[[1]], "` did not return a testor")
    stop("Logic Error: `get_store` did not return a testor; contact maintainer.")
  } else if(inherits(try(validObject(testor, complete=TRUE)), "try-error")) {
    if(
      inherits(version.compare <- try(testor@version != packageVersion("testor"), silent=TRUE), "try-error") |
      !isTRUE(version.compare)
    ) {
      stop("Invalid `testor`; contact package maintainer")
    } else {
      stop(
        "You are attempting to load a `testor` generated by a different version (", 
        testor@version, ") of `testor` vs. the currently loaded one (", packageVersion("testor"), 
        ").  In order to transition to new version, confirm that your test script is ",
        "working correctly by checking it with the version of `testor` used to generate ",
        "the tests, and then re-generate the `testor` with the new version of `testor`."
  ) } }
  testor@id <- store.id
  parent.env(testor@zero.env) <- par.frame
  list2env(getItemFuns, testor@zero.env)     # functions for accessing testorItem contents
  test.env <- new.env(parent=testor@items.new@base.env)
  wd <- getwd()                              # in case user changes it through tests
  on.exit(message("Exited before storing results; tests were not saved or changed."))  # In case interrupted or some such

  # Parse the test file

  if(inherits(try(tests.parsed <- parse(test.file)), "try-error")) {
    stop("Unable to parse `test.file`; see prior error for details.")
  }
  if(!length(tests.parsed)) {
    message("No tests in ", test.file, "; nothing to do here.")
    return(TRUE)
  }
  tests.parsed <- parse_data_assign(tests.parsed)
  
  # Evaluate the parsed calls

  tests <- new("testorTests") + tests.parsed
  testor <- testor + tests

  # Summary view of deltas and changes

  print(H1(paste0("testor for: ", test.file, collapse="")))
  print(testor.summary <- summary(testor))
  cat("\n")

  if(!interactive()) {
    if(any(tail(testor.summary, 1L)[, -1L] > 0L)) {  # Passed tests are first column
      stop("Newly generated tests do not match testor; please run in interactive mode to see why")
  } }
  # Interactively decide what to keep / override / etc.; Action "A" is use new item, "B" is use
  # old item, and "C" is do nothing.  Additionally, prepare testor 
  
  testor <- browse(testor, env=new.env(par.frame))
  on.exit(NULL)  # main failure points are now over so don't need to alert on failure

  if(length(testor@changes) == 0L) {
    message("No items to store; there either were no changes or you didn't accept any changes.")
    return(TRUE)
  } 
  print(H2("Confirm Changes"))
  cat("You are about to IRREVERSIBLY:\n")

  show(testor@changes)
  message("Update testor ([Y]es, [N]o, [Q]uit, [H]elp)?")
  help <- paste0(
    "Pressing Y will replace the previous testor with a new one updated with all the ",
    "changes you approved."
  )
  act <- testor_prompt("Update testor", new.env(par.frame), help)
  if(identical(act, "Y")) {
    if(!identical((new.wd <- getwd()), wd)) setwd(wd)  # Need to do this in case user code changed wd
    success <- try(set_store(store.id, testor))
    setwd(new.wd)
    if(!inherits(success, "try-error")) message("testor updated")
    return(success)    
  } else {
    message("testor changes NOT saved")
  }
  return(FALSE)
}
#------------------------------------------------------------------------

# CAPTURE

# FIGURE OUT HOW TO INCORPORATE `evaluate`?  IN PARTICULAR, THE REPLAY
# FEATURE SEEMS INTERESTING.
# 
# DO WE REALLY WANT TO ALLOW EXECUTION IN INTERACTIVE MODE WITH STDERR()
# AND STDOUT() CAPTURED? SEEMS A BIT WEIRD TO ALLOW IT.
# 
# IMPLEMENT CAPTURE HANDLING AT CONDITION HANDLER LEVEL AND INTEGRATE
# OUTPUT AND CONDITION STREAMS SO TAHT WE CAN DO THE REPLAY? RIGHT NOW
# CONDITIONS COME OUT OF ORDER.

# SHOULD WE CAPTURE ACTUAL stdout() VS STDOUT CAUSED BY A VISIBLE
# VALUE SEPARATELY?  PROBABLY YES, MAKES WAY MORE SENSE FOR THE
# show.testorItem METHOD
# 
# NEED A BETTER MECHANISM FOR HANDLING THE options CHANGES FOR
# ERROR AND WARNING?  DO WE REALLY WANT TO CHANGE THE ERROR
# OPTIONS? OR ALLOW WARNING=2?
# 
# CAN WE GET AWAY WITH textConnection CAPTURES FOR THE FILES TO
# AVOID CREATING/RECREATING FILES EVERY TIME?  RIGHT NOW THE SIMPLICITY
# OF CREATING THE FILES IN THE INNERMOST PART OF THE LOOP IS GREAT, BUT
# MIGHT BE CAUSING OVERHEAD.

# Debugging partially implemented by disabling captures, but really,
# that's not real debugging.  Unfortunately, because we capture std.err
# and we can't tee that, there is no good debugging to implement.
# 
#------------------------------------------------------------------------

# NAVIGATION

# SHOULD WE CAPTURE ALL OPTIONS IN CASE USER CHANGES SOME THAT WE DON'T EXPECT
# AND DON'T KNOW WHAT TO DO ABOUT?
# 
# MORE ADVANCED UNDO, VIEW OF WHAT WE'VE DONE SO FAR?
# 
# RELATED: SMART QUIT: SAVE STUFF WE'VE DONE SO FAR

# KEEP SEPARATE HISTORY FOR testor BROWSING, AND DUMP WHEN DONE?

# IMPORTANT TO HAVE COMMENTS SO WE KNOW WHAT EACH TEST IS SUPPOSED TO BE
# WHEN REVIEWING?

# PERFORMANCE PROBLEMS FROM TOO MANY NESTED ENVIRONMENTS? PARTIALLY RESOLVE
# BY COLLAPSING IGNORED TEST ENVIRONMENTS?
# 
# SHOULD WE KEEP OBJECTS THAT USER CREATES WHILE BROWSING
# ACROSS TESTS?  PROBABLY, BUT WE DON'T RIGHT NOW.
# 
# DO WE EVEN NEED TO KEEP THE TESTS FOR IGNORED TEST?  JUST DISCARD
# IN runtests?
# 
# ACCEPT ALL (HIDDEN?) OPTION
# 
# RESPONSIVENESS OF COMMAND LINE IS A LITTLE SLOW; WHY?

#------------------------------------------------------------------------

# S4 STRUCTURE/LOGIC
# 
# NEED TO THINK THROUGH intialize METHODS SO HOPEFULLY WE CAN AVOID INSTANTIATING
# WITH .items ARGUMENT FOR testorList INHERITORS SINCE THAT'S A BIT WEIRD 
# 
# GENERALLY, NEED TO ADD MORE METHODS SO WE'RE NOT MESSING WITH SLOTS
# DIRECTLY
# 
# WHY ARE testor CLASSES AVAILABLE EVEN THOUGH THEY ARE NOT EXPORTED????
# DO THEY HAVE TO BE IN ORDER TO SUPPORT READING THE STORED OBJECTS?
# RELATED: WHY IS IT THAT IT SEEMS THAT THE GENERICS ARE EXPORTED
# AFTER DOCUMENTATION, BUT ANY SUBSEQUENT RE-INSTALLS APPERAS TO BLOW THEM
# AWAY?
# 
#------------------------------------------------------------------------
#
# SECTIONS
# 
# WHAT DO WE DO WITH DEFAULT SECTION THAT HAS VALUES SCATTERED THROUGHOUT?
# 
# SUPER CONFUSING WHEN USING title & expr FOR testor_sect WHEN ACCIDENTALLY
# PUTTING EXPR IN TITLE SINCE CHECKING THAT title IS A STRING FORCES
# EVALUATION OF expr? HOW TO CHECK WITHOUT CAUSING PROBLEMS? THE ISSUE
# HERE IS FOR SUBSECTIONS? BLERGH, LEAVE TITLE FIRST, CAN SKIP IT IF YOU
# WANT SUBSECTIONS WITHOUT TITLE BY SPECIFYING expr=
# 
# TEST `description`

#------------------------------------------------------------------------

# MISC
# NEED TO CHECK WHETHER THERE ARE NON-STANDARD RE-STARTS CONFIGURED?
# OR DO WE JUST NOT SUPPORT THAT? PROBABLY DON'T SUPPORT IT.
# MAKE SURE LS DOESN'T GET DEFINED IN GLOBAL ENV!! RIGHT NOW IT SEEMS TO BE,
# EVERY NOW AND THEN.
# 
# DO THE ONLOADS STUFF DO ANYTHING NOW?
# 
# PROVIDE FACILITIES TO UPATE testor ID WHEN testors ARE MOVED, THOUGH
# TYPICALLY THE testor SHOULD ALWAYS BE IN THE SAME RELATIVE LOCATION
# TO THE SCRIPT THAT RUNS IT.
# 
# NOT SURE THAT CONDITION COMPARISON FUNCTION SHOULD AUTOMATICALLY LOOP
# THROUGH LISTS. THIS COMPLICATES THE FUNCTION SUBSTANTIALLY, THOUGH IT
# DOES HAVE THE ADVANTAGE OF ALLOWING THE USER TO HANDLE THE CASES WERE
# CONDITION LISTS HAVE DIFFERENT NUMBER OF ENTRIES; MAYBE THERE IS AN OPTION
# TO PROVIDE A LIST VERSION VS. NORMAL VERSION? AND IF THERE IS A MISMATCH
# IN CONDITION LENGTHS, THEN ACTIVELY TELL THE USER THEY CAN USE THE
# LIST VERSION?
