\name{runtests}
\alias{runtests}
\title{Run Tests and Compare Against Stored Results}
\usage{
runtests(test.file, store.id = sub("\\\\.[Rr]$", ".rds", test.file))
}
\arguments{
  \item{test.file}{path to the file containing tests}

  \item{store.id}{R object describing store location;
  typically a path to a .rds file; set to NULL to
  auto-generate a location on the file.system}
}
\description{
The objective of this function is to allow seamless
creation of unit tests from a file that contains informal
tests (i.e. lines of code that need to be run and reviewed
to determine whether they executed appropriately). The
workflow consists of two parts.  First: \enumerate{ \item
create a file with informal tests (expressions) \item
evaluate file through this function \item review each
expression to confirm it evaluates appropriately \item
store results } Then: \enumerate{ \item modify the code you
are testing \item run this function to see if any of your
tests broke \item interactively add tests or update tests
\item cycle back to 1. } There are two main benefits to
this approach over the standard approach: \enumerate{ \item
it is trivial to create tests for expressions that return
very complex objects (e.g. imagine trying to write a test
to confirm that \code{`\link{lm}`} behavior isn't changing)
because the test is the expression itself \item there is
very little overhead to writing the tests since they are
pretty much a command that any user of your code would type
out as is } Some will rightly argue that #2 here is
actually a drawback since it isn't immediately obvious from
looking at the test as written what the result of the test
should be, or closely related, it isn't possible to assert
what the result of the test should be.  These are fair
criticisms, but the main objective of this function is to
enable #1.  Additionally, if you want assertions, there is
nothing preventing you from using:
\code{identical(my_fun(my_test_input), TRUE)} as an
\dQuote{informal} test in your test file.
}
\section{Test Storage}{
  The default behavior for this function is to store test
  results in a \file{.rds} file in the same location as the
  \file{.R} file containing the test expressions.  You can
  easily modify the location of the stored tests through
  the \code{`store.id`} parameter by specifying a new file
  system location.

  If you wish to store your tests outside of your
  filesystem (e.g. MySQL database), you can do so by
  creating methods for \code{`\link{get_store}`} and
  \code{`\link{set_store}`}.  This function uses the
  character methods for those generic functions.  See
  documention for \code{`\link{get_store}`} for more
  details.
}

\section{Result Capture}{
  In order to properly capture output, this function must
  mess with streams and options.  In particular, it will do
  the following: \enumerate{ \item temporarily set
  \code{options(warn=1L)} during expression evaluation
  \item temporarily set \code{options(error=null)} during
  expression evaluation \item use
  \code{`\link{capture.output}`} to capture any output to
  \file{stdout} \item use
  \code{\link{sink}(type="message")} to capture output to
  \file{stderr} } This should all be transparent to the
  user, unless the user is also attempting to modify these
  settings in the test expressions.  The problematic
  interaction are around the \code{`\link{options}`}
  function.  If the user sets \code{options(warn=1)} with
  the hopes that setting will persist beyond the execution
  of the test scripts, that will not happen.  If the user
  sets \code{options(error=recover)} or some such in a test
  expression, and that expression throws an error, you will
  be thrown into recovery mode with no visibility of
  \file{stderr} or \file{stdout}, which makes for pretty
  challenging debugging.

  If the function is run with message diversion already
  activated, then it will not capture any messages produced
  by the test expressions.  If one of the test expressions
  enables message capture, then this function will stop
  capturing messages from that point on.  If after this
  another test expression disables message capture, this
  function will start capturing messages starting with the
  subsequent test expression (i.e. any message output
  between the expression self disabling message output and
  the end of the expression will go to \file{stdout}).
}

\section{Error Handling}{
  Because \code{`runtests`} evaluates test expressions
  within a call to \code{`\link{withCallingHandlers}`},
  there are some limitations on successfully running
  \code{`runtests`} inside your own error handling calls.
  In particular, \code{`runtests`} will not work properly
  if run inside a \code{`\link{tryCatch}`} statement. If
  test expressions throw conditions, the internal
  \code{`\link{withCallingHandlers}`} will automatically
  hand over control to your \code{`\link{tryCatch}`}
  statement without an opportunity to complete
  \code{`runtests`} computations.  Unfortunately there does
  not seem to be a way around this since we have to use
  \code{`\link{withCallingHandlers}`} so that test
  statements after non-aborting conditions are run.
}
\seealso{
\code{`\link{get_store}`}
}

