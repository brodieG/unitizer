\name{getTest}
\alias{getAborted}
\alias{getConds,}
\alias{getMsg,}
\alias{getOut,}
\alias{getTest}
\alias{getVal,}
\title{Retrieve Additional Info About Tests}
\usage{
getTest(x)
}
\arguments{
  \item{x}{object to get additional data for (should be one
  of \code{`obj`}, \code{`ref`})}
}
\value{
depends on what you requested: \itemize{ \item
\code{`getConds`}: the conditions as a list of conditions
or an empty list if no conditions occurred.  \item
\code{`getOut`}: the screen output (i.e. anything produced
by cat/print, or any visible evaluation output) as a
character vector \item \code{`getMsg`}: anything that was
output to \code{`stderr`}, mostly this is all contained in
the conditions as well, though there could be other output
here, as a character vector \item \code{`getExpr`}: the
call that was tested as an unevaluated call, but keep in
mind that if you intend to evaluate this for a reference
item the environment may not be the same so you could get
different results (\code{`ls`} will provide more details)
\item \code{`getAborted`}: returns whether the test call
issues a restart call to the `abort` restart, as `stop`
does.  \item \code{`getVal`}: the value that results from
evaluating the test, note that typing \code{`obj`} and
\code{getVal(`obj`)} at the \code{`testor`} prompt are
equivalent \item \code{`reCall`}: will load the call used
to generate the test on the prompt (not implemented yet). }
}
\description{
Intended for use exclusively within the \code{`testor`}
interactive command line.  For example \code{getMsg(.new)}
will retrieve any \file{stderr} that occurred during test
evaluation (for reference tests, use \code{getMsg(.ref)}.
}

