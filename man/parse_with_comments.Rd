\name{parse_with_comments}
\alias{parse_with_comments}
\title{Recursively Descends Through a Parsed Expression and Assigns Comments}
\usage{
parse_with_comments(file, text = NULL)
}
\arguments{
  \item{expr.main}{an expression produced by
  \code{`\link{parse}`}}
}
\value{
an expression with comments retrieved from the parse
attached to the appropriate sub-expressions/calls as a
\dQuote{comment} \code{`\link{attr}`}
}
\description{
In order to implement this we had to make several
assumptions about the behaviour of
\code{`\link{getParseData}`}.  In particular: \itemize{
\item Top level comments show up with negative ids, but are
top level for all intents and purposes \item All content
tokens (i.e. anything other than brackets, commas, etc.)
are contained inside an \code{`expr`}, even if the only
thing the `expr` contains is a simple constant.  \item
Comments are not content tokens and can exist on the top
level without being wrapped in an \code{`expr`} \item The
only tokens that count as elements in an expression are
opening brackets and \code{`expr`}; this assumption is
necessary to allow mapping the parsed data back to the
expression.  What confuses the issue a bit is that
operators show up at the top level, but you can actually
ignore them.  Also, parantheses should only be kept if they
are the topmost item, as otherwise they are part of a
function call and should be ignored.  \item Comments inside
function formals are not assigned to the formals proper
\item `exprlist` tokens are removed completely b/c as far
as we can tell they are not part of the parsed object (but
exist in parse data).  \item known issue: comments in
formals after a line break are assigned to the body of the
function as opposed to \code{`function`}, but this should
not be apparent in common use.  \item you cannot attach
comments to \code{`NULL`}, if you must use \code{`(NULL)`}.
This is a feature, as it proivdes a way to put comments in
the file without them showing up during \code{`testor`}
use. } Note that as a result of this trial and error
interpretation of \code{`\link{getParseData}`} it is likely
that comment parsing is not 100 percent robust.
}
\details{
Due to some reference weirdness going on when dealing
directly with expressions had to change this function to
accept text/file rather than an expression as an input (but
even that didn't fix it!!!)
}
\seealso{
comments_assign, getParseData, parse
}

