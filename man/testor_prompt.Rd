\name{testor_prompt}
\alias{testor_prompt}
\title{Handles The Actual User Interaction}
\usage{
testor_prompt(text, browse.env = globalenv(), help = character(),
  valid.opts, hist.con = NULL, exit.condition = function(...) FALSE)
}
\arguments{
  \item{text}{the prompt text to display}

  \item{browse.env}{the environment to evaluate user
  expressions in; typically this will contain interesting
  objects (use \code{ls()} to review)}

  \item{valid}{opts the special letters user can type to
  get a special action, typically a character vector where
  the names are one letter (though they don't actually have
  to be) and are looked for as user typed input; note that
  the quit and help options will always be appended to
  this}

  \item{help}{a character vector with help suggestions}

  \item{hist.con}{connection to save history to}

  \item{exit.condition}{function used to evaluate whether
  user input should cause the prompt loop to exit}
}
\value{
mixed allowable user input
}
\description{
Will keep accepting user input until either: \itemize{
\item User types one of the names of \code{`valid.opts`},
typically "Y" or "N", but will vary \item User types "Q"
\item User inputs an expression that when evaluated and fed
to \code{`exit.condition`} returns TRUE } The set-up is
intended to replicate something similar to what happens
when code hits a \code{`browse()`} statement.  User
expressions are evaluated and output to screen, and special
expressions as described above cause the evaluation loop to
terminate.
}
\seealso{
browse_testor_items
}
\keyword{internal}

