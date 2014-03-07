\name{testor_prompt}
\alias{testor_prompt}
\title{Handles The Actual User Interaction}
\usage{
testor_prompt(text, browse.env, help = character(),
  valid.opts = valid.opts.def, hist.con = NULL)
}
\arguments{
  \item{text}{the prompt text to display}

  \item{browse.env}{the environment to evaluate user
  expressions in; typically this will contain interesting
  objects (use \code{ls()} to review)}

  \item{help}{a character vector with help suggestions}

  \item{hist.con}{connection to save history to}
}
\description{
Handles The Actual User Interaction
}
\seealso{
browse_testor_items
}
\keyword{internal}

