\name{text_wrap}
\alias{text_wrap}
\title{Wrap Text At Fixed Column Width}
\usage{
text_wrap(x, width)
}
\arguments{
  \item{x}{character vector}

  \item{width}{integer vector with}
}
\value{
a list with, for each item in \code{`x`}, a character
vector of the item wrapped to length \code{`width`}
}
\description{
Some day this should be upgraded to break at whitespaces or
use hyphens instead of wrapping arbitrarily at spec'ed
width
}
\keyword{internal}

