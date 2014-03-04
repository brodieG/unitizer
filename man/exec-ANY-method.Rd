\docType{methods}
\name{exec,ANY-method}
\alias{exec,ANY-method}
\title{Manages Test Expression Evaluation}
\usage{
\S4method{exec}{ANY}(x, test.env)
}
\arguments{
  \item{test}{the call to test}

  \item{test.env}{the environment to evaluate the
  \code{`test`} in}
}
\value{
a \code{`\link{testorItem-class}`} object
}
\description{
Runs test, captures value, stdout, stderr, conditions, etc.
}
\keyword{internal}

