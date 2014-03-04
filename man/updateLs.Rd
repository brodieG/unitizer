\name{updateLs}
\alias{updateLs}
\title{Compare The Objects In Environment for Ref vs. New Item}
\usage{
updateLs(x, ...)
}
\arguments{
  \item{x}{the \code{`\link{testorItem-class}`}}

  \item{base.env}{the last environment to search through
  for objects}
}
\value{
\code{`\link{testorItem-class}`} object with updated
\code{`ls`} field and environment reference parent
}
\description{
Makes sure that when we call \code{`ls`} when browsing its
environment the information reflecting any limitations on
what objects are actually available to browse is properly
reflected.
}
\details{
The status of environment objects is tracked in
\code{`x@ls$status`}, where objects of different status are
marked like so: \itemize{ \item ': object exists in
browsing environment, but not the same as it was when test
was evalaluated \item *: object was present during test
evaluation but is not available in testor anymore \item **:
object was not present during test evaluation, but exists
in current environment }
}

