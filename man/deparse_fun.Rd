\name{deparse_fun}
\alias{deparse_fun}
\title{Returns a Character Function Name From A Language Object}
\usage{
deparse_fun(x)
}
\arguments{
  \item{x}{a call or a symbol}
}
\value{
character 1 length, NA if \code{`x`} can't possibly be a
function
}
\description{
Note this doesn't really try to check too hard whether the
\code{`x`} is indeed a function.
}
\keyword{internal}

