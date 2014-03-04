\docType{methods}
\name{append,testorList-method}
\alias{append,testorList-method}
\title{Append To a \code{`\link{testorList-class}`} Object}
\usage{
\S4method{append}{testorList}(x, values, after = length(x))
}
\arguments{
  \item{x}{the object to append to}

  \item{values}{the object to append}

  \item{after}{a subscript, after which the values are to
  be appended.}
}
\description{
\code{`values`} is coerced to list or expression depending
on type of \code{`x`} \code{`.items`} slot.
}
\details{
The resulting object must pass the validity method for
\code{`x`}.
}

