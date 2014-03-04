\docType{methods}
\name{initialize,testorItem-method}
\alias{initialize,testorItem-method}
\title{Initialize A \code{`\link{testorItem-class}`}}
\usage{
\S4method{initialize}{testorItem}(.Object, ...)
}
\arguments{
  \item{.Object}{ An object: see the Details section.}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Makes the fact that most of the data needs to be part of a
\code{`\link{testorItemData-class}`} object transparent to
the user.
}
\keyword{internal}

