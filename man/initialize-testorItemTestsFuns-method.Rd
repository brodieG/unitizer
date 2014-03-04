\docType{methods}
\name{initialize,testorItemTestsFuns-method}
\alias{initialize,testorItemTestsFuns-method}
\title{Ensures Functions are In Correct Format}
\usage{
\S4method{initialize}{testorItemTestsFuns}(.Object, ...)
}
\arguments{
  \item{.Object}{ An object: see the Details section.}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Also, allows the user to specify functions directly instead
of having to instantiate
\code{`\link{testorItemTestFun-class}`} for each function.
Finally, recovers the deparsed passed function name.
}
\keyword{internal}

