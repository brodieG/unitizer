\docType{class}
\name{testorItemData-class}
\alias{testorItemData-class}
\title{Data Produced During Evaluation of \code{`testor`} Test}
\description{
Kept separate from the \code{`\link{testorItem-class}`}
because these are the slots that get compared from the new
item to the reference items which means there are a whole
bunch of other classes that need to have the same structure
as this (e.g. \code{`\link{testorItemTestsFun}`}) and by
definining it we let those other classes confirm they have
the correct structure.
}
\keyword{internal}

