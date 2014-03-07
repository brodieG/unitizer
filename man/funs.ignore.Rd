\docType{data}
\name{funs.ignore}
\alias{funs.ignore}
\title{Functions To Ignore}
\format{\preformatted{List of 2
 $ :.Primitive("<-") 
 $ :function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, logical.return = FALSE, warn.conflicts = TRUE, 
    quietly = FALSE, verbose = getOption("verbose"))  
}}
\usage{
funs.ignore
}
\description{
Ignored functions are not considered tests if they are
called from the top level.
}
\keyword{internal}

