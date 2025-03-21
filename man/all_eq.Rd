% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/item.sub.R
\name{all_eq}
\alias{all_eq}
\title{Like all.equal but Returns Empty String If Not all.equal}
\usage{
all_eq(target, current, ...)
}
\arguments{
\item{target}{R object}

\item{current}{other R object to be compared to \code{target}}

\item{...}{arguments to pass to \code{\link{all.equal}}}
}
\value{
TRUE if \code{all.equal} returns TRUE, "" otherwise
all_eq(1, 1L)
all_eq(1, 2)
isTRUE(all_eq(1, 2))
}
\description{
Used as the default value comparison function since when values mismatch
we use \code{\link[diffobj]{diffObj}} which would make the text output from
\code{\link{all.equal}} somewhat redundant.
}
