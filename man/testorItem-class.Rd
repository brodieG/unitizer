\docType{class}
\name{testorItem-class}
\alias{testorItem-class}
\title{Full Representation Of an Evaluated \code{`testor`} Test}
\description{
Full Representation Of an Evaluated \code{`testor`} Test
}
\section{Slots}{

\describe{
\item{\code{call}}{the call that is tested}

\item{\code{comment}}{description of what the call is testing (or whatever, frankly)}

\item{\code{reference}}{whether this is a reference or new \code{`testorItem`}}

\item{\code{ignore}}{whether this test should be treated as a test or just a step
  towards compiling a test}

\item{\code{environment}}{the environment the test was evaluated in, should contain
  all relevant objects}

\item{\code{data}}{the container for the results of the evaluation of \code{`call`}}
}}

