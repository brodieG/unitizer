\name{top_level_parse_parents}
\alias{top_level_parse_parents}
\title{Searches Through Generation Until it Finds Top Level}
\usage{
top_level_parse_parents(ids, par.ids)
}
\arguments{
  \item{ids}{integer the ids to look through}

  \item{par.ids}{integer the parenthood relationships}
}
\value{
integer the top level parent ids for \code{`ids`}
}
\description{
Returns the id of the ancestor that is just before
\code{`id==0`}
}

