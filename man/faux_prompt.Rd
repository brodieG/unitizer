\name{faux_prompt}
\alias{faux_prompt}
\title{Emulate the R Console Prompt}
\usage{
faux_prompt(prompt = getOption("prompt"), continue = getOption("continue"))
}
\arguments{
  \item{prompt}{what character to use as the prompt
  character}

  \item{continue}{what character to use as the second and
  onward prompt line character}
}
\value{
the expression typed in by the user
}
\description{
Emulate the R Console Prompt
}

