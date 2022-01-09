# Copyright (C) 2022 Brodie Gaslam

# This file is part of "aammrtf - An Almost Most Minimal R Test Framework"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 or 3 of the License.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

## Mock a Function During Testing
##
## Replaces the guts of a function using the `trace` mechanism.  This may not
## work with functions that are inlined by the byte compiler, e.g. primitives.
## A work-around is to put these inside a wrapper function in your code,
## although of course that comes with some overhead.
##
## @param f function to mock
## @inheritParams trace (see `?trace` for other parameters.
## @examples
## expr <- quote(as.POSIXct('1999-12-31 23:59:59'))
## local({
##    mock(base::Sys.time, expr)
##    on.exit(unmock(base::Sys.time))
##    Sys.time()
## })

mock <- function(f, tracer, where=f, print=FALSE)  {
  editor <- function(name, file, title) {body(name) <- tracer; name}
  old.edit <- options(editor=editor)
  on.exit(options(old.edit))
  invisible(
    suppressMessages(
      eval(
        bquote(trace(.(substitute(f)), edit=TRUE, print=FALSE, where=.(where))),
        parent.frame()
  ) ) )
}
unmock <- function(f, where=f)  {
  invisible(
    suppressMessages(
      eval(bquote(untrace(.(substitute(f)), where=.(where))), parent.frame()) 
) ) }
