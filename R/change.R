# Copyright (C) 2022 Brodie Gaslam
# 
# This file is part of "unitizer"
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' Summary of Changes
#'
#' Changes arise any time a user, through the interactive unitizer mode,
#' requests the storing of a change (accept new version of failed test,
#' add new test, remove old test).
#'
#' @keywords internal

setClass("unitizerChanges",
  representation(failed="integer", new="integer", removed="integer", corrupted="integer", passed="integer"),
  prototype(failed=integer(2L), new=integer(2L), removed=integer(2L), corrupted=integer(2L), passed=integer(2L)),
  validity=function(object) {
    for(i in slotNames(object)) {
      if((len <- length(slot(object, i))) > 0L && len != 2L) {
        return(paste0("slot `@`", i, " must be of length 2"))
      } else if (slot(object, i)[[1L]] > slot(object, i)[[2L]]) {
        return(paste0("slot `@`", i, " must be length 2 with the first value less than the second"))
      }
    }
    TRUE
  }
)
#' Print Out A Summary Of the Changes
#' @keywords internal

setMethod("show", "unitizerChanges",
  function(object) {
    cat(as.character(object), sep="\n")
    invisible(NULL)
  }
)
#' Print Out A Summary Of the Changes
#' @keywords internal

setMethod("as.character", "unitizerChanges",
  function(x, width=getOption("width"), ...) {
    bullets <- character()
    if(sum(x@failed))
      bullets <- c(
        bullets,
        paste(
          "Replacing", x@failed[[1L]], "out of", x@failed[[2L]],
          "failed tests"
      ) )
    if(sum(x@new))
      bullets <- c(
        bullets,
        paste(
          "Adding", x@new[[1L]], "out of", x@new[[2L]],
          "new tests\n"
      ) )
    if(sum(x@removed))
      bullets <- c(
        bullets,
        paste(
          "Removing", x@removed[[1L]], "out of", x@removed[[2L]],
          "removed tests\n"
      ) )
    if(sum(x@corrupted))
      bullets <- c(
        bullets,
        paste(
          "Replacing", x@corrupted[[1L]], "out of", x@corrupted[[2L]],
          "tests with errors\n"
      ) )
    if(x@passed[[1L]])
      bullets <- c(
        bullets,
        paste(
          "Dropping", x@passed[[1L]], "out of", x@passed[[2L]],
          "passed tests\n"
      ) )
    as.character(UL(bullets), width=width)
  }
)
#' Return Sum of Total Changes
#' @keywords internal

setMethod(
  "length", "unitizerChanges",
  function(x) {
    sum(vapply(slotNames(x), function(y) slot(x, y)[[1L]], 1L))
  }
)
