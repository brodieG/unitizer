# Copyright (C) 2020  Brodie Gaslam
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

# Emulate the R Console Prompt
#
# @keywords internal
# @param prompt what character to use as the prompt character
# @param continue what character to use as the second and onward prompt line character
# @return the expression typed in by the user

faux_prompt <- function(
  prompt=getOption("prompt"), continue=getOption("continue")
) {
  res <- character()
  old.opt <- options(warn=1L)
  on.exit(options(old.opt))
  repeat {
    res.parse <- tryCatch({
        res <- paste0(res, read_line(prompt), if(length(res)) '\n' else "")
        parsed <- parse(text=res)
      },
      error=function(e) {
        if(!isTRUE(grepl(" unexpected end of input\n", conditionMessage(e)))) {
          e$call <- if(length(res)) res else NULL
          stop(e)
        }
    } )
    prompt <- `continue`
    if(is.expression(res.parse)) {
      return(res.parse)
  } }
  NULL
}
