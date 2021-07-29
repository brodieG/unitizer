# Copyright (C) 2021 Brodie Gaslam
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
  prompt.start <- prompt
  res <- ""
  res.parse <- NULL
  reset <- FALSE
  old.opt <- options(warn=1L)
  on.exit(options(old.opt))
  repeat {
    withRestarts(
      withCallingHandlers(
        res.parse <- tryCatch({
            new <- read_line(prompt)
            if(
              nzchar(new) && charToRaw(substr(new, nchar(new), nchar(new))) == 3
            ) {
              # Fake interrupt on receiving 0x03 (CTRL+C, we assume no encodings
              # out there use this internally...).  Can figure out how to
              # programatically send a CTRL+C to the console.
              invokeRestart("unitizerInterrupt")
            }
            res <- paste0(res, new, if(nzchar(res)) '\n' else "")
            parsed <- parse(text=res)
          },
          error=function(e) {
            ## Fix me: won't work in non-English locales?
            if(!isTRUE(grepl(" unexpected end of input\n", conditionMessage(e)))) {
              e$call <- if(nzchar(res)) res else NULL
              stop(e)
            }
        } ),
        interrupt=function(e) {
          invokeRestart("unitizerInterrupt")
        }
      ),
      unitizerInterrupt=function(e) {
        reset <<- TRUE
      }
    )
    if(reset) {
      cat("\n")
      if(!nzchar(res)) {
        cat("\n")
        meta_word_cat("Type \"Q\" at the prompt to quit unitizer.")
      }
      prompt <- prompt.start
      res <- ""
      reset <- FALSE
    } else prompt <- `continue`

    if(is.expression(res.parse)) {
      return(res.parse)
  } }
  NULL
}
