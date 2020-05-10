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

#' Edit Calls In Unitizer
#'
#' Used if you want to change language in test expression in a unitizer when
#' the actual results of running the expressions is unchanged.  This is useful
#' if you decided to rename functions, etc., without having to re-run the entire
#' \code{unitize} process since \code{unitize} matches tests based on
#' expressions.
#'
#' @note this is a somewhat experimental function, so make sure you backup any
#'   unitizers before you try to use it.
#'
#' @export
#' @rdname editCalls
#' @param x a unitizer object
#' @param lang.old the name of the function replace
#' @param lang.new the new name of the function
#' @param ... unused
#' @return a untizer object with function names modifies

setGeneric("editCalls", function(x, lang.old, lang.new, ...)
  standardGeneric("editCalls")
)
#' @export
#' @rdname editCalls
#' @param interactive.only logical(1L) set to FALSE if you want to allow this to
#'   run in non-interactive mode, but warnings will be suppressed and will
#'   proceed without prompting, obviously...
#' @examples
#' \dontrun{
#' untz <- get_unitizer("tests/unitizer/mytests.unitizer")
#' untz.edited <- editCalls(untz, quote(myFun), quote(my_fun))
#' set_unitizer("tests/unitizer/mytests.unitizer", untz.edited)
#' }

setMethod("editCalls", c("unitizer", "language", "language"),
  function(x, lang.old, lang.new, interactive.only=TRUE, ...) {
    warning(
      "This is an experimental function; make sure you backup any unitizers ",
      "before you edit them", immediate.=TRUE
    )
    if(!interactive_mode() && interactive.only)
      stop("Set interactive.only to FALSE to run in non-interactive mode")
    i <- 0L
    if(interactive.only) {
      u.inp <-simple_prompt(
        "Do you wish to proceed ([Y]es/[N]o)? "
      )
      if(!identical(u.inp, "Y")){
        message("Exiting without edits")
        return(x)
      }
    }
    call_sub <- function(call, old.name, new.name) {
      if(is.language(call)) {
        if(identical(call, old.name)) return(new.name)
        if(length(call) > 1)
          for(j in 1:length(call))
            if(is.language(call[[j]]))
              call[[j]] <- Recall(call[[j]], old.name, new.name)
      }
      call
    }
    for(i in seq_along(x@items.ref)) {
      # complexity here driven by a change from storing the actual call to
      # only keeping the deparsed version, but not wanting to re-write all the
      # code for the renaming

      call.is.null <- FALSE
      if(is.null(x@items.ref[[i]]@call)) {
        call.parsed <- parse(text=x@items.ref[[i]]@call.dep)
        if(!is.expression(call.parsed) || length(call.parsed) != 1L)
          # nocov start
          stop(
            "Internal Error: call `", x@items.ref[[i]]@call.dep,
            "` did not produce a length one expression when parsed"
          )
          # nocov end
        x@items.ref[[i]]@call <- call.parsed[[1L]]
        call.is.null <- TRUE
      }
      x@items.ref[[i]]@call <-
        call_sub(x@items.ref[[i]]@call, lang.old, lang.new)
      x@items.ref[[i]]@call.dep <- deparse_call(x@items.ref[[i]]@call)
      x@items.ref.calls.deparse[[i]] <- x@items.ref[[i]]@call.dep
      if(call.is.null) x@items.ref[[i]]@call <- NULL
    }
    x
} )
