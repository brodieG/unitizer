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

#' @include item.R

NULL

#' An `ls` Like Function
#'
#' Much like `ls`, except that it is designed to crawl up the \code{`.new`} and
#' \code{`.ref`} environments and display all the objects.
#'
#' This is used in \code{`browseUnitizer,unitizer-unitizerBrowse-method`},
#' and is re-assigned to \code{`ls`} for use in the \code{`unitizer`} prompt
#' environment.
#'
#' @keywords internal
#' @return list of object names, or a list with environments containing the
#'   objects

unitizer_ls <- function(name, pos = -1L, envir = parent.frame(),
   all.names = FALSE, pattern
) {
  if(!missing(pos) || !missing(name) || !missing(envir))
    stop(
      "You are using an overloaded version of `ls` that does not allow ",
      "using the `name`, `pos`, or `envir` arguments to `ls`; you can use ",
      "standard `ls` with `base::ls`."
    )
  new.item <- try(get(".NEW", parent.env(envir)), silent=TRUE)
  ref.item <- try(get(".REF", parent.env(envir)), silent=TRUE)

  ls.lst <- list()
  ls.test <- mods <- character()
  new.inv <- ref.inv <- FALSE

  if(inherits(new.item, "try-error") && inherits(ref.item, "try-error")) {
    # nocov start
    stop(
      "Internal error: could not find `unitizerItem` objects to list contents ",
      "of; contact Maintainer"
    )
    # nocov end
  }
  if(!inherits(new.item, "try-error")) {
    if(nrow(new.item@ls))
      ls.lst[["new"]] <- paste0(new.item@ls$names, new.item@ls$status)
    ls.lst[["tests"]] <- c(ls.lst[["tests"]], c(".new", ".NEW"))
    mods <- c(mods, Filter(nchar, unique(new.item@ls$status)))
    new.inv <- isTRUE(attr(new.item@ls, "invalid"))
  }
  if(!inherits(ref.item, "try-error")) {
    if(nrow(ref.item@ls))
      ls.lst[["ref"]] <- paste0(ref.item@ls$names, ref.item@ls$status)
    ls.lst[["tests"]] <- c(ls.lst[["tests"]], c(".ref", ".REF"))
    mods <- c(mods, Filter(nchar, unique(ref.item@ls$status)))
    ref.inv <- isTRUE(attr(ref.item@ls, "invalid"))
  }
  if(!inherits(ref.item, "try-error") && !inherits(new.item, "try-error")) {
    ls.lst[["tests"]] <- c(ls.lst[["tests"]], c(".diff", ".DIFF"))
  }
  if(new.inv || ref.inv) {
    warning(
      "The ls output for ",
      paste(c("`.new`", "`.ref`")[c(new.inv, ref.inv)], sep=", "),
      " is invalid.  This may be because you had corrupted environment chains ",
      "that had to be repaired. Re-generating the `unitizer` with ",
      "`unitize(..., force.update=TRUE)` should fix the problem.  If it ",
      "persists, please contact maintainer."
    )
  }
  structure(ls.lst[order(names(ls.lst))], class="unitizer_ls", mods=mods)
}

#' Worker function to actually execute the `ls` work
#'
#' @param env the environment to start \code{`ls`}ing in
#' @param stop.env the environment to stop at
#' @param all.names, same as \code{`ls`}
#' @param pattern same as \code{`ls`}
#' @param store.env NULL or environment, if the latter will populate that
#'    environment with all the objects found between \code{`env`} and
#'    \code{`stop.env`}
#' @return character or environment depending on \code{`store.env`}
#' @keywords internal

run_ls <- function(env, stop.env, all.names, pattern, store.env=NULL) {
  ls.res <- character()
  env.list <- list()
  i <- 0L
  max.envs <- getOption("unitizer.max.env.depth")
  if(!is.numeric(max.envs) || length(max.envs) != 1L || max.envs < 1)
    max.envs <- 20000L
  max.envs <- as.integer(max.envs)

  attempt <- try(
    # Get list of environments that are relevant
    while(!identical(env, stop.env)) {
      env.list <- c(env.list, env)
      env <- parent.env(env)
      if((i <- i + 1L) > max.envs)
        stop(
          "Logic error: not finding `stop.env` after ", max.envs,
          " iterations; contact package maintainer if this is an error."
        )
  } )
  if(inherits(attempt, "try-error"))
    stop("Specified `stop.env` does not appear to be in parent environments.")

  # Reverse, so when we copy objects the "youngest" overwrite the "eldest"
  for(i in rev(seq_along(env.list))) {
    ls.res <-
      c(ls.res, ls(envir=env.list[[i]], all.names=all.names, pattern=pattern))
    if(!is.null(store.env)) {
      for(j in seq_along(ls.res))
        assign(ls.res[[j]], get(ls.res[[j]], envir=env.list[[i]]), store.env)
      ls.res <- character()
  } }
  if(is.null(store.env)) sort(unique(ls.res)) else store.env
}

#' @export

print.unitizer_ls <- function(x, ...) {
  x.copy <- x
  x <- unclass(x)
  attr(x, "mods") <- NULL
  if(is.list(x)) {
    name.match <- c(
      tests="unitizer objects:",
      new="objects in new test env:",
      ref="objects in ref test env:"
    )
    names(x) <- name.match[names(x)]
  }
  val <- NextMethod()
  extra <- character()
  explain <- c(
    "'"="  ':  has changed since test evaluation",
    "*"="  *:  existed during test evaluation, but doesn't anymore",
    "**"="  **: didn't exist during test evaluation"
  )
  if(all(c("new", "ref") %in% names(x.copy)))
    extra <- c(extra, "Use `ref(.)` to access objects in ref test env")
  if(length(attr(x.copy, "mods"))) {
    extra <- c(extra, explain[attr(x.copy, "mods")])
  }
  if(length(extra)) cat(extra, sep="\n")
  invisible(val)
}

#' Clears ls Info and Marks as Invalid
#'
#' Useful when tests envs are repaired, or if we're looking at an ignored
#' test
#'
#' @keywords internal

setGeneric("invalidateLs", function(x, ...) standardGeneric("invalidateLs"))

setMethod("invalidateLs", "unitizerItems", valueClass="unitizerItems",
  function(x, ...) {
    x@.items <- lapply(as.list(x), invalidateLs)
    x
} )
setMethod("invalidateLs", "unitizerItem", valueClass="unitizerItem",
  function(x, ...) {
    x@ls <- x@ls[0,]             # ls potentially missleading
    attr(x@ls, "invalid") <- TRUE
    x
} )

