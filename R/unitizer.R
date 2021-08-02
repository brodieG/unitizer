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
#' @include item.sub.R
#' @include section.R
#' @include test.R
#' @include change.R
#' @include global.R

NULL

.unitizer.tests.levels <- c("Pass", "Fail", "New", "Deleted", "Error")

# Allow us to find a specific test based on deparse call and id
# `parse.mod` indicates whether the parse is not the same as it was when the
# bookmark was set, which indicates the bookmark may not be correct any more

setClass("unitizerBrowseBookmark",
  slots=c(call="character", id="integer", parse.mod="logical"),
  prototype=list(call="", id=0L, parse.mod=FALSE),
  validity=function(object) {
    if(!is.chr1(object@call))
      return("Slot `@call` must be character(1L) and not NA")
    if(!length(object@id) == 1L || object@id < 0L)
      return("Slot `@id` must be integer(1L) and positive")
    if(!is.TF(object@parse.mod))
      return("Slot `@parse.mode` must be TRUE or FALSE")
  }
)
setClassUnion(
  "unitizerBrowseBookmarkOrNULL", c("unitizerBrowseBookmark", "NULL")
)
# Contains All The Data for Our Tests!
#
# Generally is populated through the \code{+} methods, with the exception of
# \code{items.ref}, which is added on creation.  I guess ideally all would be
# done through different \code{+} methods, but that would complicate the
# process a bit as that would require being able to distinguish between
# reference item lists and new item lists (and the latter should never really
# be added as that should happen item by item).  Maybe some day this will be
# cleaned up.
#
# One of the challenges when maintaining this is the tension between wanting to
# keep all the item/test data in sub-objects, and the difficulty in extracting
# summary data across all items/tests in that structure.  As a result of this,
# a compromise solution has been to extract some of the relevant meta data
# into vectors/matrices available at the top level (e.g. the @@tests.*
# objects).
#
# Ultimately, we need far more specialized accessor functions that don't
# require understanding what those meta data mean exactly, and how they need
# to be used.  An example is the \code{ignored} function.
#
# Things get particularly complicated with the \code{browse} objects, which
# basically rehash a lot of this data, but split into groups and sub-groups,
# and at this point with meta-data stored in a \code{unitizerBrowseMapping}
# object that replicates the role of the aforementioned @@tests.* objects in
# \code{unitizer}.
#
# @keywords internal
# @slot id the identifier for the unitizer, typically a file name, but can be
#   anything
# @slot items.new a list of all the tests in the new file
# @slot items.ref a list of all the previously saved tests
# @slot items.new.map a vector that maps the entries in \code{items.new} to
#   those in \code{items.ref}, where position in vector is id/position in
#   slot \code{items.new}, and value is id/position in \code{items.ref}
#   new items will show up as NA here
# @slot items.new.calls.deparse a character vector of the deparsed calls in
#   \code{items.new}
# @slot items.new.calls.deparse integer() tracks what instance of a particular
#   deparse string this is to allow us to disambiguate duplicate tests for
# @slot items.envs contains the environments for each call
# @slot tests.fail vector highlighting which tests failed
# @slot tests.new vector highlighting which tests did not exist in reference
# @slot test.status a vector that contains the result of the test ("pass",
#   "fail", "new", "indeterminable")
#   for every item in \code{items.new}
# @slot tests.result a logical matrix with a row for each item in
#   \code{items.new} where each column represents the result of each sub tests
# @slot tests.errorDetails an S4 object with a slot for each sub test, where
#   the slot contains a \code{\link{unitizerItemTestError-class}} object
#   either NULL or a character vector describing the test failure reason for
#   every item in \code{items.new}
# @slot items.ref.calls.deparse like \code{items.new.calls.deparse}, but for
#   the reference items
# @slot items.ref.map maps reference items to the new items; deleted items will
#   show up as NA here, where position in vector is id/position in slot
#   \code{items.ref}, and value is id/position in \code{items.new}
# @slot sections a list of \code{\link{unitizerSection-class}}
# @slot section.map a map of every item in \code{items.new} to a section
# @slot changes contains summary data on the test results
# @slot upgraded.from character(1L) whether the unitizer was upgraded from load
#   and from what version, "" if it was not upgraded.
# @slot best.name character(1L) a friendlier name derived from test and unitizer
#   locations.
# @slot show.progress integer(1L) level of chattiness in progress reporting,
#   carried here as otherwise no way to feed it through the `+` method when
#   adding tests.

setClass(
  "unitizer",
  representation(
    id="ANY",
    version="character",            # should really be 'package_version', but want to avoid setOldClass, so use `as.character(packageVersion())` to populate
    zero.env="environment",         # keep functions and stuff here
    base.env="environment",
    test.file.loc="character",      # location of test file that produced `unitizer`
    # internal used during browsing to determine a re-eval instruction by user
    eval="logical",
    # eval time for all tests in `unitizer`, computed in
    # `+.unitizer.unitizerTestsOrExpression`
    eval.time="numeric",
    updated="logical",              # unitizer has been queued for update
    # should reflect whether a unitizer was modified at least once so that we
    # can report this in return values
    updated.at.least.once="logical",
    global="unitizerGlobalOrNULL",  # Global object used to track state

    items.new="unitizerItems",                         # Should all be same length
    items.new.map="integer",
    items.new.calls.deparse="character",
    items.new.calls.deparse.id="integer",
    items.envs="list",

    tests.fail="logical",                  # really need tests.fail?
    tests.error="logical",                 # really need tests.error? redundant with tests.result
    tests.new="logical",
    tests.status="factor",                 # pass/fail/error/new
    tests.result="matrix",
    tests.errorDetails="unitizerItemsTestsErrors",
    tests.conditions.new="logical",        # Whether the test produced new conditions, used to check whether we need to display conditions on ignored tests

    items.ref="unitizerItems",             # Should all be same length
    items.ref.calls.deparse="character",
    items.ref.map="integer",

    sections="list",
    section.map="integer",
    section.parent="integer",    # same length as sections, for each section links to parent, where parent is the outermost section a section is nested within

    # Note, all section references for ref objects are parent sections since
    # when we browse we don't track nested sections.  Usage of @section.ref.map
    # is a bit overloaded; we first try to re-assign any reference sections to
    # new sections if possible, and if not assign them NA; subsequently, before
    # we store the updated `unitizer`, we transfer all the newly generated
    # section info for the new reference tests here, so need to be careful about
    # how we interpret this.  We should clean this up at some point

    sections.ref="list",
    section.ref.map="integer",

    # "compressed" versions of the tracking data in @global

    state.new="unitizerGlobalTrackingStore",
    state.ref="unitizerGlobalTrackingStore",

    changes="unitizerChanges",       # Summary of user changes
    res.data="data.frameOrNULL",     # details of test evaluation and user review
    bookmark="unitizerBrowseBookmarkOrNULL",  # used for re-eval navigation

    # fields to support >1.4.14 functionality

    upgraded.from="character",       # was upgraded on load
    best.name="character",
    show.progress="integer"

  ),
  prototype(
    version=as.character(packageVersion("unitizer")),
    tests.status=factor(levels=.unitizer.tests.levels),
    base.env=baseenv(),
    zero.env=baseenv(),
    test.file.loc=NA_character_,
    eval=FALSE,
    eval.time=0,
    updated=FALSE,
    updated.at.least.once=FALSE,
    bookmark=NULL,
    global=unitizerGlobal$new(enable.which=character()),  # dummy so tests will run
    upgraded.from="",
    show.progress=0L
  ),
  validity=function(object) {
    if(!is.object(object@id) && is.character(object@id)) { # default id format
      # # No guarantees store id actually exists, so not enforcing this check
      # if(
      #   !file_test("-d", dirname(object@id)) ||
      #   !identical(dirname(object@id), normalize_path(dirname(object@id)))
      # ) {
      #   return(
      #     paste0(
      #       "slot `id` must be a properly normalized directory when using ",
      #       "default `unitizer` stores."
      #   ) )
      # }
    }
    if(
      !identical(length(object@eval.time), 1L) || is.na(object@eval.time) ||
      object@eval.time < 0L
    )
      return("slot `eval.time` must be length 1L, positive, and not NA")
    if(!is.TF(object@updated.at.least.once))
      return("slot `updated.at.least.once` must be TRUE or FALSE")
    if(!is.TF(object@updated))
      return("slot `updated` must be TRUE or FALSE")
    TRUE
  }
)
setClass(
  "unitizerSummary",
  slots=c(data="matrix", dels="integer", totals="integer"),
  validity=function(object) {
    if(
      !is.integer(object@data) ||
      !all(colnames(object@data) %in% .unitizer.tests.levels)
    )
      return(
        paste0(
          "Slot `data` must be an integer matrix with colnames %in% ",
          deparse(val.names)
      ) )
    if(length(object@dels) != 1L)
      return("Slot `dels` must be integer length one")
    if(!all(names(object@totals) %in% .unitizer.tests.levels))
      return(
        paste0(
          "Slot `totals` must be integer with names ",
          deparse(c(val.names))
      ) )
    TRUE
} )
setClass(
  "unitizerObjectList", contains="unitizerList",
  validity=function(object) {
    if(
      !all(
        vapply(
          object@.items,
          function(x) is(x, "unitizer") || is(x, "unitizerLoadFail"),
          logical(1L))
    ) )
      return(
        "slot `.items` may only contain \"unitizer\" or \"unitizerLoadFail\" ",
        "objects."
      )
    TRUE
  }
)
setClass(
  "unitizerObjectListSummary", contains="unitizerList",
  slots=c(test.files="character", totals="integer", updated="logical"),
  validity=function(object) {
    if(!all(vapply(object@.items, is, logical(1L), "unitizerSummary")))
      return("slot `.items` may only contain \"unitizerSummary\" objects")
    if(length(object@.items) != length(object@test.files))
      return("slot `.items` and slot `test.files` must be same length")
    if(length(object@.items) != length(object@updated))
      return("slot `.items` and slot `updated` must be same length")
    TRUE
} )
# - Methods -------------------------------------------------------------------

# Display Unitizer Summary
#
# Unfortunately no choice but to use \code{getOptions("width")} from within
# here.  Maybe could pre-compute in one of earlier stages and stuff into
# \code{object}?  Not a big deal
#
# @keywords internal
# @param object the object to show
# @return NULL

#' @rdname unitizer_s4method_doc

setMethod("show", "unitizerSummary",
  function(object) {
    sum.mx <- object@data
    rownames(sum.mx) <- strtrunc(rownames(sum.mx), 80L)
    cat(
      summ_matrix_to_text(sum.mx, show.nums=FALSE), "",
      sep="\n"
    )
    invisible(NULL)
} )

# Determine if a \code{unitizer} Passed Based On Summary
#
# @keywords internal
# @param object object to test for passing
# @return logical(1L)

setGeneric("passed", function(object, ...) standardGeneric("passed"))
setMethod("passed", "unitizerSummary",
  function(object, ...) !as.logical(sum(object@totals[-1L]))
)
#' @rdname unitizer_s4method_doc

setMethod("initialize", "unitizer",
  function(.Object, ...) {
    .Object <- callNextMethod()
    .Object@tests.result <- tests_result_mat(0L)

    # Re-use assigned @base.env if it isn't the baseenv(), since that means
    # user provided a base env.  in theory this should be a base.env that
    # already has for parent the `zero.env` because we're trying to recreate the
    # same environment chain of a different unitizer for when we re-use a
    # unitizer in unitize_dir

    if(identical(.Object@base.env, baseenv())) .Object@base.env <- new.env()

    parent.env(.Object@base.env) <- .Object@zero.env
    parent.env(.Object@items.new@base.env) <- .Object@base.env
    parent.env(.Object@items.ref@base.env) <- .Object@base.env
    .Object
} )
# Compute Length of a \code{\link{unitizer-class}} Object

#' @rdname unitizer_s4method_doc

setMethod("length", "unitizer",
  function(x) {
    len.vec <- unique(
      c(
        length(x@items.new), length(x@items.new.map),
        length(x@items.new.calls.deparse)
    ) )
    if(length(len.vec) != 1L)
      stop(
        "Inconsistent sub-object length; should not happen; contact maintainer."
      )
    len.vec
} )
# Summarize Results
#
# Also prints to screen, but only if \code{level == 1L}
#
# @param object the object to summarize
# @param silent whether to suppress display of summary object
# @return a unitizerSummary object
# @keywords internal

#' @rdname unitizer_s4method_doc

setMethod("summary", "unitizer",
  function(object, silent=FALSE, ...) {
    if(!isTRUE(silent) && !identical(silent, FALSE))
      stop("Argument `silent` must be TRUE or FALSE")
    ignore <- ignored(object@items.new)
    deleted <- which(!ignored(object@items.ref) & is.na(object@items.ref.map))
    status <- factor(
      c(
        as.character(object@tests.status[!ignore]),
        rep("Deleted", length(deleted))
      ),
      levels=levels(object@tests.status)
    )
    sec.ids <- object@section.parent[
      c(object@section.map[!ignore], object@section.ref.map[deleted])
    ]
    sec.unk <- "<unknown>"
    sections <- vapply(
      sec.ids,
      function(idx)
        if(is.na(idx)) sec.unk else object@sections[[idx]]@title,
      character(1L)
    )
    sections.levels <- unique(sections[order(sec.ids)])

    sum.mx <- tapply(
      rep(1L, length(status)),
      list(factor(sections, levels=sections.levels), status), sum
    )  # this should be a matrix with the summary data.
    sum.mx[is.na(sum.mx)] <- 0L
    total <- apply(sum.mx, 2, sum)

    # truly empty test file corner case

    if(!nrow(sum.mx)) {
      if(!length(status)) {
        cols <- length(levels(status))
        sum.mx <- matrix(
          integer(cols), nrow=1L, dimnames=list(sec.unk, colnames(sum.mx))
        )
      } else {
        # nocov start
        stop(
          "Internal Error: should not have statuses reported with no ",
          "sections; contact maintainer."
        )
        # nocov end
    } }
    obj <-
      new("unitizerSummary", data=sum.mx, dels=length(deleted), totals=total)
    if(!silent) show(obj)
    obj
} )
# Summary method

#' @rdname unitizer_s4method_doc

setMethod("summary", "unitizerObjectList",
  function(object, silent=FALSE, ...) {
    # Now get summaries and loop through them

    obj.list <- as.list(object)
    summaries <- lapply(obj.list, summary, silent=TRUE)
    test.files <- vapply(obj.list, slot, character(1L), "test.file.loc")
    updated <- vapply(obj.list, slot, logical(1L), "updated")

    if(length(summaries)) {  # get aggregate results across all summaries
      totals <- Reduce(`+`, lapply(as.list(summaries), slot, "totals"))
    } else totals <- integer()
    res <- new(
      "unitizerObjectListSummary", .items=summaries, test.files=test.files,
      totals=totals, updated=updated
    )
    if(!silent) show(res)
    res
} )
# Display method

#' @rdname unitizer_s4method_doc

setMethod("show", "unitizerObjectListSummary",
  function(object) {
    test.len <- length(object)
    if(!test.len) return(invisible(NULL))
    scr.width <- getOption("width")
    test.files.trim <- col.names <- fmt <- test.nums <- NA_character_

    # Adjust paths so we need only show their common part, and then get the
    # full name of the directory that they correspond to (as much as possible
    # anyway)

    test.files.trim <- unique_path(object@test.files)
    full.dir <- attr(test.files.trim, "common_dir")

    # Ignore any columns with zero totals other than pass/fail

    review.req <- !vapply(as.list(object), passed, logical(1L))

    # Display; if updated, mark with `NA` as we don't know what the deal is
    # until we re-run the tests

    totals <- t(vapply(as.list(object), slot, object[[1L]]@totals, "totals"))
    rownames(totals) <- test.files.trim
    totals[object@updated, ] <- NA_integer_
    disp <- summ_matrix_to_text(totals, from="left")

    # Post processing

    for(j in seq_along(disp)) {
      i <- j - 1L
      if(!i) next else if(i > nrow(totals)) break
      disp[[j]] <- if(object@updated[[i]]) {
        sub("^(\\s*) (\\d+\\.)", "\\1$\\2", disp[[j]])
      } else if(review.req[[i]]) {
        sub("^(\\s*) (\\d+\\.)", "\\1*\\2", disp[[j]])
      } else disp[[j]]
    }
    meta_word_cat(
      "Summary of files in common directory '", relativize_path(full.dir),
      "':\n\n", sep="", trail.nl=FALSE
    )
    meta_word_cat(disp, "", trail.nl=FALSE)

    # Legends

    if(any(review.req | object@updated))
      meta_word_cat("Legend:", trail.nl=FALSE)
    if(any(review.req & !object@updated))
      meta_word_cat("* `unitizer` requires review", trail.nl=FALSE)
    if(any(object@updated))
      meta_word_cat(
        "$ `unitizer` has been modified and needs to be re-run to",
        "recompute summary", sep=" ", trail.nl=FALSE
      )
    cat("\n")
    invisible(NULL)
} )
setGeneric(
  "registerItem", function(e1, e2, ...) standardGeneric("registerItem")
)

# Helper Methods for Adding Items to \code{\link{unitizer-class}} Object
#
# @aliases testItem,unitizer,unitizerItem-method
# @seealso \code{\link{+,unitizer,unitizerItem-method}}
# @keywords internal

setMethod("registerItem", c("unitizer", "unitizerItem"),
  function(e1, e2, ...) {
    item.new <- e2
    if(identical(length(e1@items.new), 0L))
      e1@items.new@base.env <- parent.env(item.new@env)
    item.new@id <- length(e1@items.new) + 1L
    e1@items.new <- e1@items.new + item.new
    e1@items.new.calls.deparse <-
      c(e1@items.new.calls.deparse, call.dep <- item.new@call.dep)
    e1@items.new.calls.deparse.id <- c(
      e1@items.new.calls.deparse.id, sum(e1@items.new.calls.deparse == call.dep)
    )
    if(length(e1@items.new.map) > 0L) {
      idx.vec <- seq_along(e1@items.ref.calls.deparse)
      items.already.matched <- e1@items.new.map[!is.na(e1@items.new.map)]
      items.already.matched.vec <-
        if(!length(items.already.matched)) TRUE else -items.already.matched
      item.map <-
        match(call.dep, e1@items.ref.calls.deparse[items.already.matched.vec])
      e1@items.new.map <- c(
        e1@items.new.map,
        item.map <- idx.vec[items.already.matched.vec][item.map]
      )
    } else {
      e1@items.new.map <- c(
        e1@items.new.map,
        item.map <- match(call.dep, e1@items.ref.calls.deparse)
      )
    }
    e1
} )
setGeneric("testItem", function(e1, e2, ...) standardGeneric("testItem"))
setMethod("testItem", c("unitizer", "unitizerItem"),
  function(e1, e2, ...) {
    item.new <- e2
    slot.names <- unitizerItemDataSlots
    test.result.tpl <- tests_result_mat(1L)
    test.error.tpl <- vector("list", length(slot.names))
    names(test.error.tpl) <- slot.names
    item.map <- tail(e1@items.new.map, 1L)
    tests.conditions.new <- FALSE

    if(is.na(item.map)) {
      test.status <- "New"
      e1@tests.fail <- c(e1@tests.fail, FALSE)
      e1@tests.error <- c(e1@tests.error, FALSE)
      e1@tests.new <- c(e1@tests.new, TRUE)
      e1@tests.result <- rbind(e1@tests.result, test.result.tpl)
      # A new test with conditions by definition has new conditions
      if(length(item.new@data@conditions)) tests.conditions.new <- TRUE
    } else {
      e1@items.ref.map[[item.map]] <- length(e1@items.new)
      item.ref <- e1@items.ref[[item.map]]

      # this should be initialized properly, and con probably be corrupted
      # pretty easily

      section <- e1@sections[[e1@section.map[[length(e1@items.new)]]]]

      # Test functions and the data to test is organized in objects with
      # the exact same structure as item.new@data, so cycle through the slots.
      # Status is always "Error" if something indeterminable happens,
      # if not and a failure happens, then it is "Fail", and if nothing goes
      # wrong for any of the slots, it is "Pass" (there is only one status for
      # all slots)

      test.status <- "Pass"
      test.result <- test.result.tpl
      if(nrow(test.result) != 1L)
        # nocov start
        stop("Internal Error: tpl matrix should be one row; contact maintainer.")
        # nocov end

      get_dat <- function(x, i) {
        dat <- if(identical(i, "value")) slot(x, i)[[1L]] else slot(x, i)
        if(is.call(dat) || is.symbol(dat)) call("quote", dat)
        else dat
      }
      for(i in slot.names) {
        comp.fun.name <- slot(section@compare, i)@fun.name
        comp.fun.anon <- isTRUE(is.na(comp.fun.name))
        if(comp.fun.anon) comp.fun.name <- "<anon.FUN>"
        item.new.dat <- get_dat(item.new@data, i)
        item.ref.dat <- get_dat(item.ref@data, i)

        if(comp.fun.anon) {
          test.call <- list(  # pull out and use compare function
            slot(section@compare, i)@fun, item.ref.dat, item.new.dat
          )
          mode(test.call) <- "call"
        } else {
          test.call <- call(  # pull out and use compare function
            comp.fun.name, item.ref.dat, item.new.dat
          )
        }
        # this is a bit roundabout b/c we added this hack in long after the
        # code was initially written; note we don't use `global` here b/c
        # we don't need to track state during comparison (but what happens
        # if user comparison function changes state??)
        #
        # We also don't use eval_with_capture which would be a natural thing to
        # do because that is slow.  We just keep writing to the dump file and
        # then will get rid of it at the end.  One possible issue here is that
        # we no longer detect sink issues caused by user possibly changing sinks
        # in the comparison function.

        sink(file=e1@global$cons@dump.c, append=TRUE)
        sink(type="message", file=e1@global$cons@dump.c, append=TRUE)
        on.exit({
          # nocov start emergency only
          sink(type="message")
          sink()
          # nocov end
        })
        res.tmp <- eval_user_exp(
          test.call, e2@env, global=NULL, with.display=FALSE
        )
        on.exit(NULL)
        sink(type="message")
        sink()

        cond <- res.tmp$conditions
        test.res <- if(length(cond)) {
          structure(
            list(
              msg=conditionMessage(cond[[1L]]), call=conditionCall(cond[[1L]]),
              cond.class=class(cond[[1L]])
            ),
            class=c("testItemTestFail")
          )
        } else {
          test.res <- res.tmp$value
        }
        if(isTRUE(test.res)) {
          test.result[1L, i] <- TRUE
          next
        }
        # Comparison failed

        err.tpl <- new(
          "unitizerItemTestError", .new=item.new.dat, .ref=item.ref.dat
        )
        err.msg <- paste0("comparison function `", comp.fun.name, "`")

        if(inherits(test.res, "testItemTestFail")) {
          test.status <- "Error"
          test.cond <- test.res$cond.class
          if(!length(test.cond)) test.cond <- "<unknown>"
          err.tpl@value <- paste0(
            err.msg, " signaled a condition of class `",
            deparse(test.cond, width.cutoff=500), "`",
            ", with message \"", test.res$msg, "\" and call `",
            paste0(deparse(test.res$call), collapse=""), "`."
          )
          err.tpl@compare.err <- TRUE
        } else if(is.character(test.res)) {
          if(identical(test.status, "Pass")) test.status <- "Fail"
          err.tpl@value <- test.res
        } else if(identical(test.res, FALSE)) {
          test.status <- "Fail"
          err.tpl@value <- ""
        } else {
          test.status <- "Error"
          err.tpl@value <- paste0(
            err.msg,
            " returned something other than TRUE, FALSE, or character vector ",
            sprintf("(%s of length %d)", typeof(test.res), length(test.res))
          )
          err.tpl@compare.err <- TRUE
        }
        test.error.tpl[[i]] <- err.tpl
        if(identical(i, "conditions")) {  #only failed/error tests get this far
          # if a mismatch, and new conditions, we'll want to show these
          if(length(item.new@data@conditions))
            tests.conditions.new <- TRUE
        }
      }
      e1@tests.result <- rbind(e1@tests.result, test.result)
      e1@tests.new <- c(e1@tests.new, FALSE)
      if(!all(test.result)) {
        if(identical(test.status, "Fail")) {
          e1@tests.fail <- append(e1@tests.fail, TRUE)
          e1@tests.error <- append(e1@tests.error, FALSE)
        } else if(identical(test.status, "Error")) {
          e1@tests.fail <- append(e1@tests.fail, FALSE)
          e1@tests.error <- append(e1@tests.error, TRUE)
        } else {
          # nocov start
          stop("Internal Error: impossible test status; contact maintainer.")
          # nocov end
        }
      } else {
        e1@tests.fail <- append(e1@tests.fail, FALSE)
        e1@tests.error <- append(e1@tests.error, FALSE)
      }
    }
    # so added irrespective of pass/fail
    e1@tests.conditions.new <- c(e1@tests.conditions.new, tests.conditions.new)

    if(length(e1@tests.status)) {
      e1@tests.status <- unlist(
        list(
          e1@tests.status, factor(test.status, levels=levels(e1@tests.status))
      ) )
    } else {
      e1@tests.status <- factor(test.status, levels=levels(e1@tests.status))
    }
    e1 <- e1 + do.call(new, c(list("unitizerItemTestsErrors"), test.error.tpl))
    e1
} )

setGeneric("getTarget", function(object, ...) standardGeneric("getTarget"))
setGeneric("getName", function(object, ...) standardGeneric("getName"))

# Create A Human Readable Names for a \code{unitizer}
#
# @keywords internal
# @param object a unitizer
# @return character(1L) a descriptive name

setMethod("getTarget", "unitizer",
  function(object, ...) {
    id <- try(object@id, silent=TRUE)
    if(inherits(id, "try-error")) {
      return("<unknown store id>")
    }
    relativize_path(as.store_id_chr(id))
} )
# @rdname getTarget,unitizer-method

setMethod("getName", "unitizer",
  function(object, ...) {
    f.name <- try(object@test.file.loc, silent=TRUE)
    if(
      inherits(f.name, "try-error") ||
      !is.chr1plain(f.name) || is.na(f.name)
    ) {
      return(getTarget(object))
    }
    relativize_path(f.name)
} )

#' @rdname unitizer_s4method_doc

setMethod("as.character", "unitizer",
  function(x, ...) {
    name <- try(getName(x))
    name.fin <- if(inherits(name, "try-error")) "<name retrieval failure>" else
      name
    sprintf("unitizer for '%s'", pretty_path(name))
} )

