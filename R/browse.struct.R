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
#' @include unitizer.R
#' @include class_unions.R

NULL

# Prepares a \code{`unitizer`} Object for Review
#
# Mainly, splits up the tests by section and subsection and creates an indexing
# structure to keep track of what tests are in which section/subsection.  This
# simplifies implementation of non-linear navigation through the tests.
#
# @param start.at.browser used to force the review of the unitizer to start at
#   browser, useful when in review mode, or when all tests passed but user
#   elected to review unitizer anyway from the unitize_dir menu

setGeneric("browsePrep", function(x, mode, ...) standardGeneric("browsePrep"))
setMethod("browsePrep", c("unitizer", "character"), valueClass="unitizerBrowse",
  function(
    x, mode, hist.con=NULL, interactive=FALSE, start.at.browser=FALSE,
    use.diff=TRUE, ...
  ) {
    if(length(mode) != 1L || !mode %in% c("review", "unitize"))
      stop("Argument `mode` must be one of \"review\" or \"unitize\"")
    unitizer.browse <- new(
      "unitizerBrowse", mode=mode, hist.con=hist.con, interactive=interactive,
      global=x@global, start.at.browser=start.at.browser, use.diff=use.diff
    )
    # Assign the `show.diff` status to the errors, this isn't done when the
    # tests are evaluated.

    for(i in seq_along(x@tests.errorDetails))
      x@tests.errorDetails[[i]]@.use.diff <- use.diff

    # - Unitize ----------------------------------------------------------------

    # At some point need to rationalize this to a simpler instantiator for the
    # sub section objects since so much of the logic is similar

    if(identical(mode, "unitize")) {

      # Re-assign any ignored tests to be of the type of the next non-ignored
      # test, irrespective of what the ignored test was (only within a section)

      ign.test <- ignored(x@items.new)
      ign.split <- split(ign.test, x@section.map)
      ign.split.map.interim <- lapply(  # find next non-ignored in section
        ign.split,
        function(x) {
          oob <- length(x) + 1L
          id.seq <- seq_along(x)
          ids <- integer(length(x))
          ids <- ifelse(x, oob, id.seq)
          res <- rev(cummin(rev(ids)))
          res[res == oob] <- id.seq[res == oob]
          res
      } )
      ids.split <- split(seq_along(x@items.new), x@section.map)
      ign.map <- unlist(  # map back to non-ignored id
        lapply(
          seq_along(ids.split),
          function(x) ids.split[[x]][ign.split.map.interim[[x]]]
      ) )
      # Copy over non-ignored outcomes; one slot we don't change is
      # `tests.conditions.new` because we still want to use that to show errors
      # if they happen.

      fields.to.map <- c(
        "tests.fail", "tests.error", "tests.new", "tests.status", "tests.result"
      )
      for(i in seq_along(ign.map)[ign.test]) {
        for(j in fields.to.map) {
          if(is.matrix(slot(x, j))) {
            slot(x, j)[i, ] <- slot(x, j)[ign.map[i], ]
          } else {
            slot(x, j)[i] <- slot(x, j)[ign.map[i]]
      } } }
      # Add sub-sections

      rem.count.all <- 0L
      # Loop through parent sections
      for(i in sort(unique(x@section.parent))) {
        sect.par <- which(x@section.parent == i)
        # all items in parent section
        sect.map <- x@section.map %in% sect.par
        sect.map.ref <- which(
          is.na(x@items.ref.map) & !ignored(x@items.ref) &
          x@section.ref.map == i
        )
        rem.item.count <- length(sect.map.ref)
        rem.count.all <- rem.count.all + rem.item.count

        if(
          !sum(vapply(x@sections[sect.par], length, integer(1L))) &&
          !rem.item.count
        ) next
        browse.sect <- new(
          "unitizerBrowseSection", section.id=i,
          section.title=x@sections[[i]]@title
        )
        # Note: anything querying reference items has to go through items.new.map
        # since order isn't same.

        # Failed tests

        browse.sect <- browse.sect + new(
          "unitizerBrowseSubSectionFailed",
          show.out=TRUE, show.msg=TRUE,
          items.new=x@items.new[x@tests.fail & sect.map],
          show.fail=x@tests.errorDetails[x@tests.fail & sect.map],
          items.ref=x@items.ref[x@items.new.map[x@tests.fail & sect.map]],
          new.conditions=x@tests.conditions.new[x@tests.fail & sect.map],
          tests.result=x@tests.result[x@tests.fail & sect.map, , drop=FALSE]
        )
        # New tests

        browse.sect <- browse.sect + new(
          "unitizerBrowseSubSectionNew",
          show.msg=TRUE, show.out=TRUE,
          items.new=x@items.new[x@tests.new & sect.map],
          new.conditions=x@tests.conditions.new[x@tests.new & sect.map],
          tests.result=x@tests.result[x@tests.new & sect.map, , drop=FALSE]
        )
        # Corrupted tests

        browse.sect <- browse.sect + new(
          "unitizerBrowseSubSectionCorrupted",
          items.new=x@items.new[x@tests.error & sect.map],
          show.fail=x@tests.errorDetails[x@tests.error & sect.map],
          items.ref=x@items.ref[x@items.new.map[x@tests.error & sect.map]],
          new.conditions=x@tests.conditions.new[x@tests.error & sect.map],
          tests.result=x@tests.result[x@tests.error & sect.map, , drop=FALSE]
        )
        # Passed tests

        browse.sect <- browse.sect + new(
          "unitizerBrowseSubSectionPassed",
          items.new=x@items.new[x@tests.status == "Pass" & sect.map],
          items.ref=x@items.ref[
            x@items.new.map[x@tests.status == "Pass"  & sect.map]
          ],
          show.fail=FALSE,
          new.conditions=rep(F, sum(x@tests.status == "Pass" & sect.map)),
          tests.result=x@tests.result[
            x@tests.status == "Pass" & sect.map, , drop=FALSE
          ]
        )
        # Removed tests are a little funky b/c they are not part of the main
        # data array; by definition can't have new conditions on removed test

        browse.sect <- browse.sect + new(
          "unitizerBrowseSubSectionRemoved",
          items.ref=x@items.ref[sect.map.ref],
          new.conditions=rep(FALSE, rem.item.count),
          tests.result=tests_result_mat(rem.item.count)
        )
        # Add entire section

        unitizer.browse <- unitizer.browse + browse.sect
        NULL # SO above isn't last step in loop used for debugging
      }
      # Removed tests that couldn't be mapped
      rem.unmapped <- !ignored(x@items.ref) & is.na(x@section.ref.map) &
        is.na(x@items.ref.map)
      if(length(which(rem.unmapped))) {
        browse.sect <- new(
          "unitizerBrowseSection", section.id=0L,
          section.title=paste0(if(rem.count.all) "Other ", "Removed Items")
        )
        rem.item.count <- length(which(rem.unmapped))
        # by definition can't have new conditions on removed tests
        browse.sect <- browse.sect + new(
          "unitizerBrowseSubSectionRemoved",
          items.ref=x@items.ref[rem.unmapped],
          new.conditions=rep(FALSE, rem.item.count),
          tests.result=tests_result_mat(rem.item.count)
        )
        unitizer.browse <- unitizer.browse + browse.sect
      }
    } else if(identical(mode, "review")) {
    # - Review -----------------------------------------------------------------

      for(i in seq_along(x@sections.ref)) {  # Loop through parent sections
        # will have to check what the section numbers are, this might not be
        # right
        sect.map <- x@section.ref.map == i
        if(!length(which(sect.map))) next

        browse.sect <- new(
          "unitizerBrowseSection", section.id=i,
          section.title=x@sections.ref[[i]]@title
        )
        # Note: anything querying reference items has to go through items.new.map
        # since order isn't same.

        # Passed tests

        browse.sect <- browse.sect + new(
          "unitizerBrowseSubSectionPassed",
          items.new=x@items.ref[sect.map],
          show.msg=TRUE, new.conditions=rep(FALSE, sum(sect.map)),
          tests.result=tests_result_mat(sum(sect.map))
        )
        unitizer.browse <- unitizer.browse + browse.sect
        NULL # SO above isn't last step in loop used for debugging
      }
    } else stop("Internal Error: unexpected `mode`")  # nocov

    unitizer.browse
  }
)
setGeneric("bookmarked", function(x, ...) standardGeneric("bookmarked"))
setMethod("bookmarked", "unitizerObjectList", function(x, ...) {
  bookmarked <- vapply(
    x,
    function(y)
      is(y, "unitizer") && is(y@bookmark, "unitizerBrowseBookmark"),
    logical(1L)
  )
  if(!length(which(bookmarked)) %in% 0:1) {
    # nocov start
    stop(
      "Internal Error: no more than one unitizer may be bookmarked at any ",
      "given time; contact maintainer"
    )
    # nocov end
  }
  bookmarked
} )
# Keeps track of All Test Review Data
#
# Seemed like a brilliant idea to make this an object to simplify validation,
# but as result cycling through the items is incredibly annoying.  Need to
# develop better ways to iterate through each item while getting all the data
# here, as well as ways of easily knowing which sections/subsections are
# ignored.
#
# The real issue with all this stuff is that \code{`item.new`} and
# \code{`item.ref`} can be NULL, and whether on or the other or neither are
# NULL changes the processing logic.  Probably the thing to do is extract the
# non NULL values (i.e. item.main) and store them in a list, along with a
# list highlighting which of \code{`item.new`} or \code{`item.ref`} has been
# picked.
#
# @slot item.id unique, 1 incrementing up to total number of reviewable items
# @slot item.id.rel non-unique, unique within each sec/sub.sec
# @slot item.id.orig the original id of the item used to re-order tests in the
#   order they show up in the original files
# @slot item.ref whether a test is a reference test or not
# @slot reviewed whether a test has been reviewed
# @slot review.val what action the user decided ("N") is default

setClass("unitizerBrowseMapping",
  slots=c(
    item.id="integer",
    item.id.rel="integer",
    item.id.orig="integer",
    item.id.ord="integer",
    item.ref="logical",
    sec.id="integer",
    sub.sec.id="integer",
    reviewed="logical",
    review.val="character",
    review.def="character",
    review.type="factor",
    tests.result="matrix",
    ignored="logical",
    new.conditions="logical"
  ),
  prototype=list(
    review.type=factor(levels=c("New", "Passed", "Failed", "Removed", "Corrupted")),
    tests.result=tests_result_mat(0L)
  ),
  validity=function(object) {
    if(
      !identical(
        levels(object@review.type),
        c("New", "Passed", "Failed", "Removed", "Corrupted")
      ) || any(is.na(object@review.type))
    ) {
      return("Invalid slot `@review.type`")
    }
    if(any(is.na(object@item.ref))) {
      return("Invalid slot `@item.ref` must be logical and not NA")
    }
    TRUE
} )
# Helper Object for Browsing
#
# Key element here is the \code{`@@mapping`} slot which is generated by
# \code{`\link{+,unitizerBrowse,unitizerBrowseSection-method}`}, which allows us
# to navigate all the tests.

setClass("unitizerBrowse", contains="unitizerList",
  slots=c(
    mapping="unitizerBrowseMapping",
    last.id="integer",          # so that `reviewNext` knows what to show next
    # so that `reviewNext` knows what headers to display
    last.reviewed="integer",
    jumping.to="logical",       # indicate there was a re-eval jump
    hist.con="ANY",             # should be 'fileOrNULL', but setOldClass` issues
    mode="character",
    review="integer",           # counter to figure out when browse/review menu
    inspect.all="logical",      # force inspection of all elements
    # user has triggered at least one navigation command
    navigating="logical",
    browsing="logical",         # current test selected via browse
    human="logical",            # whether user has had any interaction at all
    # so navprompt can communicate back re-eval status
    re.eval="integer",
    force.up="logical",         # force update even if no changes
    interactive="logical",      # whether to browse in interactive mode
    # whether in non-interactive mode but required input
    interactive.error="logical",
    global="unitizerGlobal",    # object for global settings
    # indicate whether any auto-accepts were triggered
    auto.accept="logical",
    multi="logical",            # whether many unitizers are being browsed
    multi.quit="logical",       # whether many unitizers are being browsed
    # whether to show browser first, also disables warnings about reviewing
    # tests that are not usually reviewed
    start.at.browser="logical",
    use.diff="logical"          # Whether to use a diff in failure comparisons

  ),
  prototype=list(
    mapping=new("unitizerBrowseMapping"),
    last.id=0L,
    last.reviewed=0L,
    jumping.to=FALSE,
    hist.con=NULL,
    mode="unitize",
    review=1L,
    inspect.all=FALSE,
    navigating=FALSE,
    browsing=FALSE,
    human=FALSE,
    re.eval=0L,
    force.up=FALSE,
    interactive=FALSE,
    interactive.error=FALSE,
    auto.accept=FALSE,
    multi=FALSE,
    multi.quit=FALSE,
    start.at.browser=FALSE
  ),
  validity=function(object) {
    if(length(object@mode) != 1L || ! object@mode %in% c("unitize", "review")) {
      return("Slot `@mode` must be character(1L) in c(\"unitize\", \"review\")")
    }
    if(!is.TF(object@inspect.all))
      return("Slot `@inspect.all` must be TRUE or FALSE")
    if(!is.TF(object@start.at.browser))
      return("Slot `@start.at.browser` must be TRUE or FALSE")
    if(!is.TF(object@navigating))
      return("Slot `@navigating` must be TRUE or FALSE")
    if(!is.TF(object@browsing))
      return("Slot `@browsing` must be TRUE or FALSE")
    if(!is.TF(object@auto.accept))
      return("Slot `@auto.accept` must be TRUE or FALSE")
    if(!is.TF(object@jumping.to))
      return("Slot `jumping.to` must be TRUE or FALSE")
    if(!is.TF(object@force.up))
      return("Slot `force.up` must be TRUE or FALSE")
    if(length(object@re.eval) != 1L || !isTRUE(object@re.eval %in% 0:2))
      return("Slot `@re.eval` must be integer(1L) and in 0:2")
    if(!is.TF(object@multi))
      return("Slot `multi` must be TRUE or FALSE")
    TRUE
  }
)
# Display Summary of Tests and User Decisions
#
# Used to help navigate tests.  Will only show reviewed tests because
# implementing the ability to skip ahead has several annoying implications
# that we did not want to support (need to check that eventually all tests
# are reviewed, etc.)

#' @rdname unitizer_s4method_doc

setMethod("show", "unitizerBrowse", function(object) {
  obj.rendered <- as.character(object)
  cat(obj.rendered, "\n", sep="")
  invisible(obj.rendered)
} )

setGeneric("getIdOrder", function(object, ...) standardGeneric("getIdOrder"))
setMethod(
  "getIdOrder", "unitizerBrowse",
  function(object, ...) {
    # Figure out order as stuff showed up in original file; deletd reference ids
    # are put at the end.  Note the implicit assumption here is that the stuff in
    # sections is in the same order in file and here, which is almost certainly
    # true except for stuff outside of sections

    ids <- object@mapping@item.id.orig
    max.id.orig <- max(c(0L, ids[!object@mapping@item.ref]))
    if(any(object@mapping@item.ref)) {
      ids[object@mapping@item.ref] <-
        rank(ids[object@mapping@item.ref], ties.method="first") + max.id.orig
    }
    ids
  }
)

# Create a Text Representation of an Object
#
# @param object object to render
# @param width how many characters to display max per line, use 0L to use the
#   terminal window width as determined by \code{`getOption("width")`}; note
#   this is a guideline, if you pass numbers that lead to too narrow renderings
#   it will be ignored.
# @param ... not used
# @return character vector, one element per line, for use with e.g.
#   \code{`cat(x, sep=\n`)}

#' @rdname unitizer_s4method_doc

setMethod("as.character", "unitizerBrowse", valueClass="character",
  function(x, width=0L, ...) {
    if(!is.numeric(width) || width < 0 || length(width) != 1L) {
      stop("Argument `width` must be a positive scalar numeric.")
    }
    width <- as.integer(width)
    width.max <- if(width) width else getOption("width")
    # this used to limit what test were shown
    tests.to.show <- rep(TRUE, length(x@mapping@review.type))
    out.calls <- character(sum(tests.to.show))
    out.calls.idx <- integer(sum(tests.to.show))
    out.sec <- character(length(unique(x@mapping@sec.id[tests.to.show])))
    out.sec.idx <- integer(length(out.sec))
    out <- character(length(out.calls) + length(out.sec))

    # Work on figuring out all the various display lengths

    min.deparse.len <- 20L
    sec.id.prev <- 0L
    item.id.formatted <- format(justify="right",
      paste0(ifelse(x@mapping@ignored, "*", ""), x@mapping@item.id.ord)
    )
    num.pad <- ". "
    front.pad <- "  "
    rev.type <- format(as.character(x@mapping@review.type), justify="right")
    rev.fail.corr <- x@mapping@review.type %in% c("Failed", "Corrupted")
    rev.new <- x@mapping@review.type == "New"

    if(isTRUE(x@global$unitizer.opts[["unitizer.color"]])) {
      rev.type[rev.fail.corr] <- crayon::yellow(rev.type[rev.fail.corr])
      rev.type[rev.new] <- crayon::blue(rev.type[rev.new])
    }
    rev.type <- ifelse(!x@mapping@ignored, rev.type, "-")

    rev.type.n <- crayon::col_nchar(rev.type)
    rev.type.pad <- max(rev.type.n) - rev.type.n
    pads <-
      vapply(Map(rep, " ", rev.type.pad), paste0, collapse="", character(1L))

    review.formatted <- paste(sep=":",
      paste0(" ", pads, rev.type),
      format(
        ifelse(x@mapping@reviewed, as.character(x@mapping@review.val), "-")
      )
    )[tests.to.show]

    disp.len <- width.max - max(nchar(item.id.formatted)) -
      max(nchar(crayon::strip_style(review.formatted))) -
      nchar(num.pad) - nchar(front.pad)
    if(disp.len < min.deparse.len) {
      warning("Selected display width too small, will be ignored")
      disp.len <- min.deparse.len
    }
    j <- k <- l <- 0L

    # Display in order tests appear in file; note this is not in same order
    # as they show up in review (also, we're still really ordering by section)
    # first, and only then by original id

    id.ord <- x@mapping@item.id[order(x@mapping@sec.id, getIdOrder(x))]
    for(i in id.ord) {
      if(!tests.to.show[[i]]) next
      j <- j + 1L
      l <- l + 1L

      sec.id <- x@mapping@sec.id[[i]]
      sub.sec.id <- x@mapping@sub.sec.id[[i]]
      id.rel <- x@mapping@item.id.rel[[i]]

      item <- if(is.null(x[[sec.id]][[sub.sec.id]]@items.new[[id.rel]])) {
        x[[sec.id]][[sub.sec.id]]@items.ref[[id.rel]]
      } else {
        x[[sec.id]][[sub.sec.id]]@items.new[[id.rel]]
      }
      if(!identical(sec.id.prev, sec.id)) {
        k <- k + 1L
        out.sec[[k]] <- x[[sec.id]]@section.title
        out.sec.idx[[k]] <- l
        sec.id.prev <- sec.id
        l <- l + 1L
      }
      # Now paste the call together, substituting into the padding template
      call.dep <- paste0(one_line(item@call.dep, disp.len - 1L), " ")

      out.calls[[j]] <- call.dep
      out.calls.idx[[j]] <- l
    }
    # Combine all the call pieces, start by resizing template

    call.chrs <- nchar(out.calls)
    call.chrs.max <- max(call.chrs)
    tar.len <- min(disp.len, max(call.chrs.max + 3L, 15L))
    dot.pad <- substr(  # this will be the padding template
      paste0(rep(" . ", ceiling(tar.len / 3)), collapse=""), 1L, tar.len
    )
    calls.fin <- rep(dot.pad, length(call.chrs))
    substr(calls.fin, 1L, call.chrs) <- out.calls
    out.fin <- paste0(
      front.pad, item.id.formatted[id.ord], num.pad, calls.fin,
      review.formatted[id.ord], "\n"
    )
    # Now generate headers and interleave them

    out.width <- max(nchar(crayon::strip_style(out.fin))) - 1L
    out.sec.proc <- vapply(
      out.sec,
      function(x) as.character(H2(x), margin="none", width=out.width),
      character(1L)
    )
    out[out.calls.idx] <- out.fin
    out[out.sec.idx] <- out.sec.proc

    if(length(out.sec) == 1L) out[-out.sec.idx] else out
} )
#' @rdname unitizer_s4method_doc

setMethod(
  "as.data.frame", "unitizerBrowse",
  function(x, row.names = NULL, optional = FALSE, ...) {
    id.order <- getIdOrder(x)
    calls.dep <- deparseCalls(x)
    if(is.null(calls.dep)) calls.dep <- character()
    sec.titles <-
      vapply(x@mapping@sec.id, function(y) x[[y]]@section.title, character(1L))

    res <- data.frame(
      id=x@mapping@item.id,
      call=calls.dep,
      section=sec.titles,
      ignored=x@mapping@ignored,
      status=x@mapping@review.type,
      user=factor(x@mapping@review.val, levels=c("Y", "N")),
      reviewed=x@mapping@reviewed,
      stringsAsFactors=FALSE
    )[order(x@mapping@sec.id, id.order), ]
    rownames(res) <- NULL
    res
  }
)
# Indicate Whether to Exit Review Loop

setMethod("done", "unitizerBrowse", valueClass="logical",
  function(x, ...) {
    isTRUE(x@last.id >= max(x@mapping@item.id))
} )

# Based on User Input, Return Either Reference Or New Items
#
# Translates "Y", "N", etc. into c("A", "B", "C"), where "A" means return value
# from new item list, "B" return value from old item list (the original store)
# and "C" means return NULL.

setGeneric("processInput", function(x, ...) standardGeneric("processInput"))
setMethod("processInput", "unitizerBrowse", valueClass="unitizerItems",
  function(x, ...) {
    items <- new("unitizerItems")
    for(i in x@mapping@item.id) {
      # while it was nice to have mapping as an object for validation, this is
      # terrible

      sec <- x@mapping@sec.id[[i]]
      sub.sec <- x@mapping@sub.sec.id[[i]]
      id.rel <- x@mapping@item.id.rel[[i]]
      input <- x@mapping@review.val[[i]]
      input.translate <- x[[sec]][[sub.sec]]@actions[[input]]
      item <- switch(
        input.translate,
        A=x[[sec]][[sub.sec]]@items.new[[id.rel]],
        B=x[[sec]][[sub.sec]]@items.ref[[id.rel]],
        C=NULL
      )
      # Note here we over-write existing section.id because if we pick a reference
      # item, we still want to associate it with the section of the new item it
      # was matched to, unless we're dealing with a deleted item, in which case
      # there is no section

      if(!is.null(item)) {
        if(identical(as.character(x@mapping@review.type[[i]]), "Removed")) {
          sec <- NA_integer_
        }
        item@section.id <- sec
      }
      items <- items + item
    }
    items
} )
# Get id for unreviewed test

setGeneric("unreviewed", function(x, ...) standardGeneric("unreviewed"))
setMethod("unreviewed", "unitizerBrowse",
  function(x, ...) {
    unreviewed <- which(
      !x@mapping@reviewed & !x@mapping@ignored & (
        if(!identical(x@mode, "review")) x@mapping@review.type != "Passed"
        else TRUE
    ) )
    sort(unreviewed)
} )
# Represents a \code{`unitizer_sect`}

setClass("unitizerBrowseSection", contains="unitizerList",
  slots=c(
    section.id="integer",
    section.title="character",
    review.val="character"
) )

# Add Sections to Our Main Browse Object
#
# Primarily we're contructing the \code{`@@mapping`} slot which will then allow
# us to carry out requisite computations later.  See
# \code{`\link{unitizerBrowseMapping-class}`} for details on what each of the
# slots in \code{`mapping`} does.
#
# Also, some more discussion of this issue in the docs for \code{`\link{unitizer-class}`}.

#' @rdname unitizer_s4method_doc

setMethod("+", c("unitizerBrowse", "unitizerBrowseSection"), valueClass="unitizerBrowse",
  function(e1, e2) {
    e1 <- append(e1, list(e2))
    item.count <- unlist(lapply(as.list(e2), length))
    test.types <- unlist(lapply(as.list(e2), slot, "title"))
    max.item <- length(e1@mapping@item.id)
    max.sub.sec <- if(max.item) max(e1@mapping@sub.sec.id) else 0L

    # New items if available, ref items otherwise

    sec.item.list <- as.list(extractItems(e2))
    action.default <- vapply(as.list(e2), slot, character(1L), "action.default")

    mapping.new <- new("unitizerBrowseMapping",
      # This id tracks the order of tests as we intend to review them
      # is it possible for sum(item.count) to be zero?
      item.id=(max.item + 1L):(max.item + sum(item.count)),
      item.id.rel=unlist(lapply(item.count, function(x) seq(length.out=x))),
      item.id.orig=vapply(sec.item.list, slot, 1L, "id"),
      item.ref=vapply(sec.item.list, slot, FALSE, "reference"),
      sec.id=rep(length(e1), sum(item.count)),
      sub.sec.id=rep(
        seq_along(item.count), item.count
      ),
      review.val=rep(action.default, item.count),
      review.def=rep(action.default, item.count),
      reviewed=rep(FALSE, sum(item.count)),
      review.type=factor(
        rep(test.types, item.count),
        levels=levels(e1@mapping@review.type)
      ),
      ignored=unlist(lapply(as.list(e2), ignored)),
      new.conditions=unlist(lapply(as.list(e2), slot, "new.conditions")),  # get conditions from each sub-section
      tests.result=do.call(rbind, lapply(as.list(e2), slot, "tests.result"))
    )
    for(i in slotNames(e1@mapping)) {
      comb_fun <- if(is.matrix(slot(e1@mapping, i))) rbind else append
      slot(e1@mapping, i) <- comb_fun(slot(e1@mapping, i), slot(mapping.new, i))
    }
    # Update the id ord; this is a bit redundant as ideally we would just do it
    # once at the end, but since this function is used in different places we
    # just do it here and accept that it is a bit repetitive

    e1@mapping@item.id.ord <- getIdOrder(e1)
    e1
  }
)
# Represents A Section/Action Type when Browsing
#
# @keywords internal
# @slot items.new the new items associated with this sub sections
# @slot items.ref the reference items associated with this sub sections
# @slot title character 1 length current test types (failed, added, removed, corrupted)
# @slot prompt character 1 length what to prompt the user to do
# @slot actions character 2 length containing c("A", "B", "C"), where "A"
#   means return value from new item list, "B" return value from old item
#   list (the original store) and "C" means return NULL.  The first value
#   corresponds to the action on user typing `Y`, the second the action on
#   user typing `N`.
# @slot show.msg logical whether to automatically show stderr produced during
#   evaluation
# @slot show.out logical whether to automatically show stdout produced during
#   evaluation
# @slot show.fail FALSE, or a unitizerItemsTestsErrors-class object if you want
#   to show the details of failure
# @slot new.conditions whether the items produced new conditions

setClass("unitizerBrowseSubSection",
  slots=c(
    items.new="unitizerItemsOrNULL",
    items.ref="unitizerItemsOrNULL",
    title="character",
    prompt="character",
    detail.s="character",
    detail.p="character",
    help="character",
    actions="character",
    action.default="character",
    show.out="logical",
    show.msg="logical",
    show.fail="unitizerItemsTestsErrorsOrLogical",
    new.conditions="logical",
    tests.result="matrix"
  ),
  prototype=list(
    show.msg=FALSE, show.fail=FALSE, show.out=TRUE, action.default="N",
    show.msg=TRUE
  ),
  validity=function(object) {
    if(
      !is.null(object@items.ref) && !is.null(object@items.new) &&
      length(object@items.ref) != length(object@items.new)
    ) {
      return("Ref list must have the same number of items as new list, or be NULL")
    } else if(is.null(object@items.ref) && is.null(object@items.new)) {
      return("Reference and New Items cannot both be NULL")
    } else if (
      !is.character(object@actions) || !all(object@actions %in% c("A", "B", "C")) ||
      length(object@actions) != length(unique(object@actions)) ||
      is.null(names(object@actions)) | !all(names(object@actions) %in% c("Y", "N"))
    ) {
      return("`actions` input incorrect")
    } else if (!is.logical(object@show.out) || length(object@show.out) != 1L) {
      return("Argument `show.out` must be a 1 length logical")
    } else if (!is.logical(object@show.msg) || length(object@show.msg) != 1L) {
      return("Argument `show.msg` must be a 1 length logical")
    } else if (
      !is(object@show.fail, "unitizerItemsTestsErrors") &&
      !identical(length(object@show.fail), 1L)
    ) {
      return("Argument `show.fail` must be a 1 length logical or a \"unitizerItemsTestsErrors\" object")
    } else if (!is.character(object@prompt) || length(object@prompt) != 1L) {
      return("Argument `prompt` must be a 1 length character")
    } else if (!is.character(object@detail.s) || length(object@detail.s) != 1L) {
      return("Argument `detail.s` must be a 1 length character")
    } else if (
      !is.character(object@detail.p) || length(object@detail.p) != 1L ||
      is.na(object@detail.p) || !isTRUE(grepl("%s", object@detail.p))
    ) {
      return(
        "Argument `detail.p` must be character(1L), non-NA, and contain '%s'"
      )
    } else if (
      length(object@new.conditions) !=
      max(length(object@items.ref), length(object@items.new))
    ) {
      return("Argument `new.condtions` must be supplied and be the same length as the items.")
    } else if (any(is.na(object@new.conditions))) {
      return("Argument `new.conditions` may not contain any NA values.")
    } else if(
      !is.logical(object@tests.result) ||
      !identical(colnames(object@tests.result), slotNames("unitizerItemData"))
    ) {
      return(
        paste0(
          "Argument `tests.result` must be logical matrix with colnames equal ",
          "to slot names for `unitizerItemData`"
      ) )
    } else if(
      !identical(length(object@action.default), 1L) ||
      !length(which(object@action.default %in% c("Y", "N")))
    ) {
      return("Argument `action.default` must be \"Y\" or \"N\"")
    }
    TRUE
  }
)
# Compute Length of a \code{\link{unitizerBrowseSubSection-class}}

#' @rdname unitizer_s4method_doc

setMethod("length", "unitizerBrowseSubSection", valueClass="logical",
  function(x) max(length(x@items.new), length(x@items.ref))
)
setMethod("ignored", "unitizerBrowseSubSection", valueClass="logical",
  function(x, ...) {
    sub.sect <- if(is.null(x@items.new)) x@items.ref else x@items.new
    vapply(as.list(sub.sect), ignored, logical(1L))
} )
# Subset A \code{unitizerBrowse} Object
#
# Used primarily to confirm actions on multiple items.  Note this means ids
# are no longer continuous, something that we assume when we cycle through
# items.  Need to think about this a bit...
#
# Generally, be careful about using a subsetted browse object as you would
# a non-subsetted one until we get around to making cycling more robust.
#
# Finally, note that this conflicts with the underlying nature of a
# \code{unitizerList} since we're overriding the \code{[} method.  All of this
# is caused by the nested nature of sections and sub-sections, which is
# feeling like a worse design decision every time I look at it.  Note also that
# something like \code{ubobj[4]} and \code{ubobj[[4]]} will likely return
# completely different things as in the former we are subsetting based on the
# order implied by \code{ubobj@@mapping}, whereas in the latter we're directly
# pulling out an entire section.  Obviously not ideal, but since this is
# internal we're going to ignore the problem for now.

#' @rdname unitizer_s4method_doc

setMethod(
  "[",
  signature(x="unitizerBrowse", i="subIndex", j="missing", drop="missing"),
  function(x, i) {
    if(!is.numeric(i) || any(is.na(i)) || any(i < 0))
      stop("Argument `i` must be stricitly positive numeric")
    i <- as.integer(i)
    ub.new <- new("unitizerBrowse")
    if((length(i) == 1L) && !i || !any(i)) return(ub.new)
    if(!all(i %in% c(0L, x@mapping@item.id))) stop("Index out of bounds")

    id.ind <- match(i, x@mapping@item.id)

    # need to select all sections and subsections, even including empty ones?
    # won't for now, but need to think about whether this could cause problems

    id.df <- data.frame(
      i=x@mapping@sec.id, j=x@mapping@sub.sec.id, k=x@mapping@item.id.rel
    )[id.ind, ]

    ids.split <- lapply(
      split(id.df[-1L], id.df$i), function(x) split(x$k, x$j)
    )
    for(i in names(ids.split)) {
      ub.sec <- x[[as.integer(i)]][0L]    # get section with no contents

      # Cycle through selected sub-sections, and add them to our empty section
      # after subsetting them

      for(j in names(ids.split[[i]])) {
        ub.sec <- ub.sec +
          x[[as.integer(i)]][[as.integer(j)]][ids.split[[i]][[j]]]
      }
      # Now add section to new browser object

      ub.new <- ub.new + ub.sec
    }
    ub.new
} )
# Subset a \code{unitizerBrowseSubSection} Object

#' @rdname unitizer_s4method_doc

setMethod("[",
  signature(
    x="unitizerBrowseSubSection", i="subIndex", j="missing", drop="missing"
  ),
  function(x, i) {
    if(!is.numeric(i) || any(is.na(i)) || any(i < 0))
      stop("Argument `i` must be stricitly positive numeric")
    i <- as.integer(i)
    if(!all(i %in% 0:max(length(x)))) stop("Index out of bounds")
    new.sub <- new(class(x))

    # Unfortunately we have a hodgepodge of slots that need subsetting vs not
    # and no systematic way of knowing which is which

    subset.slots <- c(
      "items.new", "items.ref", "new.conditions", "show.fail", "tests.result"
    )
    for(s.name in slotNames(x)) {
      if(s.name %in% subset.slots) {
        slot.old <- slot(x, s.name)
        slot(new.sub, s.name) <-
          if(is.matrix(slot.old)) slot.old[i, , drop=FALSE] else slot.old[i]
      } else {
        slot(new.sub, s.name) <- slot(x, s.name)
      }
    }
    return(new.sub)
} )
# Pull Out Deparsed Calls From Objects
#
# Used primarily as a debugging tool, should probably be migrated to use
# \code{\link{extractItems}}
#
# @return character the deparsed calls

setGeneric("deparseCalls", function(x, ...) standardGeneric("deparseCalls"))
setMethod("deparseCalls", "unitizerBrowse",
  function(x, ...) {
    unlist(lapply(as.list(x), deparseCalls))
} )
setMethod("deparseCalls", "unitizerBrowseSection",
  function(x, ...) {
    unlist(lapply(as.list(x), deparseCalls))
} )
setMethod("deparseCalls", "unitizerBrowseSubSection",
  function(x, ...) {
    if(is.null(x@items.new) && is.null(x@items.ref)) return(character())
    items <- if(!is.null(x@items.new)) x@items.new else x@items.ref
    deparseCalls(items)
  }
)
setMethod("deparseCalls", "unitizerItems",
  function(x, ...) {
    vapply(as.list(x), slot, character(1L), "call.dep"
) } )
# Pull out items from unitizerBrowse objects

setGeneric("extractItems", function(x, ...) standardGeneric("extractItems"))
setMethod("extractItems", "unitizerBrowse", valueClass="unitizerItems",
  function(x, ...) {
    Reduce(append, lapply(as.list(x), extractItems))
  }
)
setMethod("extractItems", "unitizerBrowseSection", valueClass="unitizerItems",
  function(x, ...) {
    item.list <- lapply(
      as.list(x),
      function(y) {
        if(is.null(y@items.new) && is.null(y@items.ref)) return(new("unitizerItems"))
        if(!is.null(y@items.new)) y@items.new else y@items.ref
      }
    )
    Reduce(append, item.list)
  }
)
# Specific Sub-Section defaults

setClass("unitizerBrowseSubSectionFailed", contains="unitizerBrowseSubSection",
  prototype=list(
    title="Failed",
    prompt="Overwrite with new result%s",
    detail.s=paste0(
      "The following test failed because the new evaluation does not match ",
      "the reference value from the store."
    ),
    detail.p=paste0(
      "The %s tests in this section failed because the new evaluations do not ",
      "match the reference values from the store."
    ),
    help=paste0(
      "Tests fail when a test expression produces ",
      "different results than when it was originally added to the store. ",
      "You should type N at the prompt unless you know the previous result ",
      "is incorrect and should be replaced by the new result.\n\n",

      "Test failure in this case is caused by %s; see `?unitizer_sect` for ",
      "more details on what causes test failures and how to customize that ",
      "behavior.\n\n",

      "If you wish to examine test values more closely you can retrieve the ",
      "reference value with `.ref`, and the newly evaluated one with `.new`. ",
      "`.diff` contains a precomputed diff (i.e. ",
      "`diffobj::diffObj(.ref, .new)`). `.NEW` and `.REF` contain all stored ",
      "components of the test, and `.DIFF` contains the diffs ",
      "between each of those components.  `.new`, `.ref`, and `.diff` ",
      "are each respectively copies of `.NEW$value`, `.REF$value`, ",
      "and `.DIFF$value` provided for convenience.",

      "%s"
    ),
    actions=c(Y="A", N="B")
) )
setClass("unitizerBrowseSubSectionNew", contains="unitizerBrowseSubSection",
  prototype=list(
    title="New",
    prompt="Add test%s to store",
    detail.s="The following test is new.",
    detail.p="The %s tests in this section are new.",
    help=paste0(
      "A new test will be used as the reference value for future tests, so ",
      "make sure you review the value carefully before you add it to the ",
      "store by selecting 'Y' at the prompt.%s%s"
    ),
    actions=c(Y="A", N="C"), show.out=TRUE
) )
setClass("unitizerBrowseSubSectionCorrupted",
  contains="unitizerBrowseSubSection",
  prototype=list(
    title="Corrupted",
    prompt="Overwrite with new result%s",
    detail.s=paste0(
      "The test outcome for the following test cannot be assessed because ",
      "errors occurred while attempting comparison. Please review the errors ",
      "and contemplate using a different comparison function with ",
      "`unitizer_sect`."
    ),
    detail.p=paste0(
      "The test outcome for the %s tests in this section cannot be assessed ",
      "because errors occurred while attempting comparison. Please review the ",
      "errors and contemplate using a different comparison function with ",
      "`unitizer_sect`."
    ),
    help=paste0(
      "unitizer is unable to compare the reference and new test values ",
      "because the comparison function itself caused an error.  You can ",
      "change the unitizer function with `unitizer_sect`.  You can also ",
      "manually compare `.NEW` and `.REF` and decide whether to replace the ",
      "old value with the new one by selecting 'Y' at the prompt.%s%s"
    ),
    actions=c(Y="A", N="B")
) )
setClass("unitizerBrowseSubSectionRemoved", contains="unitizerBrowseSubSection",
  prototype=list(
    title="Removed",
    prompt="Remove test%s from store",
    detail.s=paste0(
      "The following test exists in the unitizer store but not in the new ",
      "test script."
    ),
    detail.p=paste0(
      "The %s tests in this section exist in the unitizer store but not in the ",
      "new test script."
    ),
    help=paste0(
      "A previously stored test no longer exists in the test file; you can ",
      "remove the stored value by selecting 'Y' at the prompt.%s%s"
    ),
    actions=c(Y="C", N="B")
) )
setClass("unitizerBrowseSubSectionPassed", contains="unitizerBrowseSubSection",
  prototype=list(
    title="Passed",
    prompt="Keep test%s in store",
    detail.s="The following test passed.",
    detail.p="The %s tests in this section passed.",
    actions=c(Y="A", N="C"),
    action.default="Y",
    show.out=TRUE
) )
# Add a browsing sub-section to a browse section
#
# @param e1 a \code{\link{unitizerBrowseSection-class}}
# @param e2 a \code{\link{unitizerBrowseSubSection-class}}
# @return a \code{\link{unitizerBrowseSection-class}}

#' @rdname unitizer_s4method_doc

setMethod("+", c("unitizerBrowseSection", "unitizerBrowseSubSection"),
  valueClass="unitizerBrowseSection",
  function(e1, e2) {
    e1 <- append(e1, list(e2))
  }
)
# Return value for \code{browseUnitizerInternal}

setClass(
  "unitizerBrowseResult",
  slots=c(
    unitizer="unitizer", re.eval="integer", updated="logical",
    interactive.error="logical", data="data.frame",
    bookmark="unitizerBrowseBookmarkOrNULL",
    multi.quit="logical"
  ),
  prototype=list(multi.quit=FALSE),
  validity=function(object) {
    if(
      !identical(length(object@re.eval), 1L) || is.na(object@re.eval) ||
      !object@re.eval %in% 0L:2L
    )
      return("slot `re.eval` must be integer(1L) in 0:2")
    if(!isTRUE(object@updated) && !identical(object@updated, FALSE))
      return("slot `updated` must be TRUE or FALSE")
    if(
      !isTRUE(object@interactive.error) &&
      !identical(object@interactive.error, FALSE)
    )
      return("slot `interactive.error` must be TRUE or FALSE")
    if(!isTRUE(dat.err <- is.unitizer_result_data(object@data)))
      return(paste0("slot `data` in unexpected format: ", dat.err))
    if(!is.TF(object@multi.quit))
      return("slot `multi.quit` must be TRUE or FALSE")

    TRUE
  }
)
