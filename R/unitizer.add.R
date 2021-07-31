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
#' @include unitizer.R

NULL

# Add a \code{`\link{unitizerSection-class}`} to a \code{`\link{unitizer-class}`}
#
# Registers the section, and the mapping of items to section.

#' @rdname unitizer_s4method_doc

setMethod("+", c("unitizer", "unitizerSection"), valueClass="unitizer",
  function(e1, e2) {
    # the map is index (item) to value (section), id auto-increments
    # with how many sections, and start tracks how many items exist in the
    # list; if we add a section, we basically blow away the section assignment
    # for the item that became a section, and replace it with the full section

    id <- length(e1@sections) + 1L
    start <- length(e1@items.new) + 1L # because sections added before items
    if(start == 1L & length(e1@section.map) == 0L) {
      e1@section.map <- rep(id, length(e2))
    } else {
      # If not initial section add, then must be a nested section, so have to
      # remove value

      # reduce length of section with nested unitizer section
      e1@sections[[e1@section.map[start]]]@length <-
        e1@sections[[e1@section.map[start]]]@length - 1L
      # remove mapping for the unitizer section element that we are expanding
      e1@section.map <- e1@section.map[-start]
      # add mapping for the now expanded section
      e1@section.map <- append(e1@section.map, rep(id, length(e2)), start - 1L)
    }
    e1@section.parent <- c(
      e1@section.parent, if(isTRUE(is.na(e2@parent))) id else e2@parent
    )
    if(
      e2@title %in%
        (titles <- vapply(e1@sections, function(x) x@title, character(1L)))
    ) {
      e2@title <- tail(make.unique(c(titles, e2@title)), 1L)
    }
    e1@sections <- append(e1@sections, list(e2))
    e1
} )
# Adds Expressions to unitizer
#
# Expressions can be added as \code{\link{unitizerTests-class}} object
# or a straight up expression, though in most cases it should be the
# latter.
#
# NOTE: you can only do this once for a \code{unitizer}.

#' @rdname unitizer_s4method_doc

setMethod("+", c("unitizer", "unitizerTestsOrExpression"), valueClass="unitizer",
  function(e1, e2) {
    start.time <- proc.time()

    if(length(e1@sections))
      # nocov start
      stop(
        "Internal Error: you are attempting to add more than one set ",
        "of tests or expressions to this `unitizer`"
      )
      # nocov end
    if(is.expression(e2)) e2 <- new("unitizerTests") + e2
    e1 <- e1 + new("unitizerSection", length=length(e2))
    matched.calls <- rep(NA_integer_, length(e2))
    i <- 1L
    sect.par <- NA_integer_
    # Used to track if there is an active section and to manage nested sections

    sect.end <- 0L

    test.env <- new.env(parent=e1@items.new@base.env)
    chr.width <- getOption("width")

    e1@global$cons <- new("unitizerCaptCons")
    old.warn <- getOption('warn')
    on.exit({
      # have to do this here because some test call this diretly
      options(warn=old.warn)
      close_and_clear(e1@global$cons)
    })

    repeat {
      if(done(e2 <- nextItem(e2))) break

      item <- exec(getItem(e2), test.env, e1@global)

      # If item is a section, added to the store and update the tests with the
      # contents of the section, and re-loop (this is how we handle nested
      #  tests), if not, store the evaluated test

      if(is(item@data@value[[1L]], "unitizerSectionExpression")) {
        sect.obj <- item@data@value[[1L]]
        if(i <= sect.end) {
          sect.end <- i + length(sect.obj) - 1L
          sect.par <- e1@section.parent[e1@section.map[[i]]]
        } else if (i > sect.end) {
          sect.end <- i + length(sect.obj) - 1L
          sect.par <- NA_integer_
        }
        e1 <- e1 + new(
          "unitizerSection", title=sect.obj@title,
          details=sect.obj@details, length=length(sect.obj),
          parent=sect.par, compare=sect.obj@compare
        )
        e2 <- e2 + sect.obj
        next
      }
      # record parent section id for when we create reference sections
      item@section.id <- e1@section.parent[[e1@section.map[[i]]]]
      # record name for attempting to match deleted tests to section
      item@section.name <- e1@sections[[item@section.id]]@title
      if(e1@show.progress > 2L)
        over_print(deparse(item@call)[[1L]], append=TRUE)

      e1 <- e1 + item  # store evaluated test and compare it to reference one

      # ignored items share environment with subsequent items

      if(!ignored(item)) test.env <- new.env(parent=test.env)

      i <- i + 1L
    }
    # Map reference tests to sections.  Tests that match directly are assigned
    # to the corresponding new section.  For deleted reference tests to new
    # sections, though we only map to parent sections, and match purely based on
    # section names

    e1@section.ref.map <- e1@section.map[e1@items.ref.map]
    e1@sections.ref <- e1@sections
    deleted <- which(is.na(e1@items.ref.map))

    if(length(deleted)) {
      sec.titles <- vapply(e1@sections, slot, character(1L), "title")
      sec.parents <- unique(e1@section.parent)
      par.titles <- sec.titles[sec.parents]

      for(i in deleted) {
        sec.match <- Filter(
          Negate(is.na), match(e1@items.ref[[i]]@section.name, par.titles)
        )
        if(identical(length(sec.match), 1L)) {
          e1@section.ref.map[[i]] <- sec.parents[[sec.match]]
        } else {
          e1@section.ref.map[[i]] <- NA_integer_
        }
    } }
    # Finalize

    if(e1@show.progress > 2L) over_print("")
    e1@eval.time <- (proc.time() - start.time)[["elapsed"]]
    on.exit()
    close_and_clear(e1@global$cons)
    e1@global$cons <- NULL
    e1
} )
# Adds \code{`\link{unitizerItems-class}`} objects to unitizer
#
# Any added \code{`\link{unitizerItems-class}`} objects are treated as
# reference items.  The only way to add new items is by adding each
# item individually with \code{`\link{+,unitizer,unitizerItem-method}`}.
#
# One aspect of copying reference items which isn't handled here is moving
# over the section data because this is kept at the \code{`\link{unitizer-class}`}
# level, not at the \code{`\link{unitizerItems-class}`} level.  The
# section copying is handled by \code{`\link{refSections,unitizer,unitizer-method}`}.
# This is something that we should clean-up eventually.

#' @rdname unitizer_s4method_doc

setMethod("+", c("unitizer", "unitizerItems"), valueClass="unitizer",
  function(e1, e2) {
    itemsType(e2) <- "reference"
    parent.env(e2@base.env) <- e1@base.env
    e1@items.ref <- e2
    if(length(e1@items.ref)) {
      e1@items.ref.calls.deparse <- vapply(
        as.list(e1@items.ref), slot, character(1L), "call.dep"
      )
      e1@items.ref.map <- rep(NA_integer_, length(e1@items.ref))
    }
    e1
  }
)
setGeneric("refSections", function(x, y) standardGeneric("refSections"))
# Extract Reference Section Data
#
# Using one unitizer with existing new items, and another unitizer that we
# just created from it by pulling out the tests we intend to keep, recreate
# the sections for the tests we intend to keep.
#
# This isn't super robust as we're not ensuring that the two unitizers used
# here are related in any way.  Would be better to have something that does
# this properly...
#
# @param x the new unitizer that will be stored with the reference tests
# @param y the unitizer that will be used to generate the sections

setMethod("refSections", c("unitizer", "unitizer"), valueClass="unitizer",
  function(x, y) {
    if(!length(x@items.ref)) return(x)

    sections.ref.ids <- vapply(as.list(x@items.ref), slot, 1L, "section.id")
    sections.unique <- Filter(Negate(is.na), sort(unique(sections.ref.ids)))
    if(!length(sections.unique)) return(x)
    if(!all(sections.unique %in% seq_along(y@sections))) {
      # nocov start
      stop(
        "Internal Error: reference tests referencing non-existing sections in ",
        "original; contact maintainer"
      )
      # nocov end
    }
    sects <- y@sections[sections.unique]
    sects.ranks <- rank(sections.unique, ties.method="first")
    sections.ref.mapped <- sects.ranks[match(sections.ref.ids, sections.unique)]

    sects.map <- ifelse(
      is.na(sections.ref.mapped),
      max(sections.unique) + 1L,
      sections.ref.mapped
    )
    if(na.sects <- sum(is.na(sections.ref.ids))) {
      na.sect <- new("unitizerSectionNA", length=na.sects)
      sects <- c(sects, list(na.sect))
    }
    x@sections.ref <- sects
    x@section.ref.map <- sects.map

    # Re-sequence ids so they map to our reference mapping

    for(i in 1:length(x@items.ref)) x@items.ref[[i]]@id <- i

    x
  }
)
# Adds \code{`\link{unitizerItem-class}`} to \code{`\link{unitizer-class}`}
#
# All tests are run on addition, and mapping information between reference and
# new tests is also recored.
#
# @seealso \code{`\link{registerItem,unitizer,unitizerItem-method}`}

#' @rdname unitizer_s4method_doc

setMethod("+", c("unitizer", "unitizerItem"),
  function(e1, e2) {
    e2 <- try(updateLs(e2, e1@items.new@base.env))
    if(inherits(e2, "try-error"))
      # nocov start
      stop(
        "Internal Error: unable to update LS for new item; contact maintainer."
      )
      # nocov end
    e1 <- registerItem(e1, e2)
    e1 <- testItem(e1, e2)
    e1
} )
# Add Test Errors to \code{`\link{unitizer-class}`}

#' @rdname unitizer_s4method_doc

setMethod("+", c("unitizer", "unitizerItemTestsErrors"),
  function(e1, e2) {
    e1@tests.errorDetails <- append(e1@tests.errorDetails, list(e2))
    e1
} )
