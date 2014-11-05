#' @include item.R
#' @include unitizer.R
#' @include class_unions.R

NULL

#' Prepares a \code{`unitizer`} Object for Review
#'
#' Mainly, splits up the tests by section and subsection and creates an indexing
#' structure to keep track of what tests are in which section/subsection.  This
#' simplifies implementation of non-linear navigation through the tests.
#'
#' @keywords internal

setGeneric("browsePrep", function(x, mode, ...) standardGeneric("browsePrep"))
setMethod("browsePrep", c("unitizer", "character"), valueClass="unitizerBrowse",
  function(x, mode, ...) {
    if(length(mode) != 1L || !mode %in% c("review", "unitize"))
      stop("Argument `mode` must be one of \"review\" or \"unitize\"")

    unitizer.browse <- new("unitizerBrowse", mode=mode)

    # - Unitize ----------------------------------------------------------------

    if(identical(mode, "unitize")) {
      for(i in unique(x@section.parent)) {                           # Loop through parent sections
        sect.map <- x@section.map %in% which(x@section.parent == i)  # all items in parent section
        if(
          sum(vapply(x@sections[which(x@section.parent == i)], length, integer(1L))) == 0L ||
          (
            length(which(x@tests.fail & sect.map)) == 0L &&
            length(which(x@tests.new & sect.map)) == 0L &&
            length(which(x@tests.error & sect.map)) == 0L
          )
        ) next
        browse.sect <- new(
          "unitizerBrowseSection", section.id=i,
          section.title=x@sections[[i]]@title
        )
        # Note: anything querying reference items has to go through items.new.map
        # since order isn't same.

        browse.sect <- browse.sect + new(                            # Failed tests
          "unitizerBrowseSubSectionFailed",
          items.new=x@items.new[x@tests.fail & sect.map],
          show.fail=x@tests.errorDetails[x@tests.fail & sect.map],
          items.ref=x@items.ref[x@items.new.map[x@tests.fail & sect.map]],
          new.conditions=x@tests.conditions.new[x@tests.fail & sect.map]
        )
        browse.sect <- browse.sect + new(                            # New tests
          "unitizerBrowseSubSectionNew",
          show.msg=TRUE, show.out=TRUE,
          items.new=x@items.new[x@tests.new & sect.map],
          new.conditions=x@tests.conditions.new[x@tests.new & sect.map]
        )
        browse.sect <- browse.sect + new(                            # Corrupted tests
          "unitizerBrowseSubSectionCorrupted",
          items.new=x@items.new[x@tests.error & sect.map],
          show.fail=x@tests.errorDetails[x@tests.error & sect.map],
          items.ref=x@items.ref[x@items.new.map[x@tests.error & sect.map]],
          new.conditions=x@tests.conditions.new[x@tests.error & sect.map]
        )
        browse.sect <- browse.sect + new(                            # Passed tests
          "unitizerBrowseSubSectionPassed",
          items.new=x@items.new[x@tests.status == "Pass" & sect.map],
          show.fail=FALSE,
          new.conditions=rep(F, sum(x@tests.status == "Pass" & sect.map))
        )
        unitizer.browse <- unitizer.browse + browse.sect
        NULL # SO above isn't last step in loop used for debugging
      }
      if(length(which(!ignored(x@items.ref[is.na(x@items.ref.map)])))) {  # Removed tests
        browse.sect <- new(
          "unitizerBrowseSection", section.id=0L,
          section.title="Removed Items"
        )
        browse.sect <- browse.sect + new(
          "unitizerBrowseSubSectionRemoved",
          items.ref=x@items.ref[is.na(x@items.ref.map) & !ignored(x@items.ref)],
          new.conditions=rep(FALSE, length(which(is.na(x@items.ref.map) & !ignored(x@items.ref)))) # by definition can't have new conditions on removed tests
        )
        unitizer.browse <- unitizer.browse + browse.sect
      }
    } else if(identical(mode, "review")) {
    # - Review -----------------------------------------------------------------

      for(i in seq_along(x@sections.ref)) {                   # Loop through parent sections
        sect.map <- x@section.ref.map == i   # will have to check what the section numbers are, this might not be right
        if(!length(which(sect.map))) next

        browse.sect <- new(
          "unitizerBrowseSection", section.id=i,
          section.title=x@sections.ref[[i]]@title
        )
        # Note: anything querying reference items has to go through items.new.map
        # since order isn't same.

        browse.sect <- browse.sect + new(                            # Passed tests
          "unitizerBrowseSubSectionPassed",
          items.new=x@items.ref[sect.map],
          show.fail=FALSE, new.conditions=rep(FALSE, sum(sect.map))
        )
        unitizer.browse <- unitizer.browse + browse.sect
        NULL # SO above isn't last step in loop used for debugging
      }
      message("did we handle tests without a section?")
    } else
      stop("Logic Error: unexpected `mode`")

    # - Finalize ---------------------------------------------------------------

    unitizer.browse
  }
)
#' Keeps track of All Test Review Data
#'
#' Seemed like a brilliant idea to make this an object to simplify validation,
#' but as result cycling through the items is incredibly annoying.  Need to
#' develop better ways to iterate through each item while getting all the data
#' here, as well as ways of easily knowing which sections/subsections are
#' ignored.
#'
#' The real issue with all this stuff is that \code{`item.new`} and
#' \code{`item.ref`} can be NULL, and whether on or the other or neither are
#' NULL changes the processing logic.  Probably the thing to do is extract the
#' non NULL values (i.e. item.main) and store them in a list, along with a
#' list highlighting which of \code{`item.new`} or \code{`item.ref`} has been
#' picked.
#'
#' @slot item.id unique, 1 incrementing up to total number of reviewable items
#' @slot item.id.rel non-unique, unique within each sec/sub.sec
#' @slot reviewed whether a test has been reviewed
#' @slot review.val what action the user decided ("N") is default
#' @keywords internal

setClass("unitizerBrowseMapping",
  slots=c(
    item.id="integer",
    item.id.rel="integer",
    sec.id="integer",
    sub.sec.id="integer",
    reviewed="logical",
    review.val="character",
    review.type="factor",
    ignored="logical",
    new.conditions="logical"
  ),
  prototype=list(
    review.type=factor(levels=c("New", "Passed", "Failed", "Removed", "Corrupted"))
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
    TRUE
} )
#' Helper Object for Browsing
#'
#' Key element here is the \code{`@@mapping`} slot which is generated by
#' \code{`\link{+,unitizerBrowse,unitizerBrowseSection-method}`}, which allows us
#' to navigate all the tests.
#' @keywords internal

setClass("unitizerBrowse", contains="unitizerList",
  slots=c(
    mapping="unitizerBrowseMapping",
    last.id="integer",         # used so that `reviewNext` knows what to show next
    last.reviewed="integer",   # used so that `reviewNext` knows what headers to display
    hist.con="ANY",            # should be 'fileOrNULL', but gave up on this due to `setOldClass` issues
    mode="character"
  ),
  prototype=list(
    mapping=new("unitizerBrowseMapping"),
    last.id=0L,
    last.reviewed=0L,
    hist.con=NULL,
    mode="unitize"
  ),
  validity=function(object) {
    if(length(object@mode) != 1L || ! object@mode %in% c("unitize", "review")) {
      return("Slot `@mode` must be character(1L) in c(\"unitize\", \"review\")")
    }
    TRUE
  }
)

#' Display Summary of Tests and User Decisions
#'
#' Used to help navigate tests.  Will only show reviewed tests because
#' implementing the ability to skip ahead has several annoying implications
#' that we did not want to support (need to check that eventually all tests
#' are reviewed, etc.)
#'
#' @keywords internal

setMethod("show", "unitizerBrowse", function(object) {
  obj.rendered <- as.character(object)
  cat(obj.rendered, sep="\n")
  invisible(obj.rendered)
} )
setGeneric("render", function(object, ...) standardGeneric("render"))

#' Create a Text Representation of an Object
#'
#' @keywords internal
#' @param object object to render
#' @param width how many characters to display max per line, use 0L to use the
#'   terminal window width as determined by \code{`getOption("width")`}; note
#'   this is a guideline, if you pass numbers that lead to too narrow renderings
#'   it will be ignored.
#' @param ... not used
#' @return character vector, one element per line, for use with e.g.
#'   \code{`cat(x, sep=\n`)}

setMethod("as.character", "unitizerBrowse", valueClass="character",
  function(x, width=0L, ...) {
    if(!is.numeric(width) || width < 0 || length(width) != 1L) {
      stop("Argument `width` must be a one length numeric.")
    }
    width <- as.integer(width)
    width.max <- if(width) width else getOption("width")
    tests.to.show <- !x@mapping@ignored & x@mapping@reviewed & (
      if(identical(x@mode, "unitize")) x@mapping@review.type != "Passed" else TRUE
    )
    out.calls <- character(sum(tests.to.show))
    out.calls.idx <- integer(sum(tests.to.show))
    out.sec <- character(length(unique(x@mapping@sec.id[tests.to.show])))
    out.sec.idx <- integer(length(out.sec))
    out <- character(length(out.calls) + length(out.sec))

    # Work on figuring out all the various display lengths

    min.deparse.len <- 20L
    disp.len <- width.max - 12L - 6L - max(nchar(x@mapping@item.id))
    if(disp.len < min.deparse.len) {
      warning("Selected display width too small, will be ignored")
      disp.len <- min.deparse.len
    }
    sec.id.prev <- 0L
    item.id.formatted <- format(x@mapping@item.id)
    review.formatted <- format(
      paste(x@mapping@review.type, x@mapping@review.val, sep=":"),
      justify="right"
    )[tests.to.show]
    j <- k <- l <- 0L

    for(i in x@mapping@item.id) {
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
      call.dep <- deparse_peek(item@call, disp.len)
      out.calls[[j]] <- paste0(
        "  ", item.id.formatted[[i]], ". ",
        call.dep, " "
      )
      out.calls.idx[[j]] <- l
    }
    # We now want to rpad the calls with hyphens.  We also don't want any calls
    # plus hyphens to end up narrower than `min.deparse.len`, so this gets a
    # bit messy

    out[out.calls.idx] <- paste0(
      out.calls,
      vapply(
        max(
          max(
            nchar(out.calls),
            min.deparse.len + 2L + max(nchar(item.id.formatted))
        ) )  - nchar(out.calls) + 1L,
        function(times) paste0(rep("-", times), collapse=""),
        character(1L)
      ), " ",
      review.formatted
    )
    out[out.sec.idx] <- out.sec
    if(length(out.sec) == 1L) out[-out.sec.idx] else out
} )
#' Indicate Whether to Exit Review Loop
#'
#' @keywords internal

setMethod("done", "unitizerBrowse", valueClass="logical",
  function(x, ...) {
    isTRUE(x@last.id >= max(x@mapping@item.id))
} )

#' Based on User Input, Return Either Reference Or New Items
#'
#' Translates "Y", "N", etc. into c("A", "B", "C"), where "A" means return value
#' from new item list, "B" return value from old item list (the original store)
#' and "C" means return NULL.
#'
#' @keywords internal

setGeneric("processInput", function(x, ...) standardGeneric("processInput"))
setMethod("processInput", "unitizerBrowse", valueClass="unitizerItems",
  function(x, ...) {
    items <- new("unitizerItems")
    for(i in x@mapping@item.id) {
      sec <- x@mapping@sec.id[[i]]        # while it was nice to have mapping as an object for validation, this is terrible
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
        if(
          identical(input.translate, "B") &&
          identical(as.character(x@mapping@review.type[[i]]), "Removed")
        ) {
          sec <- NA_integer_
        }
        item@section.id <- sec
      }
      items <- items + item
    }
    items
} )
#' Represents a \code{`unitizer_sect`}
#' @keywords internal

setClass("unitizerBrowseSection", contains="unitizerList",
  slots=c(
    section.id="integer",
    section.title="character"
) )

#' Add Sections to Our Main Browse Object
#'
#' Primarily we're contructing the \code{`@@mapping`} slot which will then allow
#' us to carry out requisite computations later.  See \code{`\link{unitizerBrowseMapping-class}`}
#' for details on what each of the slots in \code{`mapping`} does.
#'
#' Also, some more discussion of this issue in the docs for \code{`\link{unitizer-class}`}.
#'
#' @keywords internal

setMethod("+", c("unitizerBrowse", "unitizerBrowseSection"), valueClass="unitizerBrowse",
  function(e1, e2) {
    e1 <- append(e1, list(e2))
    item.count <- unlist(lapply(as.list(e2), length))
    test.types <- unlist(lapply(as.list(e2), slot, "title"))
    max.item <- length(e1@mapping@item.id)
    max.sub.sec <- if(max.item) max(e1@mapping@sub.sec.id) else 0L
    mapping.new <- new("unitizerBrowseMapping",
      item.id=(max.item + 1L):(max.item + sum(item.count)),
      item.id.rel=unlist(lapply(item.count, function(x) seq(len=x))),
      sec.id=rep(length(e1), sum(item.count)),
      sub.sec.id=rep(
        seq_along(item.count), item.count
      ),
      review.val=rep("N", sum(item.count)),       # Default Action is No so that when we quit early, only reviewed stuff is kept
      reviewed=rep(FALSE, sum(item.count)),
      review.type=factor(
        rep(test.types, item.count),
        levels=levels(e1@mapping@review.type)
      ),
      ignored=unlist(lapply(as.list(e2), ignored)),
      new.conditions=unlist(lapply(as.list(e2), slot, "new.conditions"))  # get conditions from each sub-section
    )
    for(i in slotNames(e1@mapping)) {
      slot(e1@mapping, i) <- append(slot(e1@mapping, i), slot(mapping.new, i))
    }
    e1
  }
)
#' Represents A Section/Action Type when Browsing
#'
#' @keywords internal
#' @slot items.new the new items associated with this sub sections
#' @slot items.ref the reference items associated with this sub sections
#' @slot title character 1 length current test types (failed, added, removed, corrupted)
#' @slot prompt character 1 length what to prompt the user to do
#' @slot actions character 2 length containing c("A", "B", "C"), where "A"
#'   means return value from new item list, "B" return value from old item
#'   list (the original store) and "C" means return NULL.  The first value
#'   corresponds to the action on user typing `Y`, the second the action on
#'   user typing `N`.
#' @slot show.msg logical whether to automatically show stderr produced during
#'   evaluation
#' @slot show.out logical whether to automatically show stdout produced during
#'   evaluation
#' @slot show.fail FALSE, or a unitizerItemsTestsErrors-class object if you want
#'   to show the details of failure
#' @slot new.conditions whether the items produced new conditions

setClass("unitizerBrowseSubSection",
  slots=c(
    items.new="unitizerItemsOrNULL",
    items.ref="unitizerItemsOrNULL",
    title="character",
    prompt="character",
    detail="character",
    actions="character",
    show.out="logical",
    show.msg="logical",
    show.fail="unitizerItemsTestsErrorsOrLogical",
    new.conditions="logical"
  ),
  prototype=list(show.msg=FALSE, show.fail=FALSE, show.out=FALSE),
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
    } else if (!is.character(object@detail) || length(object@detail) != 1L) {
      return("Argument `prompt` must be a 1 length character")
    } else if (
      length(object@new.conditions) !=
      max(length(object@items.ref), length(object@items.new))
    ) {
      return("Argument `new.condtions` must be supplied and be the same length as the items.")
    } else if (any(is.na(object@new.conditions))) {
      return("Argument `new.conditions` may not contain any NA values.")
    }
    TRUE
  }
)
#' Compute Length of a \code{`\link{unitizerBrowseSubSection-class}`}
#'
#' @keywords internal

setMethod("length", "unitizerBrowseSubSection", valueClass="logical",
  function(x) max(length(x@items.new), length(x@items.ref))
)
setMethod("ignored", "unitizerBrowseSubSection", valueClass="logical",
  function(x, ...) {
    sub.sect <- if(is.null(x@items.new)) x@items.ref else x@items.new
    vapply(as.list(sub.sect), ignored, logical(1L))
} )

#' Assemble Title for Display
#'
#' Uses \code{`title`} slot
#' @keywords internal


setGeneric("makeTitle", function(x, ...) standardGeneric("makeTitle"))
setMethod("makeTitle", "unitizerBrowseSubSection", valueClass="character",
  function(x, ...) paste0("Review ", length(x), " ", x@title, " Tests")
)

#' Specific Sub-Section defaults
#' @keywords internal

setClass("unitizerBrowseSubSectionFailed", contains="unitizerBrowseSubSection",
  prototype=list(
    title="Failed",
    prompt="Overwrite item in store with new value",
    detail="Reference test does not match new test from test script.",
    actions=c(Y="A", N="B")
) )
setClass("unitizerBrowseSubSectionNew", contains="unitizerBrowseSubSection",
  prototype=list(
    title="New",
    prompt="Add new item to store",
    detail="Test script contains tests not present in unitizer.",
    actions=c(Y="A", N="C"), show.out=TRUE
) )
setClass("unitizerBrowseSubSectionCorrupted", contains="unitizerBrowseSubSection",
  prototype=list(
    title="Corrupted",
    prompt="Overwrite item in store with new value",
    detail=paste0(
      "Reference tests cannot be compared to new tests because errors occurred ",
      "while attempting comparison. Please review the error and contemplate using ",
      "a different comparison function with `unitizer_sect`."
    ),
    actions=c(Y="A", N="B")
) )
setClass("unitizerBrowseSubSectionRemoved", contains="unitizerBrowseSubSection",
  prototype=list(
    title="Removed",
    prompt="Remove item from store",
    detail="The following test exists in unitizer but not in the new test script.",
    actions=c(Y="C", N="B")
) )
setClass("unitizerBrowseSubSectionPassed", contains="unitizerBrowseSubSection",
  prototype=list(
    title="Passed",
    prompt="Keep item in store",
    detail="The following test exists in unitizer but not in the new test script.",
    actions=c(Y="B", N="C")
) )
#' Add a browsing sub-section to a browse section
#'
#' @param e1 a \code{`\link{unitizerBrowseSection-class}`}
#' @param e2 a \code{`\link{unitizerBrowseSubSection-class}`}
#' @return a \code{`\link{unitizerBrowseSection-class}`}
#' @keywords internal

setMethod("+", c("unitizerBrowseSection", "unitizerBrowseSubSection"),
  valueClass="unitizerBrowseSection",
  function(e1, e2) {
    e1 <- append(e1, list(e2))
  }
)
