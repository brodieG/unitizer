#' @include item.R
#' @include item.sub.R
#' @include section.R
#' @include test.R
#' @include change.R

NULL

#' Contains All The Data for Our Tests!
#'
#' Generally is populated through the \code{+} methods, with the exception of
#' \code{items.ref}, which is added on creation.  I guess ideally all would be done
#' through different \code{+} methods, but that would complicate the process a bit
#' as that would require being able to distinguish between reference item lists and
#' new item lists (and the latter should never really be added as that should happen
#' item by item).  Maybe some day this will be cleaned up.
#'
#' One of the challenges when maintaining this is the tension between wanting to
#' keep all the item/test data in sub-objects, and the difficulty in extracting
#' summary data across all items/tests in that structure.  As a result of this,
#' a compromise solution has been to extract some of the relevant meta data
#' into vectors/matrices available at the top level (e.g. the @@tests.* objects).
#'
#' Ultimately, we need far more specialized accessor functions that don't require
#' understanding what those meta data mean exactly, and how they need to be used.
#' An example is the \code{ignored} function.
#'
#' Things get particularly complicated with the \code{browse} objects, which
#' basically rehash a lot of this data, but split into groups and sub-groups,
#' and at this point with meta-data stored in a \code{unitizerBrowseMapping}
#' object that replicates the role of the aforementioned @@tests.* objects in
#' \code{unitizer}.
#'
#' @keywords internal
#' @slot id the identifier for the unitizer, typically a file name, but can be anything
#' @slot items.new a list of all the tests in the new file
#' @slot items.ref a list of all the previously saved tests
#' @slot items.new.map a vector that maps the entries in \code{items.new} to
#'   those in \code{items.ref}, where position in vector is id/position in
#'   slot \code{items.new}, and value is id/position in \code{items.ref}
#'   new items will show up as NA here
#' @slot items.new.calls.deparse a character vector of the deparsed calls in \code{items.new}
#' @slot items.envs contains the environments for each call
#' @slot tests.fail vector highlighting which tests failed
#' @slot tests.new vector highlighting which tests did not exist in reference
#' @slot test.status a vector that contains the result of the test ("pass", "fail", "new", "indeterminable")
#'   for every item in \code{items.new}
#' @slot tests.result a logical matrix with a row for each item in \code{items.new} where each column
#'   represents the result of each sub tests
#' @slot tests.errorDetails an S4 object with a slot for each sub test, where the slot contains a
#'   \code{\link{unitizerItemTestError-class}} object
#'   either NULL or a character vector describing the test failure reason for every item in \code{items.new}
#' @slot items.ref.calls.deparse like \code{items.new.calls.deparse}, but for the reference items
#' @slot items.ref.map maps reference items to the new items; deleted items will
#'   show up as NA here, where position in vector is id/position in slot
#'   \code{items.ref}, and value is id/position in \code{items.new}
#' @slot sections a list of \code{\link{unitizerSection-class}}
#' @slot section.map a map of every item in \code{items.new} to a section
#' @slot changes contains summary data on the test results

setClass(
  "unitizer",
  representation(
    id="ANY",
    version="ANY",                # should really be 'package_version', but want to avoid setOldClass
    zero.env="environment",       # keep functions and stuff here
    base.env="environment",
    test.file.loc="character",    # location of teset file that produced `unitizer`
    eval.time="numeric",          # eval time for all tests in `unitizer`, computed in `+.unitizer.unitizerTestsOrExpression`

    items.new="unitizerItems",                         # Should all be same length
    items.new.map="integer",
    items.new.calls.deparse="character",
    items.envs="list",

    # NEED TO CLEAN THIS UP; SHOULD IT BE HANDLED BY METHODS? REALLY ANNOYING
    # TO GET FAILED TESTS VS NEW TESTS VS. WHATEVER

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

    sections.ref="list",
    section.ref.map="integer",   # Note, all section references for ref objects are parent sections since when we browse we don't track nested sections

    changes="unitizerChanges"              # Summary of user changes
  ),
  prototype(
    version=packageVersion("unitizer"),
    tests.status=factor(levels=c("Pass", "Fail", "Error", "New", "Deleted")),
    zero.env=baseenv(),
    test.file.loc=NA_character_,
    eval.time=0
  ),
  validity=function(object) {
    if(length(object@items.ref)) {
      ids <- vapply(as.list(object@items.ref), slot, integer(1L), "id")
      if(!identical(ids, seq_along(ids)))
        return("Non sequential ids in reference items.")
      if(length(ids) != length(object@section.ref.map))
        return("Reference section mapping error")
      if(!identical(length(test.file.loc), 1L))
        return("slot `test.file.loc` must be length 1L")
      if(
        !identical(length(eval.time), 1L) || is.na(eval.time) || eval.time < 0L
      )
        return("slot `eval.time` must be length 1L, positive, and not NA")

      # Randomly test a subset of the items for validity (testing all becomes
      # too time consuming)

      samp <- sample(
        seq_along(object@items.ref), min(5L, length(object@items.ref))
      )
      if(length(samp))
        item.check <- lapply(as.list(object@items.ref[samp]), isValid)
      if(any(is.chr <- vapply(item.check, is.character, logical(1L))))
        return(
          paste0(
            "Invalid reference item at index[", samp[which(is.chr)[[1L]]],
            "]: ", item.check[[which(is.chr)[[1L]]]], " (note we randomly ",
            "pick up to three reference tests to check validity)."
        ) )
    }
    TRUE
  }
)
setClass(
  "unitizerSummary",
  slots=c(data="matrix", dels="integer", totals="integer"),
  validity=function(object) {
    val.names <- c("New", "Pass", "Fail", "Error")
    if(
      !is.integer(object@data) ||
      !all(colnames(object@data) %in% val.names)
    )
      return(
        paste0(
          "Slot `data` must be an integer matrix with colnames %in% ",
          deparse(val.names)
      ) )
    if(length(object@dels) != 1L)
      return("Slot `dels` must be integer length one")
    if(!identical(names(object@totals), c(val.names, "Deleted")))
      return(
        paste0(
          "Slot `totals` must be integer with names ",
          deparse(c(val.names, "Deleted"))
      ) )
    TRUE
} )
setClass(
  "unitizerObjectList", contains="unitizerList",
  validity=function(object) {
    if(!all(vapply(object@.items, is, logical(1L), "unitizer")))
      return("slot `.items` may only contain \"unitizer\" objects")
    TRUE
} )
setClass(
  "unitizerObjectListSummary", contains="unitizerList",
  slots=c(test.files="character", totals="integer")
  validity=function(object) {
    if(!all(vapply(object@.items, is, logical(1L), "unitizerSummary")))
      return("slot `.items` may only contain \"unitizer\" objects")
    if(length(object@.items) != length(object@test.files))
      return("slot `items` and slot `test.files` must be same length")
    TRUE
} )
# - Methods -------------------------------------------------------------------

#' Display Unitizer Summary
#'
#' Unfortunately no choice but to use \code{getOptions("width")} from within
#' here.  Maybe could pre-compute in one of earlier stages and stuff into
#' \code{object}?  Not a big deal
#'
#' @keywords internal
#' @param object the object to show
#' @return NULL

setMethod("show", "unitizerSummary",
  function(object) {
    sum.mx <- object@data
    cols.padded <- paste0(
      vapply(
        max(vapply(colnames(sum.mx), nchar, integer(1L))) -
          vapply(colnames(sum.mx), nchar, integer(1L)),
        function(x) paste0(rep(" ", x + 1L), collapse=""),
        character(1L)
      ),
      colnames(sum.mx)
    )
    colnames(sum.mx) <- cols.padded
    mat.print <- capture.output(print(`rownames<-`(sum.mx, NULL)))
    if(length(mat.print) < 2L) {
      warning("Summary matrix has no data so it cannot be displayed", immediate.=TRUE)
    }
    dat.width <- nchar(sub("^\\[.*\\] ", " ", mat.print[[2]]))
    max.row.name.width <- max(
      getOption("width") - dat.width - 15L,
      15L
    )
    rownames(sum.mx) <- strtrunc(rownames(sum.mx), max.row.name.width)
    print(sum.mx)
    if(object@dels)
      word_cat(
        "\nAdditionally,", object@dels, "test",
        if(object@dels > 1) "were" else "was", "deleted"
      )
    NULL
} )

#' Determine if a \code{unitizer} Passed Based On Summary
#'
#' @keywords internal
#' @param object object to test for passing
#' @return logical(1L)

setGeneric("passed", function(object, ...) standardGeneric("passed"))
setMethod("passed", "unitizerSummary",
  function(object, ...) {
    !as.logical(sum(tail(object@data, 1L)[, -1L]) + object@dels)
} )
setMethod("initialize", "unitizer",
  function(.Object, ...) {
    .Object <- callNextMethod()
    .Object@tests.result <- tests_result_mat(0L)
    .Object@base.env <- new.env(parent=.Object@zero.env)
    parent.env(.Object@items.new@base.env) <- .Object@base.env
    parent.env(.Object@items.ref@base.env) <- .Object@base.env
    .Object
} )
#' Compute Length of a \code{\link{unitizer-class}} Object
#'
#' @keywords internal

setMethod("length", "unitizer",
  function(x) {
    len.vec <- unique(c(length(x@items.new), length(x@items.new.map), length(x@items.new.calls.deparse)))
    if(length(len.vec) != 1L) stop("Inconsistent sub-object length; should not happen; contact package maintainer.")
    len.vec
} )
#' Summarize Results
#'
#' Also prints to screen, but only if \code{level == 1L}
#'
#' @param object the object to summarize
#' @param silent whether to suppress display of summary object
#' @return a unitizerSummary object
#' @keywords internal

setMethod("summary", "unitizer",
  function(object, silent=FALSE, ...) {
    if(!isTRUE(silent) || !identical(silent, FALSE))
      stop("Argument `silent` must be TRUE or FALSE")
    ignore <- ignored(object@items.new)
    status <- object@tests.status[!ignore]
    sections <- vapply(
      object@section.parent[object@section.map[!ignore]],
      function(idx) object@sections[[idx]]@title,
      character(1L)
    )
    sections.levels <- unique(
      sections[order(object@section.parent[object@section.map[!ignore]])]
    )
    sum.mx <- tapply(
      rep(1L, length(status)),
      list(factor(sections, levels=sections.levels), status), sum
    )  # this should be a matrix with the summary data.
    sum.mx[] <- ifelse(is.na(sum.mx), 0L, sum.mx)
    total <- apply(sum.mx, 2, sum)

    sum.mx <- sum.mx[, colnames(sum.mx) != "Deleted", drop=FALSE]  # Pull out deleted since we don't actually what section they belong to since sections determined by items.new only
    sum.mx <- rbind(sum.mx, "**Total**"=total)

    if(sum(sum.mx[, "Error"]) == 0L)
      sum.mx <- sum.mx[, colnames(sum.mx) != "Error"]

    sum.mx <- sum.mx[as.logical(apply(sum.mx, 1, sum, na.rm=TRUE)),]  # Remove sections with no tests
    deletes <- length(
      Filter(is.na, object@items.ref.map[!ignored(object@items.ref)])
    )
    main.dat <- if(nrow(sum.mx) == 2L) {
      `rownames<-`(sum.mx[2L, , drop=F], "")
    } else sum.mx

    obj <- new("unitizerSummary", data=main.dat, dels=deletes, totals=totals)
    if(!silent) show(obj)
    obj
} )

setMethod("summary", "unitizerObjectList",
  function(object, silent=FALSE, ...) {
    # Now get summaries and loop through them

    obj.list <- as.list(object)
    summaries <- lapply(obj.list, summary, silent=TRUE)
    test.files <- vapply(obj.list, slot, character(1L), "test.file.loc")

    if(length(summaries)) {  # get aggregate results across all summaries
      totals <- rowSums(
        vapply(as.list(summaries), slot, summaries[[1L]]@totals, "totals")
      )
    } else totals <- integer()

    res <- new(
      "unitizerObjectListSummary", .items=summaries, test.files=test.files,
      totals=totals
    )
    if(!silent) show(res)
    res
} )

setMethod("show", "unitizerObjectListSummary",
  function(object) {
    test.len <- length(object)
    if(!test.len) return(invisible(NULL))
    scr.width <- getOption("width")
    test.files.trim <- col.names <- fmt <- test.nums <- NA_character_

    test.nums <- paste0(" ", format(seq.int(test.len)))
    test.files.trim <- file.path(
      str_reduce_unique(dirname(object@test.files)),
      basename(object@test.files)
    )
    tpl.summ <- object[[1L]]
    col.names.tmp <- names(tpl.summ@totals)
    col.names <- paste0(col.names.tmp, collapse=" ")
    non.file.chars <- nchar(colnames) + max(nchar(test.nums)) + 2L
    max.file.chars <- max(15L, scr.width - non.file.chars)
    pre.sum.chars <- max.file.chars + 2L + max(nchar(test.nums))
    header <- c(rep(" ", pre.sum.chars), col.names)
    num.width <- max(nchar(col.names.tmp))
    fmt <- paste0(
      "%s. %", max.file.chars, "s ",
      paste0(
        rep(paste0(" %", num.width, "s"), length(col.names.tmp)), collapse=""
      ), "\n"
    )
    cat(header, "\n", sep="")
    for(i in seq_along(object)) {
      test.num <- if(!passed(object[[i]])) {
        sub(" (\d+)", "*\\1", test.nums[[i]])
      } else test.nums[[i]]
      cat(sprintf(fmt, c(test.num, test.files.trim, object[[i]]@totals)))
    }
} )

setGeneric("registerItem", function(e1, e2, ...) standardGeneric("registerItem"))

#' Helper Methods for Adding Items to \code{\link{unitizer-class}} Object
#'
#' @aliases testItem,unitizer,unitizerItem-method
#' @seealso \code{\link{+,unitizer,unitizerItem-method}}
#' @keywords internal

setMethod("registerItem", c("unitizer", "unitizerItem"),
  function(e1, e2, ...) {
    item.new <- e2
    if(identical(length(e1@items.new), 0L)) e1@items.new@base.env <- parent.env(item.new@env)
    item.new@id <- length(e1@items.new) + 1L
    e1@items.new <- e1@items.new + item.new
    e1@items.new.calls.deparse <-
      c(e1@items.new.calls.deparse, call.dep <- deparse_call(item.new@call))
    if(length(e1@items.new.map) > 0L) {
      idx.vec <- seq_along(e1@items.ref.calls.deparse)
      items.already.matched <- e1@items.new.map[!is.na(e1@items.new.map)]
      items.already.matched.vec <- if(!length(items.already.matched)) TRUE else -items.already.matched
      item.map <- match(call.dep, e1@items.ref.calls.deparse[items.already.matched.vec])
      e1@items.new.map <- c(e1@items.new.map, item.map <- idx.vec[items.already.matched.vec][item.map])
    } else {
      e1@items.new.map <- c(e1@items.new.map, item.map <- match(call.dep, e1@items.ref.calls.deparse))
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
      if(length(item.new@data@conditions)) tests.conditions.new <- TRUE  # A new test with conditions by definition has new conditions
    } else {
      e1@items.ref.map[[item.map]] <- length(e1@items.new)
      item.ref <- e1@items.ref[[item.map]]
      section <- e1@sections[[e1@section.map[[length(e1@items.new)]]]]  # this should be initialized properly, and con probably be corrupted pretty easily

      # Test functions and the data to test is organized in objects with
      # the exact same structure as item.new@data, so cycle through the slots.
      # Status is always "Error" if something indeterminable happens,
      # if not and a failure happens, then it is "Fail", and if nothing goes wrong
      # for any of the slots, it is "Pass" (there is only one status for all slots)

      test.status <- "Pass"
      test.result <- test.result.tpl
      if(nrow(test.result) != 1L)
        stop("Logic Error: tpl matrix should be one row; contact maintainer.")

      get_dat <- function(x, i) {
        dat <- slot(x, i)
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
        test.res <- tryCatch(
          eval(test.call, e2@env),
          condition=function(e) structure(
            list(
              msg=conditionMessage(e), call=conditionCall(e),
              cond.class=class(e)
            ),
            class=c("testItemTestFail")
        ) )
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
          test.cond <- head(tail(test.res$cond.class, 2L), 1L)
          if(!length(test.cond)) test.cond <- "<unknown>"
          err.tpl@value <- paste0(
            err.msg, " signaled a condition of type \"", test.cond
            , "\", with message \"", test.res$msg, "\" and call `",
            paste0(deparse(test.res$call), collapse=""), "`."
          )
          err.tpl@compare.err <- TRUE
        } else if(is.character(test.res)) {
          if(identical(test.status, "Pass")) test.status <- "Fail"
          err.tpl@value <- test.res
        } else if(identical(test.res, FALSE)) {
          test.status <- "Fail"
          err.tpl@value <- paste0(err.msg, " found a mismatch")
        } else {
          test.status <- "Error"
          err.tpl@value <- paste0(
            err.msg, " returned something other than TRUE or character vector"
          )
          err.tpl@compare.err <- TRUE
        }
        test.error.tpl[[i]] <- err.tpl
        if(identical(i, "conditions")) {  #only failed/error tests get this far
          if(length(item.new@data@conditions))  # if a mismatch, and new conditions, we'll want to show these
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
          stop("Logic Error: impossible test status; contact package maintainer.")
        }
      } else {
        e1@tests.fail <- append(e1@tests.fail, FALSE)
        e1@tests.error <- append(e1@tests.error, FALSE)
      }
    }
    e1@tests.conditions.new <- c(e1@tests.conditions.new, tests.conditions.new)  # so added irrespective of pass/fail

    if(length(e1@tests.status)) {
      e1@tests.status <- unlist(list(e1@tests.status, factor(test.status, levels=levels(e1@tests.status))))
    } else {
      e1@tests.status <- factor(test.status, levels=levels(e1@tests.status))
    }
    e1 <- e1 + do.call(new, c(list("unitizerItemTestsErrors"), test.error.tpl))
    e1
} )

setGeneric("getName", function(object, ...) standardGeneric("getName"))
#' Create A Human Readable Name for a \code{unitizer}
#'
#' @param object a unitizer
#' @return character(1L) a descriptive name

setMethod("getName", "unitizer",
  function(object, ...) {
    if(!is.na(object@test.file.loc)) {   # unitize
      u.name <- basename(test.file)
    } else {
      if(is.character(object@id) && length(object@id) == 1L) {
        u.name <- object@id
      } else {
        u.name <- try(as.character(object@id), silent=TRUE)
        if(inherits(u.name, "try-error")) u.name <- "<unknown>"
    } }
    u.name
  }
)

