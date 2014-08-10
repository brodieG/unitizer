#' @include item.R
#' @include item.sub.R
#' @include section.R
#' @include test.R
#' @include change.R

NULL

#' Contains All The Data for Our Tests!
#' 
#' Generally is populated through the \code{`+`} methods, with the exception of 
#' \code{`items.ref`}, which is added on creation.  I guess ideally all would be done
#' through different \code{`+`} methods, but that would complicate the process a bit
#' as that would require being able to distinguish between reference item lists and
#' new item lists (and the latter should never really be added as that should happen
#' item by item).  Maybe some day this will be cleaned up.
#' 
#' @keywords internal
#' @slot id the identifier for the unitizer, typically a file name, but can be anything
#' @slot items.new a list of all the tests in the new file
#' @slot items.ref a list of all the previously saved tests
#' @slot items.new.map a vector that maps the entries in \code{`items.new`} to 
#'   those in \code{`items.ref`}, where position in vector is id/position in 
#'   slot \code{`items.new`}, and value is id/position in \code{`items.ref`}
#'   new items will show up as NA here
#' @slot items.new.calls.deparse a character vector of the deparsed calls in \code{`items.new`}
#' @slot items.envs contains the environments for each call
#' @slot tests.fail vector highlighting which tests failed
#' @slot tests.new vector highlighting which tests did not exist in reference
#' @slot test.status a vector that contains the result of the test ("pass", "fail", "new", "indeterminable")
#'   for every item in \code{`items.new`}
#' @slot tests.result a logical matrix with a row for each item in \code{`items.new`} where each column
#'   represents the result of each sub tests
#' @slot tests.errorDetails an S4 object with a slot for each sub test, where the slot contains a 
#'   \code{`\link{unitizerItemTestError-class}`} object
#'   either NULL or a character vector describing the test failure reason for every item in \code{`items.new`}
#' @slot items.ref.calls.deparse like \code{`items.new.calls.deparse`}, but for the reference items
#' @slot items.ref.map maps reference items to the new items; deleted items will 
#'   show up as NA here, where position in vector is id/position in slot 
#'   \code{`items.ref`}, and value is id/position in \code{`items.new`}
#' @slot sections a list of \code{`\link{unitizerSection-class}`}
#' @slot section.map a map of every item in \code{`items.new`} to a section
#' @slot changes contains summary data on the test results

setClass(
  "unitizer",
  representation(
    id="ANY",
    version="ANY",               # should really be 'package_version', but want to avoid setOldClass
    zero.env="environment",                          # keep functions and stuff here
    base.env="environment",

    items.new="unitizerItems",                         # Should all be same length
    items.new.map="integer",
    items.new.calls.deparse="character",
    items.envs="list",
    
    # NEED TO CLEAN THIS UP; SHOULD IT BE HANDLED BY METHODS? REALLY ANNOYING
    # TO GET FAILED TESTS VS NEW TESTS VS. WHATEVER

    tests.fail="logical",                     # really need tests.fail?
    tests.error="logical",                     # really need tests.error? redundant with tests.result
    tests.new="logical",
    tests.status="factor",                 # pass/fail/error/new
    tests.result="matrix",
    tests.errorDetails="unitizerItemsTestsErrors",

    items.ref="unitizerItems",                         # Should all be same length
    items.ref.calls.deparse="character",
    items.ref.map="integer",

    sections="list",
    section.map="integer",
    section.parent="integer",    # same length as sections, for each section links to parent

    changes="unitizerChanges"                  # Summary of user changes
  ),
  prototype(
    version=packageVersion("unitizer"),
    tests.status=factor(levels=c("Pass", "Fail", "Error", "New", "Deleted"))
) )

# - Methods -------------------------------------------------------------------

setMethod("initialize", "unitizer",
  function(.Object, ...) {
    if(!("id" %in% names(list(...)))) stop("Argument `id` is required")
    if(!("zero.env" %in% names(list(...)))) stop("Argument `zero.env` is required")
    allowable.args <- c("id", "changes", "zero.env")
    if(!all(names(list(...)) %in% allowable.args)) stop("Only arguments ", deparse(allowable.args), " are allowed.")
    .Object <- callNextMethod()
    slot.names <- slotNames(getClass("unitizerItemData"))
    .Object@tests.result <- matrix(logical(), ncol=length(slot.names), dimnames=list(NULL, slot.names))
    .Object@base.env <- new.env(parent=.Object@zero.env)
    parent.env(.Object@items.new@base.env) <- .Object@base.env
    parent.env(.Object@items.ref@base.env) <- .Object@base.env
    .Object
} )
#' Compute Length of a \code{`\link{unitizer-class}`} Object
#' 
#' @keywords internal

setMethod("length", "unitizer", 
  function(x) {
    len.vec <- unique(c(length(x@items.new), length(x@items.new.map), length(x@items.new.calls.deparse)))
    if(length(len.vec) != 1L) stop("Inconsistent sub-object length; should not happen; contact package maintainer.")
    len.vec
} )
#' Summarize Results
#' @keywords internal

setMethod("summary", "unitizer", 
  function(object, ...) {
    ignore <- ignored(object@items.new)
    status <- object@tests.status[!ignore]
    sections <- vapply(
      object@section.parent[object@section.map[!ignore]], 
      function(idx) object@sections[[idx]]@title,
      character(1L)
    )
    sections.levels <- unique(sections[order(object@section.parent[object@section.map[!ignore]])])
    sum.mx <- tapply(
      rep(1L, length(status)), 
      list(factor(sections, levels=sections.levels), status), sum
    )  # this should be a matrix with the summary data.
    # Not sure why we originally tried to leave in NAs for deleted
    # sum.mx.nondel <- sum.mx[, colnames(sum.mx) != "Deleted"]
    # sum.mx[, colnames(sum.mx) != "Deleted"] <- ifelse(is.na(sum.mx.nondel), 0L, sum.mx.nondel)
    sum.mx[] <- ifelse(is.na(sum.mx), 0L, sum.mx)
    sum.mx <- rbind(sum.mx, "**Total**"=apply(sum.mx, 2, sum))
    sum.mx["**Total**", "Deleted"] <- length(Filter(is.na, object@items.ref.map[!ignored(object@items.ref)]))
    if(sum(sum.mx[, "Error"]) == 0L) sum.mx <- sum.mx[, colnames(sum.mx) != "Error"]
    colnames(sum.mx) <- paste0(
      vapply(
        max(vapply(colnames(sum.mx), nchar, integer(1L))) - vapply(colnames(sum.mx), nchar, integer(1L)), 
        function(x) paste0(rep(" ", x + 1L), collapse=""), 
        character(1L)
      ),
      colnames(sum.mx)
    )
    sum.mx <- sum.mx[as.logical(apply(sum.mx, 1, sum, na.rm=TRUE)),]  # Remove sections with no tests
    rownames(sum.mx) <- strtrunc(rownames(sum.mx), 15)

    if(nrow(sum.mx) == 2L) sum.mx[2L, ] else sum.mx
} )

setGeneric("registerItem", function(e1, e2, ...) standardGeneric("registerItem"))

#' Helper Methods for Adding Items to \code{`\link{unitizer-class}`} Object
#' 
#' @aliases testItem,unitizer,unitizerItem-method
#' @seealso \code{`\link{+,unitizer,unitizerItem-method}`}
#' @keywords internal

setMethod("registerItem", c("unitizer", "unitizerItem"), 
  function(e1, e2, ...) {
    item.new <- e2
    if(identical(length(e1@items.new), 0L)) e1@items.new@base.env <- parent.env(item.new@env)
    item.new@id <- length(e1@items.new) + 1L
    e1@items.new <- e1@items.new + item.new
    e1@items.new.calls.deparse <- c(e1@items.new.calls.deparse, call.dep <- paste0(deparse(item.new@call), collapse=""))
    if(length(e1@items.new.map) > 0L) {
      idx.vec <- seq_along(e1@items.ref.calls.deparse)
      items.already.matched <- Filter(Negate(is.na), e1@items.new.map)
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
    slot.names <- slotNames(getClass(item.new@data))
    test.result.tpl <- logical(length(slot.names))
    names(test.result.tpl) <- slot.names
    test.error.tpl <- vector("list", length(slot.names))
    names(test.error.tpl) <- slot.names
    item.map <- tail(e1@items.new.map, 1L)

    if(is.na(item.map)) {
      test.status <- "New"
      e1@tests.fail <- c(e1@tests.fail, FALSE)      
      e1@tests.error <- c(e1@tests.error, FALSE)      
      e1@tests.new <- c(e1@tests.new, TRUE)      
      e1@tests.result <- rbind(e1@tests.result, test.result.tpl, deparse.level=0)
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

      for(i in slot.names) {
        test.res <- tryCatch(
          (slot(section@compare, i)@fun)(slot(item.ref@data, i), slot(item.new@data, i)),  # pull out and use compare function
          error=function(e) structure(conditionMessage(e), class=c("testItemTestFail"))
        )
        err.msg <- paste0("comparison function `", slot(section@compare, i)@fun.name, "`")
        if(inherits(test.res, "testItemTestFail")) {
          test.status <- "Error"
          test.error.tpl[[i]] <- new(
            "unitizerItemTestError", value=paste0(err.msg, " produced error: ", test.res), 
            compare.err=TRUE
          )
        } else if(isTRUE(test.res)) {
          test.result[[i]] <- TRUE
          next
        } else if(is.character(test.res)) {
          if(identical(test.status, "Pass")) test.status <- "Fail"
          test.error.tpl[[i]] <- new("unitizerItemTestError", value=test.res)
        } else if(identical(test.res, FALSE)) {
          test.status <- "Fail"
          test.error.tpl[[i]] <- new(
            "unitizerItemTestError", value=paste0(err.msg, " found a mismatch")
          )
        } else {
          test.status <- "Error"
          msg <- paste0(err.msg, " returned something other than TRUE or character vector")
          test.error.tpl[[i]] <- new("unitizerItemTestError", value=msg, compare.err=TRUE)
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
    if(length(e1@tests.status)) {
      e1@tests.status <- unlist(list(e1@tests.status, factor(test.status, levels=levels(e1@tests.status))))
    } else {
      e1@tests.status <- factor(test.status, levels=levels(e1@tests.status))
    }
    e1 <- e1 + do.call(new, c(list("unitizerItemTestsErrors"), test.error.tpl))
    e1
} )


