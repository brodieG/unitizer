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

#' @include unitizer.R

NULL

#' Return Values and Related Methods for \code{unitize} Functions
#'
#' \code{unitize} and related functions are run primarily for the interactive
#' environment they provide and for their side effects (updating stored
#' \code{unitizer} objects), but the return values may be useful under some
#' circumstances if you need to retrieve test status, user selections, etc..
#'
#' \code{unitize} and \code{review} return a \code{unitizer_result} S3 object.
#' This is a data frame that contains details about the status of each test.
#' \code{unitize_dir} returns a \code{unitize_results} S3 object, which is a
#' list of \code{unitize_result}  objects.
#'
#' Both \code{unitize_results} and \code{unitize_result} have \code{print}
#' methods documented here. In addition to the \code{print} methods, both of
#' the result objects have \code{\link{get_unitizer}} methods so that you can
#' retrieve the stored \code{unitizer} objects.
#'
#' Please note that with \code{unitize_dir} you can re-review a single
#' \code{unitizer} several times during during a single call to
#' \code{unitize_dir}.  This is to allow you to re-evaluate specific
#' \code{unitizers} easily without having to re-run the entire directory again.
#' Unfortunately, as a result of this feature, the return values of
#' \code{unitize_dir} can be misleading because they only report the result of
#' the last review cycle.
#'
#' Additionally, \code{unitize_dir} will report user selections during the last
#' review even if in the end the user chose not to save the modified
#' \code{unitizer}.  You will be alerted to this by an onscreen message from the
#' \code{print} method (this is tracked in the "updated" attribute of the
#' \code{unitizer_result} object).  Finally, if in the last iteration before
#' exit you did not save the \code{unitizer}, but you did save it in  previous
#' review cycles in the same \code{unitize_dir} call, the displayed selections
#' and test outcomes will correspond to the last unsaved iteration, not the
#' one that was saved.  You will be alerted to this by an on-screen message
#' (this is tracked through the "updated.at.least.once" attribute of the
#' \code{unitizer_result} object).
#'
#' @name unitizer_result
#' @rdname unitizer_result
#' @aliases unitizer_results
#' @seealso \code{\link{unitize}}, \code{\link{get_unitizer}}
#' @param x the object to print
#' @param ... extra arguments for print generic
#' @return \code{x}, invisibly

NULL

#' @export
#' @rdname unitizer_result

print.unitizer_result <- function(x, ...) {
  if(!isTRUE(fail <- is.unitizer_result(x))) stop(fail)

  x$call <- strtrunc(x$call, 30L)
  x$section <- if(length(unique(x$section)) > 1L) strtrunc(x$section, 15L)
  x$ignored <- factor(ifelse(x$ignored, "*", ""), levels=c("", "*"))

  store.char <- if(is.chr1plain(attr(x, "store.id"))) {
    pretty_path(attr(x, "store.id"))
  } else {
    att <- try(as.character(attr(x, "store.id")), silent=TRUE)
    if(inherits(att, "try-error")) "<untranslateable store.id>" else att
  }
  word_cat("Test File: ", pretty_path(attr(x, "test.file")), "\n", sep="")
  word_cat("Store ID: ", store.char, "\n\n", sep="")
  res <- NextMethod(x, ...)
  if(!isTRUE(attr(x, "updated")))
    word_cat(
      "\nYou chose NOT to save these changes to the unitizer store\n"
    )
  invisible(res)
}
#' @export
#' @rdname unitizer_result

print.unitizer_results <- function(x, ...) {
  if(
    !inherits(x, "unitizer_results") || !is.list(x) ||
    !all(
      vapply(
        x,
        function(y) is(y, "unitizerLoadFail") || isTRUE(is.unitizer_result(y)),
        logical(1L)
  ) ) )
    stop(
      "Argument `x` must be a \"unitizer_results\" list containing only ",
      "`unitizerLoadFail` or `unitizer_result` objects"
    )
  if(!length(x)) {
    meta_word_cat("No unitizers")
    return(invisible(NULL))
  }
  failed <- vapply(x, is, logical(1L), "unitizerLoadFail")
  which.fail <- which(failed)
  which.pass <- which(!failed)
  files <- # extract both S4 slot and S3 attribute...
    vapply(x, function(y) pretty_path(attr(y, "test.file")), character(1L))
  files.short <- unique_path(files)
  files.dir <- attr(files.short, "common_dir")
  updated <- TRUE

  if(length(which.pass)) {
    # Looking at non-ignored only, compute counts in each category, and how many
    # of them the user selected Y for; vals will be a 3D Array

    vals <- vapply(
      x[which.pass],
      function(y) {
        y2 <- y[!y$ignored, ]
        counts <- tapply(y2$status, y2$status, length)
        yesses <- tapply(y2$user == "Y", y2$status, sum)
        counts[is.na(counts)] <- 0L
        yesses[is.na(yesses)] <- 0L
        rbind(yesses=yesses, counts=counts)
      },
      matrix(integer(1L), 2L, length(levels(x[[which.pass[[1L]]]]$status)))
    )
    # Compute which columns to display (always show first column); note we reduce
    # vals to a matrix by selecting only the "counts" values of the first dim

    to.show <- unique(which(!!rowSums(vals["counts", , ,drop=TRUE])))
    if(!length(to.show)) to.show <- 1L

    # Now collapse into string form

    as.frac <- function(y) {
      setNames(
        c(
          paste0(y["yesses", ,drop=F], "/", y["counts", , drop=F]),
          paste0(rowSums(y), collapse="/")
        ),
        c(colnames(y), "Totals")
    ) }
    vals.char <- apply(vals[, to.show, ,drop=FALSE], 3L, as.frac)
    tots.char <- as.frac(apply(vals[, to.show, ,drop=FALSE], 1L:2L, sum))

    count.mx <- t(
      cbind(
        vals.char, tots.char,
        deparse.level=0L
    ) )
    # drop totals if superflous

    if(ncol(count.mx) == 2L) count.mx <- count.mx[, -2L, drop=FALSE]
    if(nrow(count.mx) == 2L) count.mx <- count.mx[-2L, , drop=FALSE]

    # pad col names for equal width

    max.width <- max(c(nchar(count.mx), nchar(dimnames(vals)[[2L]])))
    colnames(count.mx) <-
      sprintf(paste0("%", max.width, "s"), colnames(count.mx))

    # Combine with file names and totals

    fin.mx <- cbind(
      test.file=c(files.short[which.pass], "Totals"),
      count.mx
    )
    fin.df <- cbind(id=c(which.pass, 0L), as.data.frame(fin.mx))
    fin.out <- capture.output(print(fin.df, row.names=FALSE))

    # Mark any non-updated tests

    updated <-
      vapply(
        x[which.pass],
        function(x)
        if(isTRUE(attr(x, "updated"))) 3L else
        if(isTRUE(attr(x, "updated.at.least.once"))) 2L else 1L,
        integer(1L)
      )
    updated.mark <- c("*", "$", " ")
    if(any(updated < 3L)) fin.out[-c(1L, length(fin.out))] <-
      paste(fin.out[-c(1L, length(fin.out))], updated.mark[updated])

    word_cat(
      "Summary of tests (accept/total):\n",
      head(fin.out, -1L), paste0(rep("-", max(nchar(fin.out))), collapse=""),
      tail(fin.out, 1L),
      if(any(updated < 3L)) "\n",
      sep="\n"
    )
    if(any(updated == 1L))
      word_cat("* unitizer was not saved")
    if(any(updated == 2L))
      word_cat("$ unitizer was saved in prior evaluation")
    cat("\n")
  }
  if(length(which.fail)) {
    if(length(which.pass)) cat("\n")
    test.files <- vapply(x[which.fail], slot, character(1L), "test.file")
    fail.reason <- vapply(x[which.fail], slot, character(1L), "reason")
    file.names.short <- unique_path(test.files)

    word_cat(
      "Unitizers for the following files could not be loaded:\n",
      as.character(
        UL(
          paste0(
            "id: ", which.fail, "; ", files.short[which.fail], ": ", fail.reason
        ) ),
        width=getOption("width") - 2L
      ),
      sep="\n"
    )
    cat("\n")
  }
  word_cat("Test files in common directory '", files.dir, "'", sep="")
  return(invisible(x))
}
# Check whether an object is of type "unitizer_result"
#
# returns TRUE on success and character string on failure

is.unitizer_result <- function(x) {
  if(!inherits(x, "unitizer_result"))
    return("does not inherit from \"unitizer_result\"")
  if(is.null(attr(x, "test.file")) || is.null(attr(x, "store.id")))
    return("is missing \"test.file\" and/or \"store.id\" attributes")
  if(!isTRUE(dat.err <- is.unitizer_result_data(x)))
    return(paste0("data ", dat.err))
  TRUE
}
# Check whether an object conforms to the data frame structure expected of
# the data component of a "unitizer_result" object
#
# returns TRUE on success and character string on failure

is.unitizer_result_data <- function(x) {
  if(!is.data.frame(x))
    return("is not a data.frame")
  names.valid <-
    c("id", "call", "section", "ignored", "status", "user", "reviewed")
  if(!identical(names(x), names.valid))
    return(paste0("does not have names expected columns"))
  if(
    !identical(
      unname(vapply(x, class, character(1L))),
      c(
        "integer", "character", "character", "logical", "factor",  "factor",
        "logical"
    ) )
  )
    return(paste0("does not have the expected column classes"))
  TRUE
}

setGeneric("extractResults", function(x, ...) standardGeneric("extractResults"))
setMethod(
  "extractResults", "unitizerObjectList",
  function(x, ...)
    structure(lapply(as.list(x), extractResults), class="unitizer_results")
)
setMethod(
  "extractResults", "unitizer",
  function(x, ...)
    structure(
      x@res.data, class=unique(c("unitizer_result", class(x@res.data))),
      test.file=x@test.file.loc,
      store.id=x@id, updated=x@updated,
      updated.at.least.once=x@updated.at.least.once
    )
)
setMethod("extractResults", "unitizerLoadFail", function(x, ...) x)

