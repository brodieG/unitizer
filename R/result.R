#' @include unitizer.R

NULL

#' Print Methods For \code{unitizer} Results
#'
#' @export
#' @param x object to display
#' @return NULL, invisibly

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

  NextMethod(x, ...)
}

#' @rdname print.unitizer_result
#' @export

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
    word_cat("No unitizers")
    return(invisible(NULL))
  }
  failed <- vapply(x, is, logical(1L), "unitizerLoadFail")
  which.fail <- which(failed)
  which.pass <- which(!failed)
  files <- # extract both S4 slot and S3 attribute...
    vapply(x, function(y) pretty_path(attr(y, "test.file")), character(1L))
  files.short <- unique_path(files)
  files.dir <- attr(files.short, "common_dir")

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
    word_cat("Summary of tests (accepted/rejected):\n\n")
    cat(
      head(fin.out, -1L), paste0(rep("-", max(nchar(fin.out))), collapse=""),
      tail(fin.out, 1L), sep="\n"
    )
  }
  if(length(which.fail)) {
    test.files <- vapply(x[which.fail], slot, character(1L), "test.file")
    fail.reason <- vapply(x[which.fail], slot, character(1L), "reason")
    file.names.short <- unique_path(test.files)

    word_cat(
      "Unitizers for the following files could not be loaded:\n\n"
    )
    cat(
      as.character(
        UL(
          paste0(
            "id: ", which.fail, "; ", files.short[which.fail], ": ", fail.reason
    ) ) ) )
  }
  word_cat("\nTest files in common directory '", files.dir, "'", sep="")
  return(invisible(NULL))
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
      store.id=x@id, updated=x@updated
    )
)
setMethod("extractResults", "unitizerLoadFail", function(x, ...) x)

