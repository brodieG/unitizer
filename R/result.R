#' @include unitizer.R

NULL

#' Print Methods For \code{unitizer} Results
#'
#' @export
#' @param x object to display
#' @return NULL, invisibly
#'

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
  failed <- vapply(x, is, logical(1L), "unitizerLoadFail")
  which.fail <- which(failed)
  which.pass <- which(!failed)

  if(length(which.pass)) {
    word_cat("Unitizer Results:\n")
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

    to.show <- unique(c(1L, which(!!rowSums(vals["counts", , ,drop=TRUE]))))

    # Now collapse into string form

    as.frac <- function(y) {
      setNames(
        c(
          paste0(y["yesses",], "/", y["counts",]),
          paste0(rowSums(y), collapse="/")
        ),
        c(colnames(y), "Totals")
    ) }
    vals.char <- apply(vals[, to.show, ,drop=FALSE], 3L, as.frac)
    tots.char <- as.frac(apply(vals[, to.show, ,drop=FALSE], 1L:2L, sum))

    # Combine with file names and totals

    file.names <- vapply(
      x[which.pass], function(y) pretty_path(attr(y, "test.file")),
      character(1L)
    )
    fin.mx <- cbind(
      test.file=c(file.names, "Totals"),
      t(
        cbind(
          vals.char,
          if(ncol(vals.char) > 1L) tots.char,
          deparse.level=0L
    ) ) )
    fin.df <- cbind(id=c(which.pass, 0L), as.data.frame(fin.mx))
    fin.out <- capture.output(format(fin.df))
    cat(
      head(fin.out, -1L), paste0(rep("-", max(nchar(fin.out))), collapse=""),
      tail(fin.out, 1L), sep="\n"
    )
  }

  message("MUST ADD FAILED SUMMARY IF RELEVENT")

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
  names.valid <- c("id", "call", "section", "ignored", "status", "user")
  if(!identical(names(x), names.valid))
    return(paste0("does not have names expected columns"))
  if(
    !identical(
      unname(vapply(x, class, character(1L))),
      c("integer", "character", "character", "logical", "factor",  "factor")
    )
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

