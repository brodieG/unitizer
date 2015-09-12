#' Print Methods For \code{unitizer} Results
#'
#' @export
#' @param x object to display
#' @return NULL, invisibly
#'

print.unitizer_result <- function(x, ...) {
  if(!isTRUE(fail <- is.unitizer_result(x))) stop(fail)
#x <- readRDS("helper/refobjs/browse_df1.rds")

  x$call <- strtrunc(x$call, 20L)
  x$section <- strtrunc(x$section, 15L)

  store.char <- if(is.chr1plain(attr(x, "store.id"))) {
    pretty_path(attr(x, "store.id"))
  } else {
    att <- try(as.character(attr(x, "store.id")), silent=TRUE)
    if(inherits(att, "try-error")) "<untranslateable store.id>" else att
  }
  word_cat("Test File: ", pretty_path(attr(x, "test.file")), "\n", sep="")
  word_cat("Store ID: ", store.char, "\n", sep="")

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
        function(y) is(x, "unitizerLoadFail") || isTRUE(is.unitizer_result(x)),
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
        y2 <- y[!nchar(y$ignored), ]
        counts <- tapply(y2$status, y2$status, length)
        yesses <- tapply(y2$user == "Y", y2$status, sum)
        counts[is.na(counts)] <- 0L
        yesses[is.na(yesses)] <- 0L
        rbind(counts=counts, yesses=yesses)
      },
      matrix(integer(), 2L, length(levels(x$status)))
    )
    # Compute which columns to display (always show first column); note we reduce
    # vals to a matrix by selecting only the "counts" values of the first dim

    to.show <- unique(c(1L, which(rowSums(vals["counts", , ,drop=TRUE]))))

    # Now collapse into string form

    as.frac <- function(y) {
      setNames(
        c(
          paste0(y["yesses"], "/", y["counts"]),
          paste0(rowSums(y), collapse="/")
        ),
        c(colnames(y), "Totals")
    ) }
    vals.char <- apply(vals[, to.show, ,drop=FALSE], 3L, as.frac)
    tots.char <- as.frac(apply(vals[, to.show, ,drop=FALSE], 1L:2L, sum))

    # Combine with file names and totals

    file.names <- vapply(
      x[which.pass], function(y) pretty_name(attr(y, "test.file")),
      character(1L)
    )
    fin.mx <- cbind(
      test.file=c(file.names, "Totals"),
      rbind(
        vals.char,
        tots.char
      )
    )
    fin.df <- cbind(id=c(which.pass, 0L), as.data.frame(fin.mx))
    fin.out <- capture.output(format(DF))
    cat(
      head(fin.out, -1L), paste0(rep("-", max(nchar(fin.out))), collapse=""),
      tail(fin.out, 1L), sep="\n"
    )
  }

  message("MUST ADD FAILED SUMMARY IF RELEVENT")

  return(invisible(NULL))

}

is.unitizer_result <- function(x) {
  if(!inherits(x, "unitizer_result"))
    return("Argument `x` must inherit from \"unitizer_result\"")
  if(!is.data.frame(x))
    return(
      "Argument `x` must be a data.frame and inherit from \"unitizer_result\""
    )
  if(!is.character(x$call) || !is.character(x$section))
    return(
      paste0(
        "Argument `x` must be a data.frame, inherit from \"unitizer_result\", ",
        "and have character columns \"call\" and \"section\""
    ) )
  if(is.null(attr(x, "test.file")) || is.null(attr(x, "store.id")))
    return(
      paste0(
        "Argument `x` must be a \"unitizer_result\" with \"test.file\" and ",
        "\"store.id\" attributes"
    ) )
  TRUE
}
