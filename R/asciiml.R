#' @include list.R

NULL

#' Print a header
#'
#' @keywords internal
#' @aliases print.H2, print.H3, print.header
#' @param x a 1 length character vector
#' @param margin one of "both", "top", "bottom", "none", weather to add newlines at top or bottom
#' @return 1 length character vector
#' @export

print.header <- function(x, margin="bottom", ...) {
  y <- as.character(x, margin, ...)
  cat(y)
  invisible(y)
}
#' @export

as.character.header <- function(x, margin="bottom", ...) {
  if(!is.character(x)) stop("Argument `x` must be a character vector")
  margin.legal <- c("both", "none", "top", "bottom")
  if(!is.character(margin) || !isTRUE(margin %in% margin.legal))
    stop("Argument `margin` must be in ", deparse(margin.legal))
  if(isTRUE(margin %in% c("both", "top"))) x <- paste0(c("", x), collapse="\n")
  if(isTRUE(margin %in% c("both", "bottom"))) x <- paste0(c(x, ""), collapse="\n")
  paste0(c(x, ""), collapse="\n")
}
#' @export

as.character.H3 <- function(x, margin="bottom", width=getOption("width"), ...) {
  x <- header_help(x, width=width,..., pad.char="-")
  NextMethod()
}
#' @export

as.character.H2 <- function(x, margin="bottom", width=getOption("width"), ...) {
  x <- header_help(x, width=width,..., pad.char="=")
  NextMethod()
}
#' @export

as.character.H1 <- function(x, margin="bottom", width=getOption("width"), ...) {
  if(width < 5L) return(x)
  x <- c(
    paste0(c("+", rep("-", width - 2L), "+"), collapse=""),
    paste0(
      "| ",
      paste0(
        text.wrapped <- unlist(text_wrap(unclass(x), width - 4L), use.names=FALSE),
        vapply(
          (width - 4L) - nchar(text.wrapped),
          function(x) paste0(rep(" ", x), collapse=""),
          character(1L)
        )
      ),
      " |"
    ),
    paste0(c("+", rep("-", width - 2L), "+"), collapse="")
  )
  NextMethod()
}

#' Helper function for single line headers
#'
#' @keywords internal
#' @param x the contents of the header
#' @param width how wide we want the header to display
#' @param ... unused, for compatibility with print generic
#' @param pad.char which character to use to form the header structure

header_help <- function(x, width, ..., pad.char="-") {
  par.call <- sys.call(-1L)
  stop2 <- function(msg) stop(simpleCondition(msg, par.call))
  if(!is.character(x) || length(x) != 1L) stop2("Argument `x` must be a 1 length character vector")
  if(!is.character(pad.char) || length(pad.char) != 1L || nchar(pad.char) != 1L) stop2("Argument `pad.char` must be a 1 length 1 character character vector.")
  if(!is.numeric(width) || length(width) != 1L) stop2("Argument `width` must be a 1 length numeric vector.")
  if(width < 8L) return(x)
  if(isTRUE(nchar(x) > width - 4L)) x <- paste0(substr(x, 1, width - 7L), "...")
  paste0(
    pad.char, " ", x, " ",
    paste0(rep_len(pad.char, width - 3L - nchar(x)), collapse=""), collapse=""
  )
}
#' Create Header Objects
#'
#' Header objects are 1 length character vectors that are printed with text
#' formatting that highlight their "headerness".
#'
#' @keywords internal
#' @seealso \code{`\link{print.header}`}
#' @aliases H1, H2, H3
#' @param x 1 length character vector to turn into a header
#' @param level 1 length integer, what level to make a header
#' @return header object

header <- function(x, level) {
  if(!is.character(x) || length(x) != 1L) stop("Argument `x` must be a one length character vector")
  levels.valid <- 1:3
  if(!identical(round(level), as.numeric(level)) || !isTRUE(level %in% levels.valid)) {
    stop("Argument `level` must be 1 length integer-like and in ", deparse(levels.valid))
  }
  structure(x, class=c(paste0("H", level), "header"))
}
H1 <- function(x) header(x, 1L)
H2 <- function(x) header(x, 2L)
H3 <- function(x) header(x, 3L)

#' Create List Objects
#'
#' Turns a character vector into list items.
#'
#' Currently doesn't support nested lists, but this might be added in the future.
#'
#' @keywords internal
#' @aliases OL
#' @param x character vector of items to make a list out of
#' @return OL/UL object

UL <- function(x) {
  if(!is.character(x)) stop("Argument `x` must be a character vector")
  structure(x, class=c("UL", "bullet"))
}
OL <- function(x) {
  if(!is.character(x)) stop("Argument `x` must be a character vector")
  structure(x, class=c("OL", "bullet"))
}
#' Print Methods for \code{`\link{UL}`} and \code{`\link{OL}`} objects
#'
#' @keywords internal
#' @export
#' @param x object to print
#' @param width integer how many characters to wrap at, if set to 0 will auto
#'   detect width with \code{getOptions("width")}
#' @return invisibly a character vector with one element per line printed

print.bullet <- function(x, width=0L, ...) {
  cat(rendered <- as.character(x, width), sep="\n")
  invisible(rendered)
}
#' Produce Character Vector Representation of Bullet Lists
#'
#' @param x object to render
#' @param width how many characters to wrap at
#' @param pre what to pre-pend to each bullet
#' @param ... dots
#' @return character vector containing rendered object, where each element
#'   corresponds to a line
#' @keywords internal

as.character.bullet <- function(x, width=0L, pre, ...) {
  if(!is.numeric(width) || length(width) != 1L || width < 0) {
    stop("Argument `width` must be a one length positive numeric.")
  }
  width <- round(width)
  if(width == 0) width <- getOption("width")
  screen.width <- width - max(nchar(pre))
  if(screen.width < 8L) width <- 8L
  items <- text_wrap(unclass(x), screen.width)
  unname(
    unlist(
      mapply(SIMPLIFY=FALSE,
        function(content, bullet) {
          paste0(
            c(
              bullet,
              rep(
                paste0(rep(" ", nchar(bullet)), collapse=""),
                length(content) - 1L
            ) ),
            content
        ) },
        items, pre
) ) ) }
#' @export

as.character.UL <- function(x, width=0L, ...) {
  bullets <- rep("- ", length(x))
  NextMethod(pre=bullets)
}
#' @export

as.character.OL <- function(x, width=0L, ...) {
  bullets <- paste0(format(1:length(x)), ". ")
  NextMethod(pre=bullets)
}

#' Wrap Text At Fixed Column Width
#'
#' Some day this should be upgraded to break at whitespaces or use hyphens
#' instead of wrapping arbitrarily at spec'ed width
#'
#' @keywords internal
#' @param x character vector
#' @param width integer vector with
#' @return a list with, for each item in \code{`x`}, a character vector
#'   of the item wrapped to length \code{`width`}

text_wrap <- function(x, width) {
  if(
    !is.character(x) || !is.numeric(width) || any(width < 1L) ||
    !identical(round(width), as.numeric(width))
  ) {
    stop("Arguments `x` and `width` must be character and integer like (all values >= 1) respectively")
  }
  if(!identical((length(x) %% length(width)), 0L)) {
    stop("Argument `x` must be a multiple in length of argument `width`")
  }
  mapply(
    unclass(x), width, SIMPLIFY=FALSE,
    FUN=function(x.sub, width.sub) {
      breaks <- ceiling(nchar(x.sub) / width.sub)
      substr(
        rep(x.sub, breaks),
        start=(1:breaks - 1) * width.sub + 1, stop=(1:breaks) * width.sub
) } ) }
