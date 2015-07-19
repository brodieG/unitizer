#' @include list.R

NULL

#' Print a header
#'
#' @keywords internal
#' @aliases print.H2, print.H3, print.header
#' @param x a 1 length character vector
#' @param margin one of "both", "top", "bottom", "none", whether to add newlines at top or bottom
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
        text.wrapped <- unlist(word_wrap(unclass(x), width - 4L), use.names=FALSE),
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
#' Similar to UL and OL objects from HTML.  These can be nested.  \code{OL}
#' supports \code{c("numbers", "letters", "LETTERS")} as bullet types.
#'
#' @keywords internal
#' @aliases OL
#' @param x character vector of items to make a list out of
#' @return OL/UL object

UL <- function(x, style="-", offset=0L) {
  stopifnot(is.chr1(style) && nchar(style) == 1L)
  bullet_obj(x, style=style, type="unordered", offset=offset)
}
OL <- function(x, style="numbers", offset=0L) {
  stopifnot(is.chr1(style) && style %in% c("numbers", "letters", "LETTERS"))
  bullet_obj(x, style=style, type="ordered", offset=offset)
}
bullet_obj <- function(x, type, style, offset) {
  if(!is.int.1L(offset) || offset < 0L)
    stop("Argument `offset` must be integer(1L) and GTE 0")
  stopifnot(is.chr1(type), type %in% c("ordered", "unordered"))
  if(is.character(x)) x <- as.list(x)
  x <- validate_bullet_list(x, offset)
  bulleter <- if(identical(type, "ordered"))
    bullet_funs[[type]] else
    function(x) rep(style, x)
  structure(
    x, class=c(type, "bullet"), style=style, offset=offset, bulleter=bulleter
  )
}
make_let_combn_fun <- function(dat) {
  function(x) {
    let.count <- ceiling(log(x, base=length(dat)))
    let.list <- rev(
      c(
        list(dat),
        replicate(let.count - 1L, c(" ", dat), simplify=FALSE)
    ) )
    let.combn <- sort(do.call(paste0, do.call(expand.grid, let.list)))
} }
bullet_funs <- list(
  numeric=function(x) as.character(seq.int(x)),
  letters=make_let_combn_fun(letters),
  LETTERS=make_let_combn_fun(LETTERS)
)
validate_bullet_list <- function(x, offset) {
  if(!is.list(x))
    stop("Argument `x` must be a list")
  for(i in seq_along(x)) {
    if(!validate_bullet(x[[i]]))
      stop("Argument `x` contains invalid bullet item at position ", i)
    if(inherits(x[[i]], "bullet"))
      attr(x[[i]], "offset") <- attr(x[[i]], "offset") + offset + 2L
  }
  x
}
validate_bullet <- function(x)
  (is.character(x) && length(x) == 1L) || inherits(x, "bullet")

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
#' @export
#' @param x object to render
#' @param width how many characters to wrap at
#' @param pre what to pre-pend to each bullet
#' @param ... dots
#' @return character vector containing rendered object, where each element
#'   corresponds to a line
#' @keywords internal

as.character.bullet <- function(x, width=0L, ...) {
  if(!is.numeric(width) || length(width) != 1L || width < 0) {
    stop("Argument `width` must be a one length positive numeric.")
  }
  width <- as.integer(width)
  if(width == 0) width <- getOption("width")
  bullet_with_offset(x, width)
}
bullet_with_offset <- function(x, width) {
  pad <- paste0(rep(" ", attr(x, "offset")), collapse="")
  char.vals <- vapply(x, is.character, logical(1L))
  char.pad <- paste0(pad, format(attr(x, "bulleter")(sum(char.vals))))
  char.pad.size <- nchar(char.pad[[1L]])
  text.width <- max(width - char.pad.size, 8L)
  char.wrapped <- word_wrap(
    unlist(x[which(char.vals)]), width=text.width, unlist=FALSE
  )
  pad.extra <- paste0(rep(" ", char.pad.size), collapse="")
  char.padded <- lapply(
    char.wrapped,
    function(x)
      if(!length(x)) x else
        paste0(c(char.pad, rep(pad.extra, length(x) - 1L)), x)
  )
  final <- vector("list", length(x))
  final[which(char.vals)] <- char.padded
  final[which(!char.vals)] <- lapply(x[which(!char.vals)], bullet_with_offset)
  unlist(final)
}
