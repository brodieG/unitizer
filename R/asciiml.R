# Copyright (C) 2020  Brodie Gaslam
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

# Text Representations of HTML Objects
#
# Functions defined here should ultimately be extended into a more
# comprehensive stand alone library for ASCII structured objects (banners,
# tables, etc.).
#
# Blah b
#
# @keywords internal
# @include list.R

NULL

## Print a Header
##
## @aliases print.H2 print.H3 print.header
## @param x a 1 length character vector
## @param margin one of "both", "top", "bottom", "none", whether to add newlines
##   at top or bottom
## @param ... passed on to \code{as.character}
## @return 1 length character vector

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
# Helper function for single line headers
#
# @param x the contents of the header
# @param width how wide we want the header to display
# @param ... unused, for compatibility with print generic
# @param pad.char which character to use to form the header structure

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
# Create Header Objects
#
# Header objects are 1 length character vectors that are printed with text
# formatting that highlight their "headerness".
#
# @seealso \code{\link{print.header}}
# @aliases H1, H2, H3
# @param x 1 length character vector to turn into a header
# @param level 1 length integer, what level to make a header
# @return header object

header <- function(x, level) {
  if(!is.character(x) || length(x) != 1L) stop("Argument `x` must be a one length character vector")
  levels.valid <- 1:3
  if(
    !is.numeric(level) ||
    !identical(round(level), as.numeric(level)) || 
    !isTRUE(level %in% levels.valid)
  ) {
    stop("Argument `level` must be 1 length integer-like and in ", deparse(levels.valid))
  }
  structure(x, class=c(paste0("H", level), "header"))
}
H1 <- function(x) header(x, 1L)
H2 <- function(x) header(x, 2L)
H3 <- function(x) header(x, 3L)

# Create List Objects
#
# Similar to UL and OL objects from HTML.  These can be nested.  \code{OL}
# supports \code{c("numbers", "letters", "LETTERS")} as bullet types.
#
# Ultimately should implement this as S4 classes as assembly of lists is
# annoying since the UL object is itself a list.
#
# @aliases OL
# @param x character vector of items to make a list out of
# @return OL/UL object

UL <- function(x, style="-", offset=0L) {
  stopifnot(is.chr1(style) && nchar(style) == 1L)
  bullet_obj(x, style=style, type="unordered", offset=offset)
}
OL <- function(x, style="numbers", offset=0L) {
  stopifnot(is.chr1(style) && style %in% c("numbers", "letters", "LETTERS"))
  bullet_obj(x, style=style, type="ordered", offset=offset)
}
make_let_combn_fun <- function(dat) {
  function(x) {
    let.count <- ceiling(log(x, base=length(dat)))
    let.list <- rev(
      c(
        list(dat),
        replicate(let.count - 1L, c(" ", dat), simplify=FALSE)
    ) )
    raw.vals <-
      paste0(do.call(paste0, do.call(expand.grid, let.list)), ".")
    # try to get a consistent sort across locales
    head(raw.vals[order(nchar(trimws(raw.vals)), raw.vals)], x)
} }
.bullet.funs <- list(
  numbers=function(x) paste0(seq.int(x), "."),
  letters=make_let_combn_fun(letters),
  LETTERS=make_let_combn_fun(LETTERS)
)
bullet_obj <- function(x, type, style, offset) {
  if(!is.int.1L(offset) || offset < 0L)
    stop("Argument `offset` must be integer(1L) and GTE 0")
  stopifnot(is.chr1(type), type %in% c("ordered", "unordered"))
  if(is.character(x)) x <- as.list(x)
  if(!is.list(x))
    stop("Argument `x` must be a list")
  for(i in seq_along(x))
    if(!validate_bullet(x[[i]]))
      stop("Argument `x` contains invalid bullet item at position ", i)

  bulleter <- if(identical(type, "ordered")) {
    f <- .bullet.funs[[style]]
    if(!is.function(f)) {
      # nocov start
      stop("Internal Error; could not find ordered function; contact maintainer")
      # nocov end
    }
    f
  } else function(x) rep(style, x)
  structure(
    x, class=c(type, "bullet"), style=style, offset=offset, bulleter=bulleter
  )
}
validate_bullet <- function(x)
  (is.character(x) && length(x) == 1L) || inherits(x, "bullet")

#' Print Methods for \code{UL} and \code{OL} objects
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
#' @param ... dots, other arguments to pass to \code{word_wrap}
#' @return character vector containing rendered object, where each element
#'   corresponds to a line
#' @keywords internal

as.character.bullet <- function(x, width=0L, ...) {
  if(!is.numeric(width) || length(width) != 1L || width < 0) {
    stop("Argument `width` must be a one length positive numeric.")
  }
  mc <- match.call()
  if("unlist" %in% names(mc))
    stop(
      "You may not specify `unlist` as part of `...` as that argument is ",
      "used internally"
    )
  width <- as.integer(width)
  if(width == 0) width <- getOption("width")
  bullet_with_offset(x, width, ...)
}
bullet_with_offset <- function(x, width, pad=0L, ...) {
  stopifnot(is.int.1L(pad), pad >= 0L)
  stopifnot(is.int.1L(width), width >= 0L)
  pad.num <- pad + attr(x, "offset")
  pad <- paste0(rep(" ", pad.num), collapse="")
  char.vals <- vapply(x, is.character, logical(1L))
  char.pad <- paste0(
    pad,
    format(attr(x, "bulleter")(sum(char.vals)), justify="right"), " "
  )
  char.pad.size <- nchar(char.pad[[1L]])
  text.width <- max(width - char.pad.size, 8L)
  char.wrapped <- word_wrap(
    unlist(x[which(char.vals)]), width=text.width, unlist=FALSE, ...
  )
  pad.extra <- paste0(rep(" ", char.pad.size), collapse="")
  char.padded <- Map(
    char.wrapped,
    char.pad,
    f=function(x, y)
      if(!length(x)) x else
        paste0(c(y, rep(pad.extra, length(x) - 1L)), x)
  )
  final <- vector("list", length(x))
  final[which(char.vals)] <- char.padded
  final[which(!char.vals)] <- lapply(
    x[which(!char.vals)], bullet_with_offset, width=width, pad=pad.num + 2L
  )
  unlist(final)
}
