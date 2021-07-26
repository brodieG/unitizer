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

## Display helper function
##
## @keywords internal

screen_out <- function(
  txt, max.len=getOption("unitizer.test.out.lines"), file=stdout(),
  width=getOption("width")
) {
  if(!is.numeric(max.len) || !length(max.len) == 2 || max.len[[1]] < max.len[[2]])
    stop(
      "Argument `max.len` must be a two length numeric vector with first value ",
      "greater than second"
    )
  stopifnot(is.int.pos.1L(width))
  if(length(txt)) {
    txt.trim <- unlist(strsplit(txt, "\n"))
    txt.wrap <- word_wrap(txt.trim, width=width)
    out.len <- length(txt.wrap)
    txt.proc <- txt.wrap[
      1L:min(out.len, if(out.len > max.len[[1]]) max.len[[2]] else Inf)
    ]
    if(sum(nchar(txt.proc))) cat(txt.proc, sep="\n", file=file)
    if(out.len > max.len[[1]]) {
      word_cat(
        "... truncated ", out.len - max.len[[2]],
        " line", if(out.len - max.len[[2]] > 1) "s",
        file=file, sep=""
      )
} } }
## Print Only First X characters
##
## @keywords internal
## @param x string to reduce length
## @param nchar.max how many characters to reduce each string to
## @param ctd 1 length character vector for what to use to indicate string
##   truncated
## @param disambig logical 1L whether to disambiguate strings that end up
##   the same after truncation (not currently implemented)
## @param from what side to truncate from

strtrunc <- function(
  x, nchar.max=getOption("width"), ctd="...", disambig=FALSE,
  from="right"
) {
  if(!identical(disambig, FALSE)) stop("Parameter `disambig` not implemented")
  if(!is.character(x)) stop("Argument `x` must be character")
  if(!is.character(ctd) || !identical(length(ctd), 1L))
    stop("Argument `ctd` must be 1 length character")
  if(!is.numeric(nchar.max) || !identical(length(nchar.max), 1L))
    stop("Argument `nchar.max` must be 1 length numeric")
  if(
    !is.character(from) || length(from) != 1L || is.na(from) ||
    !from %in% c("left", "right")
  )
    stop(
      "Argument `from` must be character(1L) %in% c(\"left\", \"right\") ",
      "and not NA"
    )
  if(all(nchar(x) <= nchar.max)) {
    x
  } else {
    len.target <- nchar.max - nchar(ctd)
    if(len.target < 1L)
      stop("`nchar.max` too small, make bigger or make `ctd` shorter.")
    chars <- nchar(x)
    pre <- post <- ""
    if(identical(from, "right")) {
      start <- 1L
      stop <- len.target
      post <- ctd
    } else {
      start <- chars - len.target + 1L
      stop <- chars
      pre <- ctd
    }
    ifelse(
      nchar(x) <= nchar.max,
      x, paste0(pre, substr(x, start, stop), post)
    )
  }
}
#' Text Wrapping Utilities
#'
#' Functions to break up character vector components to a specified width.
#'
#' \itemize{
#'   \item \code{text_wrap} breaks each element to a specified \code{width},
#'     where \code{width} can contain different values for each value in
#'     \code{x}
#'   \item \code{word_wrap} wraps at whitespace, or crudely hyphenates if
#'     necessary; note that unlike \code{text_wrap} \code{width} must be scalar
#'   \item \code{word_cat} is like \code{word_wrap}, except it outputs to screen
#'   \item \code{word_msg} is like \code{word_cat}, except it ouputs to stderr
#'   \item \code{meta_word_cat} is like \code{word_cat}, except it wraps output
#'     in formatting to highlight this is not normal output
#' }
#'
#' Newlines are replaced by empty strings in the output so that each character
#' vector in the output represents a line of screen output.
#'
#' @keywords internal
#' @return a list with, for each item in \code{x}, a character vector
#'   of the item wrapped to length \code{width}
#' @param x character vector
#' @param width what width to wrap at
#' @param tolerance how much earlier than \code{width} we're allowed to wrap
#' @param hyphens whether to allow hyphenation
#' @param unlist logical(1L) if FALSE each element in \code{x} is returned as
#'   an element of a list, otherwise one character vector is returned
#' @return if \code{unlist} is a parameter, then a character vector, or
#'   if not or if \code{unlist} is FALSE, a list with each element from \code{x}
#'   corresponding to an element from the list

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

#' @rdname text_wrap

word_wrap <- function(
  x, width=getOption("width"), tolerance=8L, hyphens=TRUE, unlist=TRUE,
  collapse=NULL
) {
  stopifnot(
    is.character(x), is.int.pos.1L(width),
    is.integer(tolerance) && length(tolerance) == 1L && !is.na(tolerance) &&
    tolerance >= 0L,
    is.null(collapse) || is.chr1(collapse)
  )
  if(!(width > 4L && width - tolerance > 2L)) {
    warning(
      "Display width too narrow to properly wrap text; setting to 80L"
    )
    width <- 80L
    tolerance <- 8L
  }
  width <- as.integer(width)

  # Define patterns, should probably be done outside of function

  let.vows <- c("a", "e", "i", "o", "u", "y")
  let.vows <- c(let.vows, toupper(let.vows))
  let.all <- c(letters, LETTERS)
  let.cons <- let.all[!let.all %in% let.vows]

  cons <- paste0("[", paste0(let.cons, collapse=""),"]")
  cons.no.h <- paste0(
    "[", paste0(let.cons[!let.cons %in% c("h", "H")], collapse=""),"]"
  )
  vows <- "[aeiouyAEIOUY]"
  ltrs <- "[a-zA-Z]"
  base.ptrn <- paste0("(.*%s).{0,", tolerance, "}$")
  non.alph.ptrn <- paste0("(.*\\W)\\w{0,", max(tolerance - 1L, 0L), "}.$")
  spc.ptrn <- sprintf(base.ptrn, "\\s")
  hyph.base <- paste0(
    "^(.*\\S*%s\\S*%s)%s\\S.{0,", tolerance, "}$"
  )
  # patterns mark places that you can insert a hyphen in, in order of preference
  # though right now there is no trade-off at all between how many more
  # characters you need to cut off to get the better match, which perhaps we
  # should explore

  hyph.ptrns <- c(
    sprintf(hyph.base, vows, cons, cons.no.h),
    sprintf(hyph.base, ltrs, cons, vows),
    sprintf(hyph.base, ltrs, vows, cons),
    sprintf(hyph.base, ltrs, vows, vows),
    sprintf(hyph.base, ".", ".", ".")      # catch-all allows hyphen anyplace
  )
  break_char <- function(x) {
    lines.raw <- ceiling(nchar(x) / (width - tolerance))
    # for hyphens
    res <- character(lines.raw + ceiling(lines.raw / (width - tolerance)))
    res.idx <- 1

    if(!nchar(x)) return(x)
    while(nchar(x)) {
      pad <- 0L  # account for hyphen
      if(nchar(x) > width) {
        x.sub <- substr(x, 1L, width + 1L)
        x.trim <- sub(spc.ptrn, "\\1", x.sub, perl=TRUE)
        matched <- grepl(spc.ptrn, x.sub, perl=TRUE)
        if(!matched) {
          x.trim <- sub(non.alph.ptrn, "\\1", x.sub, perl=TRUE)
          matched <- grepl(non.alph.ptrn, x.sub, perl=TRUE)
        }
        # Attempt to hyphenate

        hyph.match <- FALSE
        if(hyphens) {
          if(!matched) {
            for(pat in hyph.ptrns) {
              x.trim <- sub(pat, "\\1", x.sub, perl=TRUE)
              matched <- grepl(pat, x.sub, perl=TRUE)
              if(matched) {
                x.trim <- paste0(x.trim, "-")
                pad <- 1L
                break
            } }
        } }
        if(!matched) x.trim <- substr(x, 1L, width)  # Failed, truncate
        # we allow one extra char for pattern match some cases, remove here
        x.trim <- substr(x.trim, 1L, width)
        x <- sub(  # remove leading space if any
          "^\\s(.*)", "\\1",
          substr(x, min(nchar(x.trim), width) + 1L - pad, nchar(x)),
          perl=TRUE
        )
      } else {
        x.trim <- x
        x <- ""
      }
      res[[res.idx]] <- x.trim
      res.idx <- res.idx + 1L
    }
    res[1L:(res.idx - 1L)]
  }
  # x.lst workaround required because `strsplit` swallows zero char char items!!

  x.lst <- as.list(x)

  # replace new lines with 0 char item; note that leading NLs need special
  # treatment; used to put in two newlines here; not sure why though

  x.lst[nchar(x) > 0] <- strsplit(x[nchar(x) > 0], "\n")

  res <- lapply(x.lst, function(x) unlist(lapply(x, break_char)))
  res.fin <- if(unlist) unlist(res) else res
  if(!is.null(collapse)) {
    res.fin <- if(is.list(res.fin))
      lapply(res.fin, paste0, collapse=collapse) else
        paste0(res.fin, collapse=collapse)
  }
  res.fin
}
# Helper function to concatenate strings together
cc <- function(..., c="") paste0(c(...), collapse=c)

#' @rdname text_wrap

meta_word_cat <- function(
  ..., sep="\n", width=getOption("width"), tolerance=8L, file=stdout(),
  trail.nl=TRUE
) {
  # NOTE: if we change `pre` nchar width there are several calls to
  # meta_word_wrap involving `UL` that will need to be udpated as well

  out <-
    word_wrap_split(..., sep=sep, width=width, tolerance=tolerance, pre="| ")
  if(!is.null(out)) cat(out, sep="\n", file=file)
  if(trail.nl) cat("\n")
  invisible(out)
}
#' @rdname text_wrap

meta_word_msg <- function(
  ..., sep="\n", width=getOption("width"), tolerance=8L, trail.nl=TRUE
) {
  out <- paste0(
    c(
      word_wrap_split(..., sep=sep, width=width, tolerance=tolerance, pre="| "),
      if(trail.nl) ""
    ),
    collapse="\n"
  )
  if(length(out)) message(paste0(out))
  invisible(out)
}
## Like word_wrap, but handles some additional duties needed for word_cat

word_wrap_split <- function(
  ..., width=getOption("width"), tolerance=8L, pre="", sep=" "
) {
  stopifnot(is.chr1(pre))
  width <- width - nchar(pre)
  if(width < 10L) width <- 10L
  vec <- try(
    paste0(unlist(list(...)), collapse=sep),
    silent=TRUE
  )
  if(inherits(vec, "try-error")) stop(conditionMessage(attr(vec, "condition")))
  paste0(pre, word_wrap(vec, width=width, tolerance=tolerance))
}
#' @rdname text_wrap

word_cat <- function(
  ..., sep=" ", width=getOption("width"), tolerance=8L, file=stdout()
) {
  out <- word_wrap_split(..., width=width, tolerance=tolerance, sep=sep)
  if(!is.null(out)) cat(out, file=file, sep="\n")
  invisible(out)
}
#' @rdname text_wrap

word_msg <- function(...) word_cat(..., file=stderr())

#' @rdname text_wrap

word_comment <- function(
  x, width=getOption("width"), tolerance=8L, hyphens=TRUE, unlist=TRUE,
  color=crayon::has_color()
) {
  if(is.null(color)) color <- crayon::has_color()
  if(!is.character(x)) stop("Argument `x` must be character")
  if(!all(grep("^#", x)))
    stop("Argument `x` must be character with all elements starting with '#'")
  res <- word_wrap(
    x=sub("^#", "", x), width=width - 1L, tolerance=tolerance, hyphens=hyphens,
    unlist=FALSE
  )
  res <- lapply(
    res,
    function(x) if(color) crayon::silver(paste0("#", x)) else paste0("#", x)
  )
  if(unlist) unlist(res) else res
}

## Over-write a Line
##
## @keywords internal
## @param x character(1L)
## @param min.width integer(1L) minimum character width to print to
## @param max.width integer(1L) max width to print to
## @param append to last non-append \code{x} value
## @return NULL used only for side effect of cating to screen

over_print <- (
  function() {
    prev.val <- ""
    function(x, append=FALSE, min.width=30L, max.width=getOption("width")) {
      if(!is.character(x) || length(x) != 1L || is.na(x))
        stop("Argument `x` must be character(1L) and not NA")
      if(!is.integer(min.width) || length(min.width) != 1L)
        stop("Argument `min.width` must be integer(1L)")
      if(!is.integer(max.width) || length(max.width) != 1L)
        stop("Argument `max.width` must be integer(1L)")
      if(!isTRUE(append) && !identical(append, FALSE))
        stop("Argument `append` must be TRUE or FALSE")

      cat(
        c(
          "\r", rep(" ", max(min.width, max.width)), "\r",
          substr(paste0(if(append) prev.val, x), 1L, max(min.width, max.width))
        ),
        sep=""
      )
      prev.val <<- if(append) prev.val else x
      invisible(NULL)
} } ) ()

class_map <- function(val) {
  abb <- c(
    numeric="num", integer="int", character="chr", complex="cpx", factor="fct",
    matrix="mat", logical="logi"
  )
  if(is.na(mapped <- abb[match(val, names(abb))])) val else mapped
}
desc_type <- function(val) {
  class.map <- c(
    numeric="num", integer="int", character="chr", complex="cpx", factor="fct",
    logical="logi"
  )
  if(is.matrix(val))
    paste(class_map(typeof(val)), "mat") else
    class_map(head(class(val), 1L))
}
desc_size <- function(val) {
  if(!is.null(dim(val))) {
    paste0("[", paste0(dim(val), collapse=","), "]")
  } else if((length(val) != 1L || !is.object(val)) && !is.null(val)) {
    paste0("[", length(val), "]")
  }
}
desc_simple <- function(val) {
  type <- desc_type(val)
  paste0(type, desc_size(val))
}
#' One Line Description of Object
#'
#' Objects are described by class, and dimensions.  Dimensions is always denoted
#' in square brackets.  For example, \dQuote{int[10]} means an integer of length
#' ten.  Typically an object will be identified by \code{head(class(obj), 1L)}
#' along with its dimensions.  Recursive objects will have the first level shown
#' provided that doing so fits within \code{limit}.
#'
#' Eventually this will be migrated to an S3 generic to allow recursive dispatch
#' on object type.
#'
#' @export
#' @param val object to describe
#' @param limit max characters to display
#' @return character(1L) describing object
#' @examples
#' desc(list(a=iris, b=lm(dist ~ speed, cars), 1:10, matrix(letters, 2)))

desc <- function(val, limit=getOption("width")) {
  type <- desc_type(val)
  simple <- desc_simple(val)
  res <- if(nchar(simple) < limit && is.recursive(val) && length(val)) {
    descs <- vapply(val, desc_simple, character(1L))
    names <- if(is.null(names(val))) character(length(val)) else names(val)
    rec <- sprintf(
      "%s(%s)", type,
      paste0(
        ifelse(nzchar(names), paste0(names, "=", descs), descs),
        collapse=", "
    ) )
    if(nchar(rec) < limit) rec else simple
  } else simple
  if(nchar(res) > limit - 3L)
      paste0(substr(res, 1L, limit - 3L), "...") else res
}
## Collapse Multi-line Character into one line
##
## @param x character
## @param chars how many characters to display
## @keywords internal

one_line <- function(x, chars=0L) {
  if(!is.character(x) || any(is.na(x)))
    stop("Argument `x` must be character and may not contain NAs")
  chars <- as.integer(chars) # not ideal due to NA by coersion
  if(!is.numeric(chars) || length(chars) != 1L || is.na(chars))
    stop("Argument `chars` must be integer(1L) and not NA")
  one.line <- paste0(sub("^\\s*", "", unlist(strsplit(x, "\n"))), collapse="")
  if(chars < 1L) return(one.line)
  if(chars < 10L) substr(one.line, 1L, chars) else
  if(nchar(one.line) > chars)
    paste0(substr(one.line, 1L, chars - 3L), "...") else one.line
}
#' Make Valid Names
#'
#' If names are invalid, quotes them with backtics
#'
#' @keywords internal
#' @param x character vector
#' @return character vector

valid_names <- function(x) {
  ifelse(
    grepl("^[a-zA-Z.]([_a-zA-Z.][a-zA-Z0-9._]*)?$", x),
    x,
    paste0("`", x, "`")
  )
}
#' Captalizes or Decapitalizes First Letter
#'
#' @keywords internal
#' @aliases decap_first
#' @param x character
#' @return character

cap_first <- function(x) change_first(x, toupper)
decap_first <- function(x) change_first(x, tolower)
change_first <- function(x, fun) {
  if(!is.character(x)) stop("Argument `x` must be a character vector.")
  ifelse(
    nchar(x) > 2L,
    paste0(substr(fun(x), 1L, 1L), substr(x, 2L, nchar(x))),
    ifelse(nchar(x) == 1L, fun(x), x)
  )
}
## Substring To a Length, but end In Consonant
##
## @keywords internal
## @param x character vector to substring
## @param stop integer max number of characters
## @param justify character(1L) passed on to format

substr_cons <- function(x, stop, justify="left") {
  if(!is.character(x)) stop("Argument `x` must be ")
  y <- substr(x, 1, stop)
  z <- sub("[^bcdfghjklmnpqrstvwxz]*$", "", y, ignore.case=TRUE)
  format(z, width=stop, justify=justify)
}
## Remove Common Characters From Values in a Vector
##
## Note that one length \code{x} is a degenerate case that returns "".
##
## @keywords internal
## @param x character the vector to make more unique
## @param from the direction to remove common elements from

str_reduce_unique <- function(x, from="left") {
  if(
    !is.character(from) || length(from) != 1L || is.na(from) ||
    !from %in% c("left", "right")
  )
    stop(
      "Argument `from` must be character(1L) %in% c(\"left\", \"right\") ",
      "and not NA"
    )
  if(!is.character(x) || any(is.na(x)))
    stop("Argument `x` must be character and may not contain NAs")
  if(identical(length(unique(x)), 1L)) return(rep("", length(x)))  # degenerate case
  char.list <- strsplit(x, "")
  if(identical(from, "right")) char.list <- lapply(char.list, rev)
  min.len <- min(vapply(char.list, length, 1L))
  char.mx <- vapply(char.list, `[`, character(min.len), 1:min.len)
  first.diff <- min(
    which(apply(char.mx, 1, function(x) length(unique(x))) > 1L)
  )
  char.mx.trim <- char.mx[first.diff:nrow(char.mx), ]
  trim.list <- split(char.mx.trim, col(char.mx.trim))
  res <- character(length(x))
  for(i in seq_along(x)) {
    res.tmp <- c(trim.list[[i]], tail(char.list[[i]], -min.len))
    if(identical(from, "right")) res.tmp <- rev(res.tmp)
    res[[i]] <- paste0(res.tmp, collapse="")
  }
  res
}
## Convert A Matrix of Test Outcomes for Display
##
## Used by \code{show} methods for both \code{unitizerSummary} and
## \code{unitizerSummaryList}
##
## @keywords internal

summ_matrix_to_text <- function(
  mx, from="right", width=getOption("width"), show.nums=TRUE
) {
  # Ignore any columns with zero totals other than pass/fail

  if(
    !is.integer(width) || !identical(length(width), 1L) || is.na(width) ||
    width < 1L
  )
    stop("Argument `width` should be integer(1L) and strictly positive")
  totals <- colSums(mx)
  keep.cols <- colSums(mx, na.rm=TRUE) > 0L | seq_along(totals) < 3L
  mx.keep <- mx[, keep.cols, drop=FALSE]
  totals.keep <- totals[keep.cols]

  col.names <- substr_cons(names(totals.keep), 4L, justify="right")
  col.count <- length(col.names)
  tot.chr <- as.character(totals.keep)
  tot.chr[is.na(totals.keep)] <- "?"
  tot.chr[!is.na(totals.keep) & !totals.keep] <- "-"

  num.width <- max(nchar(col.names), nchar(tot.chr))

  test.len <- nrow(mx.keep)
  test.nums <- if(show.nums)
    paste0(" ", format(seq.int(test.len)), ".") else character(test.len)
  rns <- rownames(mx.keep)

  scr.width <- width
  non.file.chars <-
    (num.width + 1L) * col.count + max(nchar(test.nums)) + 2L
  max.rns.chars <-
    min(max(12L, scr.width - non.file.chars), max(nchar(rns)))

  fmt <- paste0(
    "%", max(nchar(test.nums)), "s %", max.rns.chars, "s ",
    paste0(rep(paste0(" %", num.width, "s"), col.count), collapse="")
  )
  rns.trim <- strtrunc(rns, max.rns.chars, from=from)
  # Display

  res <- do.call(sprintf, c(list(fmt, "", ""), as.list(col.names)))
  mx.keep.chr <- mx.keep
  mx.keep.chr[] <- as.character(mx.keep)
  mx.keep.chr[is.na(mx.keep)] <- "?"
  mx.keep.chr[!mx.keep] <- "-"

  for(i in seq.int(nrow(mx.keep.chr))) {
    res <- c(
      res,
      do.call(
        sprintf,
        c(list(fmt, test.nums[[i]], rns.trim[[i]]), as.list(mx.keep.chr[i, ]))
  ) ) }
  # totals.keep

  res <- c(res, paste0(rep(".", nchar(res[[1L]])), collapse=""))
  res <- c(
    res,
    do.call(sprintf, c(list(fmt, "", ""), as.list(tot.chr)))
  )
  res
}
#' Capture Both StdOut and StdErr
#'
#' Will sink both "output" and "message" streams without checking whether they
#' are already sunk, and will unsink them the same way.
#'
#' @keywords internal
#' @param a quoted to evaluate
#' @param env an environment to evaluate them in
#' @return a list with stdout and stderr captured separately, classed as
#'   "captured_output"

capture_output <- function(expr, env=parent.frame()) {
  std.out <- tempfile()
  std.err <- tempfile()
  std.err.con <- file(std.err, "w")
  old.err.con <- getConnection(sink.number(type="message"))
  files <- c(output=std.out, message=std.err)
  success <- FALSE
  sink(std.out)
  sink(std.err.con, type="message")
  on.exit({
    sink()
    sink(old.err.con, type="message")
    close(std.err.con)
    if(!success) {
      # nocov start
      # can't really test this easily
      try({
        cat(readLines(std.out), sep="\n")
        cat(readLines(std.err), sep="\n", file=stderr())
      })
      # nocov end
    }
    unlink(files)
  })
  eval(substitute(expr), env)
  res <- suppressWarnings(lapply(files, readLines))
  success <- TRUE
  invisible(structure(res, class="captured_output"))
}
#' @export
#' @rdname capture_output

print.captured_output <- function(x, ...) {
  cat(as.character(H3("Output")))
  cat(x$output, sep="\n")
  cat(as.character(H3("Message")))
  cat(x$message, sep="\n")
}

## Convert a Character Vector into a List in English
##
## @param x character elements to list
## @param singular follow on
## @param plural follow on

char_to_eng <- function(x, singular="was", plural="were") {
  stopifnot(is.character(x), is.chr1(singular), is.chr1(plural))
  if(length(x) == 1L) {
    if(nzchar(singular)) paste(x, singular) else x
  } else if (length(x)) {
    base <- paste0(paste0(head(x, -1L), collapse=", "), ", and ", tail(x, 1L))
    if(nzchar(plural)) paste(base, plural) else base
  } else ""
}
