#' Display helper function
#'
#' @keywords internal

screen_out <- function(
  txt, max.len=getOption("unitizer.test.out.lines"), file=stdout()
) {
  if(!is.numeric(max.len) || !length(max.len) == 2 || max.len[[1]] < max.len[[2]])
    stop("Argument `max.len` must be a two length numeric vector with first value greater than second")
  if(out.len <- length(txt)) {
    txt.proc <- txt[1L:min(out.len, if(out.len > max.len[[1]]) max.len[[2]] else Inf)]
    lapply(txt.proc, function(x) word_cat(x, file=file))
    if(out.len > max.len[[1]]) {
      word_cat(
        "... truncated", out.len - max.len[[2]],
        "line", if(out.len - max.len[[2]] > 1) "s",
        file=file
      )
} } }
#' Show a Faux Diff Between Two Objects
#'
#' The idea is to save the user the need to print out the objects to screen.
#' This will print two objects to screen in a faux git diff look, focusing on a
#' snippet of the object.
#'
#' @keywords internal
#' @param obj.rem object to compare
#' @param obj.add object to compare
#' @param obj.rem.name object to compare
#' @param obj.add.name object to compare
#' @param x a character vector
#' @param y another character vector to compare to
#' @param width at what width to wrap output
#' @param max.len 2 length integer vector with first value threshold at which we
#'   start trimming output and the second the length we tri to
#' @param file whether to show to stdout or stderr
#' @param frame what frame to capture in, relevant mostly if looking for a print
#'   method
#' @return
#'   \itemize{
#'      \item for \code{char_diff} a list with two vectors, each of same length
#'        as inputs, where FALSE indicates value is the same as in the other
#'        vector, and TRUE indicates it is different
#'   }
#' @aliases obj_capt obj_screen_out

diff_obj_out <- function(
  obj.rem, obj.add, obj.rem.name=deparse(substitute(obj.rem))[[1L]],
  obj.add.name=deparse(substitute(obj.add))[[1L]], width=getOption("width"),
  max.len=NULL, file=stdout(), frame=parent.frame()
) {
  err.type <- "Argument"
  if(is.null(max.len)) {
    max.len <- getOption("unitizer.test.fail.out.lines")
    err.type <- "Option"
  }
  if(
    !is.numeric(max.len) || !identical(length(max.len), 2L) || any(max.len < 1)
  )
    stop(
      err.type, " `unitizer.test.fail.out.lines` must be integer(2L) and ",
      "greater than or equal to 1"
    )
  max.len <- as.integer(max.len)
  frame # force
  tar.width <- width - 4L
  obj.add.capt <- sub("\\s+$", "", obj_capt(obj.add, tar.width, frame))
  obj.rem.capt <- sub("\\s+$", "", obj_capt(obj.rem, tar.width, frame))

  min.len <- min(length(obj.add.capt), length(obj.rem.capt))
  diffs <-
    gsub("\\s+", " ", obj.add.capt[1L:min.len]) !=
    gsub("\\s+", " ", obj.rem.capt[1L:min.len])  # shouldn't have NAs

  first.diff <- if(!any(diffs)) 1L else which(diffs)[[1L]]
  if(first.diff < max.len[[2L]]) first.diff <- 1L   # don't advance if not needed to show first difference
  if(first.diff > min.len - max.len[[1L]]) {        # could show more error, so will
    first.diff <- max(1L, min.len - max.len[[1L]] + 1L)
    max.len <- rep(max.len[[1L]], 2L)
  }
  diff <- char_diff(obj.rem.capt, obj.add.capt)
  pad.rem <- rep("   ", length(obj.rem.capt))
  pad.add <- rep("   ", length(obj.add.capt))
  pad.rem[diff[[1L]]] <- "-  "
  pad.add[diff[[2L]]] <- "+  "

  res <- c(
    obj_screen_chr(
      obj.rem.capt, obj.rem.name, first.diff=first.diff, max.len=max.len,
      width=tar.width, pad=pad.rem
    ),
    obj_screen_chr(
      obj.add.capt, obj.add.name, first.diff=first.diff, max.len=max.len,
      width=tar.width, pad=pad.add
  ) )
  if(!is.null(file)) cat(sep="\n", res, file=file)
  invisible(res)
}
# @rdname diff_obj_out

char_diff <- function(x, y) {
  stopifnot(
    is.character(x), is.character(y), !any(is.na(c(x, y)))
  )
  # find first difference

  len.match <- min(length(x), length(y))
  eq <- head(x, len.match) == head(y, len.match)
  diffs <- which(!eq)
  if(!length(diffs)) return(
    list(
      c(rep(FALSE, len.match), c(rep(TRUE, length(x) - len.match))),
      c(rep(FALSE, len.match), c(rep(TRUE, length(y) - len.match)))
    )
  )
  first.diff <- diffs[[1L]]
  eq.so.far <- rep(FALSE, first.diff - 1L)
  eq.extra <- logical(0L)

  # Try to see if difference exists in y, and if not see if any subsequent line
  # does exit, indicating deletions from x

  for(i in seq(first.diff, length(x), by=1L)) {
    n.match <- head(which(x[[i]] == tail(y, -first.diff)), 1L)
    if(length(n.match)) {
      tmp.res <- Recall(
        x[i:length(x)], y[(n.match[[1L]] + first.diff):length(y)]
      )
      return(
        list(
          c(eq.so.far, eq.extra, tmp.res[[1L]]),
          c(eq.so.far, rep(TRUE, n.match[[1L]]), tmp.res[[2L]])
    ) ) }
    eq.extra <- c(eq.extra, TRUE)
  }
  # Difference did not exist in y

  list(
    c(eq.so.far, eq.extra),
    c(eq.so.far, rep(TRUE, length(y) - first.diff + 1L))
  )
}
# @rdname diff_obj_out

obj_capt <- function(obj, width=getOption("width"), frame=parent.frame()) {
  if(!is.numeric(width) || length(width) != 1L)
    stop("Argument `width` must be a one long numeric/integer.")
  if(!is.environment(frame))
    stop("Argument `frame` must be an environment") # note this forces eval, which is needed
  width.old <- getOption("width")
  on.exit(options(width=width.old))
  width <- max(width, 10L)

  options(width=width)
  obj.out <- capture.output(
    invisible(print.res <- user_exp_display(obj, frame, quote(obj)))
  )
  options(width=width.old)
  on.exit(NULL)

  if(print.res$aborted) {  # If failed during eval retrieve conditions
    err.cond <-
      which(vapply(print.res$conditions, inherits, logical(1L), "error"))
    err.cond.msg <- if(length(err.cond)) {
      c(
        paste0(
          "<Error in print/show",
          if(is.object(obj))
            paste0(" method for object of class \"", class(obj)[[1L]], "\""),
          ">"
        ),
        paste0(
          conditionMessage(print.res$conditions[[err.cond[[1L]]]]), collapse=""
      ) )
    } else ""
    obj.out <- c(obj.out, err.cond.msg)
  }
  obj.out
}
# @rdname diff_obj_out

obj_screen_chr <- function(
  obj.chr, obj.name, first.diff, max.len, width, pad
) {
  pre <- post <- NULL
  extra <- paste0("; see `", obj.name, "`")
  if(length(obj.chr) > max.len[[1L]] && first.diff > 1L) {
    obj.chr <- tail(obj.chr, -(first.diff - 1L))
    pre <- paste0("... omitted ", first.diff - 1L, " lines")
    pad <- tail(pad, -(first.diff - 1L))
  }
  if((len <- length(obj.chr)) > max.len[[1L]]) {
    obj.chr <- head(obj.chr, max.len[[2L]])
    post <- paste0("... omitted ", len - max.len[[2L]], " lines")
    pad <- head(pad, max.len[[2L]])
  }
  pad <- format(pad)
  pad.pre.post <- paste0(rep(" ", nchar(pad[[1L]])), collapse="")
  if(!is.null(post)) {
    post <- paste0(
      pad.pre.post,
      word_wrap(paste0(post, extra, " ..."), width - nchar(pad[[1L]]))
    )
  }
  if (!is.null(pre)) {
    pre <- paste0(
      pad.pre.post,
      word_wrap(
        paste0(pre, if(is.null(post)) extra, " ..."), width - nchar(pad[[1L]])
  ) ) }
  c(
    paste0("@@ ", obj.name, " @@"),
    paste0(c(pre, paste0(pad, obj.chr), post))
  )
}
#' Print Only First X characters
#'
#' @keywords internal
#' @param x string to reduce length
#' @param nchar.max how many characters to reduce each string to
#' @param ctd 1 length character vector for what to use to indicate string truncated
#' @param disambig logical 1L whether to disambiguate strings that end up
#'   the same after truncation (not currently implemented)
#' @param from what side to truncate from

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
#' }
#'
#' @keywords internal
#' @return a list with, for each item in \code{`x`}, a character vector
#'   of the item wrapped to length \code{`width`}
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
  x, width=getOption("width"), tolerance=8L, hyphens=TRUE, unlist=TRUE
) {
  if(!is.character(x) || !is.integer(width) || length(width) != 1L || is.na(width))
    stop("Invalid arguments")
  stopifnot(
    is.integer(tolerance) && length(tolerance) == 1L && !is.na(tolerance) &&
    tolerance >= 0L
  )
  stopifnot(width > 4L && width - tolerance > 2L)
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
    res <- character(lines.raw + ceiling(lines.raw / (width - tolerance))) # for hyphens
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

        x.trim <- substr(x.trim, 1L, width)  # we allow one extra char for pattern matching in some cases, remove here
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
  x.lst[nchar(x) > 0] <- strsplit(gsub("\n", "\n\n", x[nchar(x) > 0]), "\n")     # replace new lines with 0 char item
  #x.exp <- unlist(x.lst)
  res <- lapply(x.lst, function(x) unlist(lapply(x, break_char)))
  if(unlist) unlist(res) else res
}
#' @rdname text_wrap

word_cat <- function(
  ..., sep=" ", width=getOption("width"), tolerance=8L, file=stdout()
) {
  vec <- try(
    paste0(unlist(list(...)), collapse=sep),
    silent=TRUE
  )
  if(inherits(vec, "try-error")) stop(conditionMessage(attr(vec, "condition")))
  vec <- unlist(strsplit(vec, "\n"))
  out <- word_wrap(vec, width, tolerance)
  cat(out, file=file, sep="\n")
  invisible(out)
}
#' @rdname text_wrap

word_msg <- function(...) word_cat(..., file=stderr())

#' @rdname text_wrap

word_comment <- function(
  x, width=getOption("width"), tolerance=8L, hyphens=TRUE, unlist=TRUE
) {
  if(!is.character(x)) stop("Argument `x` must be character")
  if(!all(grep("^#", x)))
    stop("Argument `x` must be character with all elements starting with '#'")
  res <- word_wrap(
    x=sub("^#", "", x), width=width - 1L, tolerance=tolerance, hyphens=hyphens,
    unlist=FALSE
  )
  res <- lapply(res, function(x) paste0("#", x))
  if(unlist) unlist(res) else res
}

#' Over-write a Line
#'
#' @keywords internal
#' @param x character(1L)
#' @param min.width integer(1L) minimum character width to print to
#' @param max.width integer(1L) max width to print to
#' @param append to last non-append \code{x} value
#' @return NULL used only for side effect of cating ot screen

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

#' Produces 1 Line Description of Value
#'
#' @keywords internal
#' @param val object to describe
#' @param limit max characters to display
#' @return character vector describing object

desc <- function(val, limit=getOption("width")) {
  if(!is.numeric(limit) | !identical(length(limit), 1L) | limit <= 4L) {
    stop("Argument `limit` must be a 1 length integer with value greater than 3")
  }
  if(is.null(val)) return("NULL")
  val.desc <- typeof(val)
  if(!is.null(class(val)) && !identical(class(val)[[1]], typeof(class(val)))) {
    val.desc <- paste(val.desc, class(val)[[1]])
  }
  first_lvl <- function(x, inc.len=TRUE) {
    class.map <- c(numeric="num", integer="int", character="chr", complex="cpx", factor="fct")
    classes <- vapply(x, function(y) class(y)[[1L]], character(1L))
    lens <- if(inc.len) {
      vapply(
        x,
        function(y) if(length(y) > 1L) paste0("(", length(y), ")") else "",
        character(1L)
      )
    } else character(length(x))
    paste0(
      ifelse(nchar(nms <- names(x)), valid_names(nms), seq_along(x)), ":",
      ifelse(is.na(new.class <- class.map[classes]), classes, new.class),
      lens, collapse=";"
    )
  }
  if(inherits(val, "data.frame")) {
    val.desc <- paste0(val.desc, " [", nrow(val), ",{", first_lvl(val, FALSE), "}]")
  } else if(!is.null(val.dim <- dim(val))) {
    val.desc <- paste0(val.desc, paste0(" [", paste0(val.dim, collapse=",")), "]")
  } else if (is.atomic(val)) {
    val.desc <- paste0(val.desc, paste0(" [", length(val), "]"))
  } else if (is.recursive(val)) {
    count_rec <- function(x, lvl=0) {
      if(is.recursive(x) && length(x)) {
        lvl <- lvl + 1L
        res <- vapply(x, count_rec, numeric(2L), lvl=lvl)
        return(c(lvl=max(res["lvl", ]), counts=sum(res["counts",])))
      } else {
        return(c(lvl=lvl, counts=1L))
      }
    }
    val.desc <- paste(val.desc, paste0("[", paste0(c(length(val), paste0(count_rec(val), collapse=";")), collapse=","), "]"))
    val.desc <- paste0(val.desc, " {", first_lvl(val), "}")
  }
  if(nchar(val.desc) > limit - 3L) paste0(substr(val.desc, 1L, limit - 3L), "...") else val.desc
}
#' Collapse Multi-line Character into one line
#'
#' @param x character
#' @param chars how many characters to display
#' @keywords internal

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
#' Substring To a Length, but end In Consonant
#'
#' @keywords internal
#' @param x character vector to substring
#' @param stop integer max number of characters
#' @param justify character(1L) passed on to format

substr_cons <- function(x, stop, justify="left") {
  if(!is.character(x)) stop("Argument `x` must be ")
  y <- substr(x, 1, stop)
  z <- sub("[^bcdfghjklmnpqrstvwxz]*$", "", y, ignore.case=TRUE)
  format(z, width=stop, justify=justify)
}
#' Remove Common Characters From Values in a Vector
#'
#' Note that one length \code{x} is a degenerate case that returns "".
#'
#' @keywords internal
#' @param x character the vector to make more unique
#' @param from the direction to remove common elements from

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
#' Convert A Matrix of Test Outcomes for Display
#'
#' Used by \code{show} methods for both \code{unitizerSummary} and
#' \code{unitizerSummaryList}
#'
#' @keywords internal

summ_matrix_to_text <- function(mx, from="right", width=getOption("width")) {
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
  num.width <- max(nchar(col.names), nchar(as.character(totals.keep)))

  test.nums <- paste0(" ", format(seq.int(nrow(mx.keep))), ".")
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
  tot.chr <- as.character(totals.keep)
  tot.chr[is.na(totals.keep)] <- "?"
  tot.chr[!totals.keep] <- "-"
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
  files <- c(output=std.out, message=std.err)
  success <- FALSE
  sink(std.out)
  sink(std.err.con, type="message")
  on.exit({
    sink()
    sink(type="message")
    close(std.err.con)
    if(!success) {
      try({
        cat(readLines(std.out), sep="\n")
        cat(readLines(std.err), sep="\n", file=stderr())
      })
    }
    unlink(files)
  })
  eval(expr, env)
  res <- lapply(files, readLines)
  success <- TRUE
  invisible(structure(res, class="captured_output"))
}
#' @export
#' @rdname capture_output

print.captured_output <- function(x, ...) {
  cat(x$output, sep="\n")
  cat(x$message, sep="\n", file=stderr())
}

