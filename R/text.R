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
#' @param width at what width to wrap output
#' @param max.len 2 length integer vector with first value threshold at which we start trimming output
#' @param file whether to show to stdout or stderr
#' @param frame what frame to capture in, relevant mostly if looking for a print
#'   method
#' @aliases obj_capt obj_screen_out

diff_obj_out <- function(
  obj.rem, obj.add, obj.rem.name=deparse(substitute(obj.rem))[[1L]],
  obj.add.name=deparse(substitute(obj.add))[[1L]], width=getOption("width"),
  max.len=getOption("unitizer.test.fail.out.lines"),
  file=stdout(), frame=parent.frame()
) {
  frame # force
  tar.width <- width - 4L
  obj.add.capt <- obj_capt(obj.add, tar.width, frame)
  obj.rem.capt <- obj_capt(obj.rem, tar.width, frame)

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
  cat(sep="\n",
    res <- c(
      obj_screen_chr(
        obj.rem.capt, obj.rem.name, first.diff=first.diff, max.len=max.len,
        width=tar.width, pad="-   "
      ),
      obj_screen_chr(
        obj.add.capt, obj.add.name, first.diff=first.diff, max.len=max.len,
        width=tar.width, pad="+   "
      )
  ) )
  invisible(res)
}
# @keywords internal
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
# @keywords internal
# @rdname diff_obj_out

obj_screen_chr <- function(
  obj.chr, obj.name, first.diff, max.len, width, pad
) {
  pre <- post <- NULL
  extra <- paste0("; see `", obj.name, "`")
  if(length(obj.chr) > max.len[[1L]] && first.diff > 1L) {
    obj.chr <- tail(obj.chr, -(first.diff - 1L))
    pre <- paste0("... omitted ", first.diff - 1L, " lines")
  }
  if((len <- length(obj.chr)) > max.len[[1L]]) {
    obj.chr <- head(obj.chr, max.len[[2L]])
    post <- paste0("... omitted ", len - max.len[[2L]], " lines")
  }
  if(!is.null(post)) {
    post <- word_wrap(paste0(post, extra, " ..."), width)
  }
  if (!is.null(pre)) {
    pre <- word_wrap(paste0(pre, if(is.null(post)) extra, " ..."), width)
  }
  c(
    paste0("@@ ", obj.name, " @@"),
    paste0(pad, c(pre, obj.chr, post))
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

#' Wrap Lines at Words
#'
#' Similar to \code{\link{text_wrap}}, but only allows one length width and
#' breaks lines at words if possible.
#'
#' Will attempt to hyphenate very crudely.
#'
#' @keywords internal
#' @param x character vector
#' @param width what width to wrap at
#' @param tolerance how much earlier than \code{width} we're allowed to wrap
#' @param hyphens whether to allow hyphenation
#' @param unlist logical(1L) if FALSE each element in \code{x} is returned as
#'   an element of a list, otherwise one character vector is returned
#' @return character vector, or list if \code{unlist} is FALSE

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
  seps <- "[^a-zA-Z0-9']"
  base.ptrn <- paste0(
    "(?:",
      "(?:(.*%s).{0,", max(tolerance - 1L, 0L), "}.)|",
      "(?:(.*%s).{0,", tolerance, "})",
    ")$"
  )
  spc.ptrn <- sprintf(base.ptrn, "\\s", "\\s")
  non.alph.ptrn <- sprintf(base.ptrn, seps, seps)
  hyph.base <- paste0(
    "^(.*[A-Za-z]*%s[A-Za-z]*%s)%s[A-Za-z].{0,", tolerance, "}$"
  )
  hyph.ptrns <- c(
    sprintf(hyph.base, vows, cons, cons.no.h),
    sprintf(hyph.base, ltrs, cons, vows),
    sprintf(hyph.base, ltrs, vows, cons),
    sprintf(hyph.base, ltrs, vows, vows)
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
        x.trim <- sub(spc.ptrn, "\\1\\2", x.sub)
        matched <- grepl(spc.ptrn, x.sub)
        if(!matched) {
          x.trim <- sub(non.alph.ptrn, "\\1\\2", x.sub)
          matched <- grepl(non.alph.ptrn, x.sub)
        }
        # Attempt to hyphenate

        hyph.match <- FALSE
        if(hyphens) {
          if(!matched) {
            for(pat in hyph.ptrns) {
              x.trim <- sub(pat, "\\1", x.sub)
              matched <- grepl(pat, x.sub)
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
          substr(x, min(nchar(x.trim), width) + 1L - pad, nchar(x))
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
  x.lst <- as.list(x)
  x.lst[nchar(x) > 0] <- strsplit(gsub("\n", "\n\n", x[nchar(x) > 0]), "\n")     # replace new lines with 0 char item
  x.exp <- unlist(x.lst)  # x.lst workaround required because `strsplit` swallows zero char char items!!
  res <- lapply(x.exp, break_char)
  if(unlist) unlist(res) else res
}
#' Print To Screen Wrapping Words
#'
#' @keywords internal
#' @seealso \code{\link{word_wrap}}

word_cat <- function(
  ..., sep=" ", width=getOption("width"), tolerance=8L, file=stdout()
) {
  vec <- try(
    paste0(unlist(list(...)), collapse=sep),
    silent=TRUE
  )
  if(inherits(vec, "try-error")) stop(conditionMessage(attr(vec, "condition")))
  vec <- unlist(strsplit(vec, "\n"))
  invisible(cat(word_wrap(vec, width, tolerance), file=file, sep="\n"))
}
#' Over-write a Line
#'
#' @keywords internal
#' @param x character(1L)
#' @param min.width integer(1L) minimum character width to print to
#' @param max.width integer(1L) max width to print to
#' @return NULL used only for side effect of cating ot screen

over_print <- function(x, min.width=30L, max.width=getOption("width")) {
  if(!is.character(x) || length(x) != 1L)
    stop("Argument `x` must be character(1L)")
  if(!is.integer(min.width) || length(min.width) != 1L)
    stop("Argument `min.width` must be integer(1L)")
  if(!is.integer(max.width) || length(max.width) != 1L)
    stop("Argument `max.width` must be integer(1L)")

  writeLines(
    c(
      "\r", rep(" ", max(min.width, max.width)), "\r",
      substr(x, 1, max(min.width, max.width))
    ), sep=""
  )
  NULL
}
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
  z <- sub("[^bcdfghjklmnpqrstvwxz]*$", "", x)
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
