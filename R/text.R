#' Display helper function
#'
#' @keywords internal

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
# Classes for tracking intermediate diff obj data
#
# DiffDiffs contains a slot corresponding to each of target and current where
# a TRUE value means the corresponding value matches in both objects and FALSE
# means that it does not match

setClass(
  "unitizerDiffDiffs",
  slots=c(target="logical", current="logical"),
  validity=function(object) {
    for(i in slotNames(object))
      if(any(is.na(slot(object, i))))
        return("slot `", i, "` may not contain NAs")
    TRUE
} )
setClass(
  "unitizerDiff",
  slots=c(
    tar.capt="character",
    cur.capt="character",
    tar.exp="ANY",
    cur.exp="ANY",
    mode="character",
    diffs="unitizerDiffDiffs"
  ),
  validity=function(object) {
    if(!is.chr1(mode) || ! mode %in% c("print", "str"))
      return("slot `mode` must be either \"print\" or \"str\"")
    TRUE
} )
#' Show Diffs Between The Display Values of Two Objects
#'
#' Designed to highlight at a glance the \bold{display} differences between
#' two objects.  Note that the difference shown or reported by these functions
#' may not be the totality of the differences between objects since display
#' methods may not display all differences.
#'
#' \itemize{
#'   \item \code{diff_print} shows the differences in the \code{print} or
#'     \code{show} screen output of the two objects
#'   \item \code{diff_str} shows the differences in the \code{str} screen output
#'     of the two objects; will show as few recursive levels as needed to show
#'     at least one difference (see \code{max.level})
#'   \item \code(diff_obj) picks between \code{diff_print} and \code{diff_str}
#'     depending on which one it thinks will provide the most useful diff
#' }
#'
#' @export
#' @param target the reference object
#' @param new the object being compared to \code{target}
#' @param context 2 length integer vector representing how many lines of context
#'   are shown on either side of differences.  The first value is the maximum
#'   before we start trimming output.  The second value is the maximum to show once
#'   we shown start trimming.  We will always attempt to show as much as
#'   \code{2 * context + 1} lines of output so context may not be centered if
#'   objects display as less than \code{2 * context + 1} lines.
#' @param max.level integer(1L) up to how many levels to try running \code{str};
#'   \code{str} is run repeatedly starting with \code{max.level=1} and then
#'   increasing \code{max.level} until a difference appears or the
#'   \code{max.level} specified here is reached.  If the value is reached then
#'   will let \code{str} run with \code{max.level} unspecified.  This is
#'   designed to produce the most compact screen output possible that shows the
#'   differences between objects, though obviously it comes at a performance
#'   cost; set to 0 to disable
#' @return character, invisibly, the text representation of the diff

diff_obj <- function(target, current, context=NULL) {
  context <- check_context(context)
  width <- getOption("width")
  frame <- parent.frame()

}
#' @rdname diff_obj
#' @export

diff_print <- function(target, current, context=NULL) {
  width <- getOption("width")
  frame <- parent.frame()
  cat(
    as.character(
      diff_print_internal(
        target, current, tar.exp=substitute(target),
        cur.exp=substitute(current), context=context, width=width
    ) ),
    sep="\n"
  )
}
#' @rdname diff_obj
#' @export

diff_str <- function(target, current, context=NULL, max.level=10) {
  width <- getOption("width")
  frame <- parent.frame()
  cat(
    as.character(
      diff_str_internal(
        target, current, tar.exp=substitute(target),
        cur.exp=substitute(current), context=context, width=width,
        frame=frame, max.lines=-1, max.level=max.level
    ) ),
    sep="\n"
  )
}
diff_print_internal <- function(
  target, current, tar.exp, cur.exp, context, width, frame
) {
  obj.add.capt <- obj_capt(target, width, frame)
  obj.rem.capt <- obj_capt(current, width, frame)
  diffs <- char_diff(obj.rem.capt, obj.add.capt)
  new(
    "unitizerDiff", tar.capt=tar.capt, cur.capt=cur.capt, tar.exp=tar.exp,
    cur.exp=cur.exp, diffs=diffs
  )
}
diff_str_internal <- function(
  target, current, tar.exp, cur.exp, context, width, frame, max.lines,
  max.level
) {
  context <- check_context(context)
  if(is.null(max.lines)) {
    max.lines <- context[[1L]] * 2L + 1L
  } else if(!is.int.1L(max.lines) || !max.lines)
    stop("Argument `max.lines` must be integer(1L) and not zero.")
  if(max.lines < 0) max.lines <- Inf
  if(!is.int.1L(max.level))
    stop("Argument `max.level` must be integer(1L) and GTE zero.")
  if(max.level > 100)
    stop("Argument `max.level` cannot be greater than 100")

  obj.add.capt.str <- obj.rem.capt.str <- obj.add.capt.str.prev <-
    obj.rem.capt.str.prev <- character()

  lvl <- 1L
  repeat{
    if(lvl > 100) lvl <- 0 # safety valve
    obj.add.capt.str <-
      obj_capt(current, width, frame, mode="str", max.level=lvl)
    obj.rem.capt.str <-
      obj_capt(target, width, frame, mode="str", max.level=lvl)
    str.len.min <- min(length(obj.add.capt.str), length(obj.rem.capt.str))
    str.len.max <- max(length(obj.add.capt.str), length(obj.rem.capt.str))

    diffs.str <- char_diff(obj.rem.capt.str, obj.add.capt.str)

    # Exit conditions

    if(
      !lvl || any(unlist(diffs.str)) || str.len.max >= max.lines ||
      (
        identical(obj.add.capt.str.prev, obj.add.capt.str) &&
        identical(obj.rem.capt.str.prev, obj.rem.capt.str)
      )
    )
      break

    lvl <- lvl + 1
    obj.add.capt.str.prev <- obj.add.capt.str
    obj.rem.capt.str.prev <- obj.rem.capt.str
  }
  diffs <- char_diff(obj.rem.capt.str, obj.add.capt.str)
  new(
    "unitizerDiff", tar.capt=obj.rem.capt.str, cur.capt=obj.add.capt.str,
    tar.exp=tar.exp, cur.exp=cur.exp, diffs=diffs
  )
}
# Unlike diff_print_internal and diff_str_internal, this one prints to screen
# and invisibly returns the result

diff_obj_internal <- function(
  target, current, tar.exp=substitute(target),
  cur.exp=substitute(current), context=NULL, width=NULL,
  frame=parent.frame()
) {
  context <- check_context(context)
  width <- check_width(width)
  if(!is.environment(frame)) stop("Argument `frame` must be an environment.")
  if(!is.int.1L(max.lines)) stop("Argument `max.lines` must be integer(1L).")

  res.print <- diff_print_internal(
    target, current, tar.exp=substitute(target),
    cur.exp=substitute(current), context=context, width=width,
    frame=frame
  )
  len.print <- max(length(res.print@tar.capt), (res.print@cur.capt))

  res.str <- diff_str_internal(
    target, current, tar.exp=substitute(target),
    cur.exp=substitute(current), context=context, width=width,
    frame=frame, max.lines=len.print
  )
  len.max <- context[[1L]] * 2 + 1
  len.str <- max(length(res.str@tar.capt), (res.str@cur.capt))

  # Chose which display to use; only favor res.str if it really is substantially
  # more compact and it does show an error

  res <- if(
    (!any(res.str) || len.print < len.str * 3) &&
    !(len.print > len.max && len.str <= len.max)
  )
    res.print else res.str

  res.chr <- as.character(res)
  cat(res.chr, file=file, sep="\n")
  invisible(res.char)
}

#' @rdname unitizer_s4method_doc

setMethod("any", "unitizerDiff",
  function(..., na.rm = FALSE) {
    dots <- list(...)
    if(length(dots) != 1L)
      stop("`any` method for `unitizerDiff` supports only one argument")
    x <- dots[[1L]]
    any(x@diffs@target, x@diffs@current)
} )
#' @rdname unitizer_s4method_doc

setMethod("as.character", "unitizerDiff",
  function(x, context, ...) {
    context <- check_context(context)
    # If there is an error, we want to show as much of the objects as we can
    # centered on the error.  If we can show the entire objects without centering
    # then we do that.

    len.max <- max(length(x@tar.capt), length(x@cur.capt))
    first.diff <- if(!any(x)) 1L else
      min(which(x@diffs@target), x@diffs@current)

    show.range <- if(len.max < 2 * context[[1L]] + 1) {
      1:len.max
    } else {
      # if first diff is too close to beginning or end, use extra context on
      # other side of error

      end.extra <- max(0, context[[2L]] - first.diff)
      start.extra <- max(0, context[[2L]] - (len.max - first.diff))
      seq(
        max(first.diff - context[[2L]] - start.extra, 1),
        min(first.diff + context[[2L]] + end.extra, len.max)
      )
    }
    # Make padding

    pad.rem <- rep("   ", length(x@tar.capt))
    pad.add <- rep("   ", length(x@cur.capt))
    pad.rem[diffs[[1L]]] <- "-  "
    pad.add[diffs[[2L]]] <- "+  "

    # As Character

    c(
      obj_screen_chr(
        x@tar.capt, x@tar.expr, obj.diffs=x@diffs@target, range=show.range,
        width=tar.width, pad=pad.rem
      ),
      obj_screen_chr(
        x@cur.capt, x@cur.expr, obj.diffs=x@diffs@current, range=show.range,
        width=tar.width, pad=pad.add
    ) )
} )
# Function to check arguments that can also be specified as options when set
# to NULL

check_context <- function(context) {
  err.msg <- cc(
    "must be integer(2L), positive, non-NA, with first value greater than ",
    "second"
  )
  if(is.null(context)) {
    context <- getOption("unitizer.test.fail.context.lines")
    if(!is.context.out.vec(context))
      stop("`getOption(\"unitizer.test.fail.context.lines\")`", err.msg)
  }
  if(!is.context.out.vec(context)) stop("Argument `context` ", err.msg)
  context
}
check_width <- function(width) {
  err.msg <- "must be integer(1L) and strictly positive"
  if(is.null(width)) {
    width <- getOption("width")
    if(!is.int.pos.1L(width))
      stop("`getOption(\"width\")` ", err.msg)
  }
  if(!is.int.pos.1L(width)) stop("Argument `width` ", err.msg)
  width
}

capt_print <- function(obj, width, frame) {

}
capt_str <- function(obj, width, frame) {
}
capt_diff <- function() {
}
#' Implements the diff_* functions
#'
#' @keywords internal
#' @inheritParams diff_ob
#' @param width at what width to wrap output
#' @param file whether to show to stdout or stderr
#' @param frame what frame to capture in, relevant mostly if looking for a print
#'   method

diff_internal <- function(
  obj.rem, obj.add, context=NULL, mode="auto",
  obj.rem.name=deparse(substitute(obj.rem))[[1L]],
  obj.add.name=deparse(substitute(obj.add))[[1L]],
  width=getOption("width"), file=stdout(), frame=parent.frame()
) {
  err.msg <- cc(
    "must be integer(2L), positive, non-NA, with first value greater than ",
    "second"
  )
  if(is.null(context)) {
    context <- getOption("unitizer.test.fail.context.lines")
    if(!is.context.out.vec(context))
      stop("`getOption(\"unitizer.test.fail.context.lines\")`", err.msg)
  }
  if(!is.context.out.vec(context)) stop("Argument `context` ", err.msg)

  context <- as.integer(context)
  frame # force
  tar.width <- width - 4L
  obj.add.capt <- obj_capt(obj.add, tar.width, frame)
  obj.rem.capt <- obj_capt(obj.rem, tar.width, frame)

  len.min <- min(length(obj.add.capt), length(obj.rem.capt))
  len.max <- str.len.max <- max(length(obj.add.capt), length(obj.rem.capt))

  # shouldn't have NAs

  diffs <- char_diff(obj.rem.capt, obj.add.capt)

  # If normal print diff doesn't fit, try to see if a str version would
  # Algorithm is to find lowest max level that shows difference that has
  # fewer lines than print/show version

  if(any(unlist(diffs)) && len.max > context[[1L]] * 2L + 1L) {
    obj.add.capt.str.prev <- obj.rem.capt.str.prev <- character()
    lvl <- 1L
    repeat{
      obj.add.capt.str <-
        obj_capt(obj.add, tar.width, frame, mode="str", max.level=lvl)
      obj.rem.capt.str <-
        obj_capt(obj.rem, tar.width, frame, mode="str", max.level=lvl)
      str.len.min <- min(length(obj.add.capt.str), length(obj.rem.capt.str))
      str.len.max <- max(length(obj.add.capt.str), length(obj.rem.capt.str))

      diffs.str <- char_diff(obj.rem.capt.str, obj.add.capt.str)

      # Exit conditions

      if(
        lvl > 100 || # safety valve
        (
          identical(obj.add.capt.str.prev, obj.add.capt.str) &&
          identical(obj.rem.capt.str.prev, obj.rem.capt.str)
        ) ||
        any(unlist(diffs.str)) || str.len.max >= len.max
      )
        break

      lvl <- lvl + 1
      obj.add.capt.str.prev <- obj.add.capt.str
      obj.rem.capt.str.prev <- obj.rem.capt.str
  } }
  # Substitute with str message if warranted
  if(str.len.max < len.max && any(unlist(diffs.str))) {
    diffs <- diffs.str
    len.max <- str.len.max
    obj.add.capt <- obj.add.capt.str
    obj.rem.capt <- obj.rem.capt.str
  }
  # If there is an error, we want to show as much of the objects as we can
  # centered on the error.  If we can show the entire objects without centering
  # then we do that.

  first.diff <- if(!any(unlist(diffs))) 1L else min(unlist(lapply(diffs, which)))
  show.range <- if(len.max < 2 * context[[1L]] + 1) {
    1:len.max
  } else {
    # if first diff is too close to beginning or end, use extra context on
    # other side of error

    end.extra <- max(0, context[[2L]] - first.diff)
    start.extra <- max(0, context[[2L]] - (len.max - first.diff))
    seq(
      max(first.diff - context[[2L]] - start.extra, 1),
      min(first.diff + context[[2L]] + end.extra, len.max)
    )
  }
  # Make padding

  pad.rem <- rep("   ", length(obj.rem.capt))
  pad.add <- rep("   ", length(obj.add.capt))
  pad.rem[diffs[[1L]]] <- "-  "
  pad.add[diffs[[2L]]] <- "+  "

  # Display to screen

  res <- c(
    obj_screen_chr(
      obj.rem.capt, obj.rem.name, obj.diffs=diffs[[1L]], range=show.range,
      width=tar.width, pad=pad.rem
    ),
    obj_screen_chr(
      obj.add.capt, obj.add.name, obj.diffs=diffs[[2L]], range=show.range,
      width=tar.width, pad=pad.add
  ) )
  if(!is.null(file)) cat(sep="\n", res, file=file)
  invisible(res)
}
#' a \code{tools::Rdiff} Between R Objects
#'
#' Just a wrapper that saves the \code{print} / \code{show} representation of an
#' object to a temp file and then runs \code{tools::Rdiff} on them.  For
#' each of \code{from}, \code{to}, will check if they are 1 length character
#' vectors referencing an RDS file, and will use the contents of that RDS file
#' as the object to compare.
#'
#' @export
#' @seealso \code{tools::Rdiff}
#' @param from an R object (see details)
#' @param to another R object (see details)
#' @param ... passed on to \code{Rdiff}
#' @return whatever \code{Rdiff} returns

Rdiff_obj <- function(from, to, ...) {
  dummy.env <- new.env()  # used b/c unique object
  files <- try(
    vapply(
      list(from, to),
      function(x) {
        if(is.chr1(x) && file_test("-f", x)) {
          rdstry <- tryCatch(readRDS(x), error=function(x) dummy.env)
          if(!identical(rdstry, dummy.env)) x <- rdstry
        }
        f <- tempfile()
        capture.output(if(isS4(x)) show(x) else print(x), file=f)
        f
      },
      character(1L)
  ) )
  if(inherits(files, "try-error"))
    stop("Unable to store text representation of objects")
  res <- tools::Rdiff(files[[1L]], files[[2L]], ...)
  unlink(files)
  invisible(res)
}
# Carries out the comparison between two character vectors and returns the
# elements that match and those that don't as a unitizerDiffDiffs object

char_diff <- function(x, y) {
  do.call("new", c(list("unitizerDiffDiffs"), char_diff_int(x, y)))
}
char_diff_int <- function(x, y) {
  stopifnot(
    is.character(x), is.character(y), !any(is.na(c(x, y)))
  )
  # find first difference

  x.d <- y.d <- logical()
  len.match <- min(length(x), length(y))
  eq <- head(x, len.match) == head(y, len.match)
  diffs <- which(!eq)
  if(!length(diffs)) {
    x.d <- c(rep(FALSE, len.match), c(rep(TRUE, length(x) - len.match)))
    y.d <- c(rep(FALSE, len.match), c(rep(TRUE, length(y) - len.match)))
  } else {
    first.diff <- diffs[[1L]]
    eq.so.far <- rep(FALSE, first.diff - 1L)
    eq.extra <- logical(0L)

    # Try to see if difference exists in y, and if not see if any subsequent line
    # does exit, indicating deletions from x

    diff.found <- FALSE
    for(i in seq(first.diff, length(x), by=1L)) {
      n.match <- head(which(x[[i]] == tail(y, -first.diff)), 1L)
      if(length(n.match)) {
        tmp.res <- Recall(
          x[i:length(x)], y[(n.match[[1L]] + first.diff):length(y)]
        )
        x.d <- c(eq.so.far, eq.extra, tmp.res[[1L]])
        y.d <- c(eq.so.far, rep(TRUE, n.match[[1L]]), tmp.res[[2L]])
        break
      }
      eq.extra <- c(eq.extra, TRUE)
    }
    if(!diff.found) {
      # Difference did not exist in y
      x.d <- c(eq.so.far, eq.extra)
      y.d <- c(eq.so.far, rep(TRUE, length(y) - first.diff + 1L))
    }
  }
  list(target=x.d, current=y.d)
}
# @rdname diff_obj_out

obj_capt <- function(
  obj, width=getOption("width"), frame=parent.frame(), mode="print",
  max.level=0L
) {
  if(!is.numeric(width) || length(width) != 1L)
    stop("Argument `width` must be a one long numeric/integer.")
  if(!is.chr1(mode) || !mode %in% c("print", "str"))
    stop("Argument `mode` must be one of \"print\" or \"str\"")
  # note this forces eval, which is needed
  if(!is.environment(frame))
    stop("Argument `frame` must be an environment")
  if(!is.int.1L(max.level) || max.level < 0)
    stop("Argument `max.level` must be integer(1L) and positive")

  width.old <- getOption("width")
  on.exit(options(width=width.old))
  width <- max(width, 10L)
  options(width=width)

  if(identical(mode, "print")) {
    obj.out <- capture.output(
      invisible(print.res <- user_exp_display(obj, frame, quote(obj)))
    )
  } else if(identical(mode, "str")) {
    obj.out <- capture.output(
      invisible(
        print.res <-
          user_exp_str(obj, frame, quote(obj), if(max.level) max.level else  NA)
    ) )
  } else stop("Logic Error: unexpected mode; contact maintainer.")

  options(width=width.old)
  on.exit(NULL)

  if(print.res$aborted) {  # If failed during eval retrieve conditions
    err.cond <-
      which(vapply(print.res$conditions, inherits, logical(1L), "error"))
    err.type <- if(identical(mode, "str")) "str"
      else if(identical(mode, "print"))
        if(isS4(obj)) "show" else "print"
      else stop("Logic Error: cannot figure out print mode; contact maintainer.")
    err.cond.msg <- if(length(err.cond)) {
      c(
        paste0(
          "<Error in ", err.type,
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
# constructs the full diff message with additional meta information

obj_screen_chr <- function(
  obj.chr, obj.name, obj.diffs, range, width, pad
) {
  pre <- post <- NULL
  extra <- paste0("; see `", obj.name, "`")
  len.obj <- length(obj.chr)

  if(length(pad)) {
    pad <- format(pad)
    pad.pre.post <- paste0(rep(" ", nchar(pad[[1L]])), collapse="")
  } else pad.pre.post <- character()

  if(len.obj) {
    omit.first <- max(min(range[[1L]] - 1L, len.obj), 0L)
    omit.last <- max(len.obj - tail(range, 1L), 0L)
    diffs.last <- sum(tail(obj.diffs, -tail(range, 1L)))

    if(omit.first)
      pre <- paste0("... omitted ", omit.first, " lines w/o differences")
    if(omit.last) {
      post <- paste0(
        "... omitted ", omit.first, " lines w/ ", diffs.last, " differences"
    ) }
    if(!is.null(post)) {
      post <- paste0(
        pad.pre.post,
        word_wrap(paste0(post, extra, " ..."), width - nchar(pad[[1L]]))
    ) }
    if (!is.null(pre)) {
      pre <- paste0(
        pad.pre.post,
        word_wrap(
          paste0(pre, if(is.null(post)) extra, " ..."), width - nchar(pad[[1L]])
    ) ) }
  }
  c(
    paste0("@@ ", obj.name, " @@"),
    paste0(c(pre, paste0(pad, obj.chr)[range[range <= len.obj]], post))
  )
}
#' Print Only First X characters
#'
#' @keywords internal
#' @param x string to reduce length
#' @param nchar.max how many characters to reduce each string to
#' @param ctd 1 length character vector for what to use to indicate string
#'   truncated
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

  # replace new lines with 0 char item

  x.lst[nchar(x) > 0] <- strsplit(gsub("\n", "\n\n", x[nchar(x) > 0]), "\n")

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
  cat(x$output, sep="\n")
  cat(x$message, sep="\n", file=stderr())
}

