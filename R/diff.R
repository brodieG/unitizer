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
    if(!is.chr1(object@mode) || ! object@mode %in% c("print", "str"))
      return("slot `mode` must be either \"print\" or \"str\"")
    if(length(object@tar.capt) != length(object@diffs@target))
      return("slot `tar.capt` must be same length as slot `diffs@target`")
    if(length(object@cur.capt) != length(object@diffs@current))
      return("slot `cur.capt` must be same length as slot `diffs@current`")
    TRUE
} )
#' @rdname unitizer_s4method_doc

setMethod("any", "unitizerDiff",
  function(x, ..., na.rm = FALSE) {
    dots <- list(...)
    if(length(dots))
      stop("`any` method for `unitizerDiff` supports only one argument")
    any(x@diffs)
} )
#' @rdname unitizer_s4method_doc

setMethod("any", "unitizerDiffDiffs",
  function(x, ..., na.rm = FALSE) {
    dots <- list(...)
    if(length(dots))
      stop("`any` method for `unitizerDiffDiffs` supports only one argument")
    any(x@target, x@current)
} )
#' @rdname unitizer_s4method_doc

setMethod("as.character", "unitizerDiff",
  function(x, context, width, ...) {
    context <- check_context(context)
    width <- check_width(width)
    # If there is an error, we want to show as much of the objects as we can
    # centered on the error.  If we can show the entire objects without centering
    # then we do that.

    len.max <- max(length(x@tar.capt), length(x@cur.capt))
    first.diff <- if(!any(x)) 1L else
      min(which(x@diffs@target), which(x@diffs@current))

    show.range <- if(len.max <= 2 * context[[1L]] + 1) {
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
    # Match up the diffs; first step is to match the matches since we know that
    # there are the exact same number of these in both

    matches <- sum(!x@diffs@target)
    tar.len <- length(x@diffs@target)
    tar.seq <- seq.int(tar.len)
    tar.ids <- integer(tar.len)
    cur.len <- length(x@diffs@current)
    cur.ids <- integer(cur.len)
    tar.ids[!x@diffs@target] <- seq.int(matches)
    cur.ids[!x@diffs@current] <- seq.int(matches)

    # Walk through the target vector and for each mismatch find best mismatch in
    # current vector to try do the wordiff agains

    index <- 0L
    safety <- 0L

    repeat {
      if(index >= tar.len) break
      if((safety <- safety + 1L) > tar.len)
        stop(
          "Logic Error: infinite loop detected trying to find mismatched lines; ",
          "contact maintainer"
        )
      mismatches.next <- which(!tar.ids & tar.seq > index)
      if(!length(mismatches.next)) break
      mismatch.next <- mismatches.next[[1L]]

      # most recent matched value
      prev.match <- if(mismatch.next == 1L) 0L else
        max(head(tar.ids, mismatch.next - 1L))

      # Need to find the unmatched values in current that are between
      # `prev.match` and `prev.match + 1`; matches are encoded as negative values

      prev.match.cur <- if(prev.match) {
        match(c(prev.match, prev.match + 1L), cur.ids)
      } else {
        c(0L, match(1L, cur.ids))
      }
      # If no more matching values, indicate one index past end of cur vector
      # since we'll be looking at index values less than that

      if(is.na(prev.match.cur[2L])) prev.match.cur[2L] <- cur.len + 1L

      # There are possible unmatched items in cur we can match to our unmatched
      # item in tar
      if(
        diff(prev.match.cur) > 1 &&
        length(cur.unmatch <- which(
          !cur.ids[seq(prev.match.cur[1L] + 1L, prev.match.cur[2L] - 1L)]
        ) )
      ) {
        cur.unm.id <- prev.match.cur[1L] + cur.unmatch[1L]
        tar.ids[mismatch.next] <- -cur.unm.id
        cur.ids[cur.unm.id] <- -mismatch.next
      }
      index <- mismatch.next
    }
    # Now extract the corresponding strings to compare

    tar.ids.mismatch <- -cur.ids[cur.ids < 0L & -cur.ids %in% show.range]
    cur.ids.mismatch <- -tar.ids[tar.ids < 0L & -tar.ids %in% show.range]

    # Add word colors

    tar.txt <- x@tar.capt
    cur.txt <- x@cur.capt

    word.color <-
      diff_word(tar.txt[tar.ids.mismatch], cur.txt[cur.ids.mismatch])

    tar.txt[tar.ids.mismatch] <- word.color$target
    cur.txt[cur.ids.mismatch] <- word.color$current

    # Color lines that were not word colored

    tar.seq <- seq_along(tar.ids)
    cur.seq <- seq_along(cur.ids)
    tar.line.diff <- x@diffs@target & !tar.seq %in% tar.ids.mismatch &
      tar.seq %in% show.range
    cur.line.diff <- x@diffs@current & !cur.seq %in% cur.ids.mismatch &
      cur.seq %in% show.range

    tar.txt[tar.line.diff] <- clr(tar.txt[tar.line.diff], color="red")
    cur.txt[cur.line.diff] <- clr(cur.txt[cur.line.diff], color="green")

    # Add all the display stuff

    pad.rem <- rep("   ", length(x@tar.capt))
    pad.add <- rep("   ", length(x@cur.capt))
    pad.rem[x@diffs@target] <- "-  "
    pad.add[x@diffs@current] <- "+  "
    c(
      obj_screen_chr(
        tar.txt,  x@tar.exp, diffs=x@diffs@target, range=show.range,
        width=width, pad=pad.rem, color="red"
      ),
      obj_screen_chr(
        cur.txt,  x@cur.exp, diffs=x@diffs@current, range=show.range,
        width=width, pad=pad.add, color="green"
    ) )
} )
# groups characters based on whether they are different or not and colors
# them; assumes that the chrs vector values are words that were previously
# separated by spaces, and collapses the strings back with the spaces at the
# end

color_words <- function(chrs, diffs, color) {
  stopifnot(length(chrs) == length(diffs))
  if(length(chrs)) {
    grps <- cumsum(c(0, abs(diff(diffs))))
    chrs.grp <- tapply(chrs, grps, paste0, collapse=" ")
    diff.grp <- tapply(diffs, grps, head, 1L)
    cc(diff_color(chrs.grp, diff.grp, seq_along(chrs.grp), color), c=" ")
  } else cc(chrs)
}

# Apply diff algorithm within lines; need to preselect which lines to line up
# with each other.
#
# For each line, splits into characters and groups them into alike and diff
# groups, colors them, collapses each back into one character value, and
# returns a list with target values and current values word colored.  The
# vectors within the list will have the same # of elements as the inputs

diff_word <- function(target, current) {
  stopifnot(
    is.character(target), is.character(current),
    length(target) == length(current)
  )
  # Compute the char by char diffs for each line

  tar.split <- strsplit(target, " ")
  cur.split <- strsplit(current, " ")
  diffs <- Map(char_diff, tar.split, cur.split)

  # Merge the sequences of equal/diff characters and then color them

  words.colored <- vapply(
    seq_along(diffs),
    function(i)
      c(
        target=color_words(tar.split[[i]], diffs[[i]]@target, "red"),
        current=color_words(cur.split[[i]], diffs[[i]]@current, "green")
      ),
    c(target=character(1L), current=character(1L))
  )
  # Restructure for return

  split(words.colored, rownames(words.colored))
}
# Apply line colors

diff_color <- function(txt, diffs, range, color) {
  stopifnot(
    is.character(txt), is.logical(diffs), !any(is.na(diffs)),
    length(txt) == length(diffs), is.integer(range), !any(is.na(range)),
    all(range > 0 & range <= length(txt)), is.chr1(color)
  )
  to.color <- diffs & seq_along(diffs) %in% range
  txt[to.color] <- clr(txt[to.color], color)
  txt
}
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
#'     of the two objects; will show as many recursive levels as possible so
#'     long as context lines are not exceeded, and if they are, as few as
#'     possible to show at least one error (see \code{max.level})
#'   \item \code{diff_obj} picks between \code{diff_print} and \code{diff_str}
#'     depending on which one it thinks will provide the most useful diff
#' }
#' @export
#' @param target the reference object
#' @param new the object being compared to \code{target}
#' @param context 2 length integer vector representing how many lines of context
#'   are shown on either side of differences.  The first value is the maximum
#'   before we start trimming output.  The second value is the maximum to be
#'   shown before we start trimming.  We will always attempt to show as much as
#'   \code{2 * context + 1} lines of output so context may not be centered if
#'   objects display as less than \code{2 * context + 1} lines.
#' @param max.level integer(1L) up to how many levels to try running \code{str};
#'   \code{str} is run repeatedly starting with \code{max.level=1} and then
#'   increasing \code{max.level} until we fill the context or a difference
#'   appears or the \code{max.level} specified here is reached.  If the value is
#'   reached then will let \code{str} run with \code{max.level} unspecified.
#'   This is designed to produce the most compact screen output possible that
#'   shows the differences between objects, though obviously it comes at a
#'   performance cost; set to 0 to disable
#' @return character, invisibly, the text representation of the diff

diff_obj <- function(target, current, context=NULL) {
  context <- check_context(context)
  frame <- parent.frame()
  width <- getOption("width")

  diff_obj_internal(
    target, current, tar.exp=substitute(target), cur.exp=substitute(current),
    context=context, frame=frame, width=width
  )
}
#' @rdname diff_obj
#' @export

diff_print <- function(target, current, context=NULL) {
  context <- check_context(context)
  width <- getOption("width")
  frame <- parent.frame()
  cat(
    as.character(
      diff_print_internal(
        target, current, tar.exp=substitute(target), frame=frame,
        cur.exp=substitute(current), context=context, width=width
      ),
      context=context,
      width=width
    ),
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
        frame=frame, max.lines=NULL, max.level=max.level
      ),
      context=context,
      width=width
    ),
    sep="\n"
  )
}
# Implements the diff_* functions
#
# @keywords internal
# @inheritParams diff_obj
# @param tar.exp the substituted target expression
# @param cur.exp the substituted current expression
# @param width at what width to wrap output
# @param file whether to show to stdout or stderr
# @param frame what frame to capture in, relevant mostly if looking for a print
#   method

diff_print_internal <- function(
  target, current, tar.exp, cur.exp, context, width, frame
) {
  cur.capt <- obj_capt(target, width, frame)
  tar.capt <- obj_capt(current, width, frame)
  diffs <- char_diff(tar.capt, cur.capt)
  new(
    "unitizerDiff", tar.capt=tar.capt, cur.capt=cur.capt, tar.exp=tar.exp,
    cur.exp=cur.exp, diffs=diffs, mode="print"
  )
}
diff_str_internal <- function(
  target, current, tar.exp, cur.exp, context, width, frame, max.lines,
  max.level=10
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
    if(lvl > 100) lvl <- NA # safety valve
    obj.add.capt.str <-
      obj_capt(current, width, frame, mode="str", max.level=lvl)
    obj.rem.capt.str <-
      obj_capt(target, width, frame, mode="str", max.level=lvl)
    str.len.min <- min(length(obj.add.capt.str), length(obj.rem.capt.str))
    str.len.max <- max(length(obj.add.capt.str), length(obj.rem.capt.str))

    # Overshot full displayable size; check to see if previous iteration had
    # differences

    if(str.len.max > max.lines && lvl > 1L && any(diffs.str)) {
      obj.add.capt.str <- obj.add.capt.str.prev
      obj.rem.capt.str <- obj.rem.capt.str.prev
      break
    }
    # Other break conditions

    if(
      is.na(lvl) || lvl >= max.level ||
      (
        identical(obj.add.capt.str.prev, obj.add.capt.str) &&
        identical(obj.rem.capt.str.prev, obj.rem.capt.str)
      )
    )
      break

    # Run differences and iterate

    diffs.str <- char_diff(obj.rem.capt.str, obj.add.capt.str)
    lvl <- lvl + 1
    obj.add.capt.str.prev <- obj.add.capt.str
    obj.rem.capt.str.prev <- obj.rem.capt.str
  }
  diffs <- char_diff(obj.rem.capt.str, obj.add.capt.str)
  tar.exp <- call("str", tar.exp, max.level=lvl)
  cur.exp <- call("str", cur.exp, max.level=lvl)
  new(
    "unitizerDiff", tar.capt=obj.rem.capt.str, cur.capt=obj.add.capt.str,
    tar.exp=tar.exp, cur.exp=cur.exp, diffs=diffs, mode="str"
  )
}
# Unlike diff_print_internal and diff_str_internal, this one prints to screen
# and invisibly returns the result

diff_obj_internal <- function(
  target, current, tar.exp=substitute(target),
  cur.exp=substitute(current), context=NULL, width=NULL,
  frame=parent.frame(), max.level=10L, file=stdout()
) {
  context <- check_context(context)
  width <- check_width(width)
  if(!isTRUE(file.err <- is.open_con(file, writeable=TRUE)))
    stop("Argument `file` is not valid because: ", file.err)
  if(!is.environment(frame)) stop("Argument `frame` must be an environment.")

  res.print <- diff_print_internal(
    target, current, tar.exp=tar.exp, cur.exp=cur.exp, context=context,
    width=width, frame=frame
  )
  len.print <- max(length(res.print@tar.capt), length(res.print@cur.capt))

  res.str <- diff_str_internal(
    target, current, tar.exp=tar.exp,
    cur.exp=cur.exp, context=context, width=width,
    frame=frame, max.lines=len.print
  )
  len.max <- context[[1L]] * 2 + 1
  len.str <- max(length(res.str@tar.capt), length(res.str@cur.capt))

  # Chose which display to use; only favor res.str if it really is substantially
  # more compact and it does show an error

  res <- if(
    (!any(res.str) || len.print < len.str * 3) &&
    !(len.print > len.max && len.str <= len.max)
  )
    res.print else res.str

  res.chr <- as.character(res, context, width=width)
  cat(res.chr, file=file, sep="\n")
  invisible(res.chr)
}

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
  as.integer(context)
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
  diffs.int <- char_diff_int(x, y)
  if(sum(!diffs.int[[1L]]) != sum(!diffs.int[[2L]]))
    stop(
      "Logic Error: diff produced unequal number of matching lines; contact ",
      "maintainer."
    )
  do.call("new", c(list("unitizerDiffDiffs"), diffs.int))
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

    # Try to see if difference exists in y, and if not see if any subsequent
    # line does exist, indicating deletions from x.
    # This grows vectors, but doesn't seem to be a huge performance issue at
    # the normal scale we run this.

    diff.found <- FALSE
    for(i in seq(first.diff, length(x), by=1L)) {
      n.match <- head(which(x[[i]] == tail(y, -first.diff)), 1L)
      if(length(n.match)) {
        tmp.res <- Recall(
          x[i:length(x)], y[(n.match[[1L]] + first.diff):length(y)]
        )
        x.d <- c(eq.so.far, eq.extra, tmp.res[[1L]])
        y.d <- c(eq.so.far, rep(TRUE, n.match[[1L]]), tmp.res[[2L]])
        diff.found <- TRUE
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
  if(!is.na(max.level) && (!is.int.1L(max.level) ||  max.level < 0))
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
          user_exp_str(obj, frame, quote(obj), max.level)
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
  obj.chr, obj.name, diffs, range, width, pad, color=NA_character_
) {
  pre <- post <- NULL
  obj.name.dep <- deparse(obj.name)[[1L]]
  extra <- character()
  len.obj <- length(obj.chr)

  if(length(pad)) {
    pad <- format(pad)
    pad.pre.post <- paste0(rep(" ", nchar(pad[[1L]])), collapse="")
  } else pad.pre.post <- character()

  if(len.obj) {
    omit.first <- max(min(range[[1L]] - 1L, len.obj), 0L)
    omit.last <- max(len.obj - tail(range, 1L), 0L)
    diffs.last <- sum(tail(diffs, -tail(range, 1L)))

    if(omit.first)
      pre <- paste0(
        "~~ omitted ", omit.first, " line", if(omit.first != 1L) "s",
        " w/o diffs"
      )
    if(omit.last) {
      post <- paste0(
        "~~ omitted ", omit.last, " line", if(omit.last != 1L) "s", " w/ ",
        diffs.last, " diff", if(diffs.last != 1L) "s"
    ) }
    if(!is.null(post)) {
      post <- clr(
        paste0(
          pad.pre.post,
          word_wrap(paste0(post, extra, " ~~"), width - nchar(pad[[1L]]))
        ),
        "silver"
    ) }
    if (!is.null(pre)) {
      pre <- clr(
        paste0(
          pad.pre.post,
          word_wrap(
            paste0(pre, if(is.null(post)) extra, " ~~"), width - nchar(pad[[1L]])
        ) ),
        "silver",
    ) }
  }
  c(
    clr(paste0("@@ ", obj.name.dep, " @@"), "cyan"),
    paste0(
      c(pre, paste0(clr(pad, color), obj.chr)[range[range <= len.obj]], post)
    )
  )
}
