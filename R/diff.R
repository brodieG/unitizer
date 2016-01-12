# Classes for tracking intermediate diff obj data
#
# DiffDiffs contains a slot corresponding to each of target and current where
# a TRUE value means the corresponding value matches in both objects and FALSE
# means that it does not match

setClass("unitizerDiffDiffs", slots=c(target="integer", current="integer"))
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
    first.diff <- if(!any(x)) {
      return("No visible differences between objects")
    } else min(which(x@diffs@target), which(x@diffs@current))

    show.range <- if(len.max <= 2 * context[[1L]] + 1) {
      1:len.max
    } else {
      rng.trim <- 2 * context[[2L]] + 1
      if(first.diff <= rng.trim) {
        # if can show first diff starting from beginning, do that
        1:rng.trim
      } else if (len.max - first.diff + 1 <= rng.trim) {
        # if first diff is close to end, then show through end
        tail(1:len.max, -rng.trim)
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
          "Logic Error: infinite loop detected trying to find mismatched ",
          "lines; contact maintainer"
        )
      mismatches.next <- which(!tar.ids & tar.seq > index)
      if(!length(mismatches.next)) break
      mismatch.next <- mismatches.next[[1L]]

      # most recent matched value
      prev.match <- if(mismatch.next == 1L) 0L else
        max(head(abs(tar.ids), mismatch.next - 1L))

      # Need to find the unmatched values in current that are between
      # `prev.match` and `prev.match + 1`; matches are encoded as negative values

      prev.match.cur <- if(prev.match) {
        match(c(prev.match, prev.match + 1L), abs(cur.ids))
      } else {
        c(0L, match(1L, abs(cur.ids)))
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

    c(
      obj_screen_chr(
        tar.txt,  x@tar.exp, diffs=x@diffs@target, range=show.range,
        width=width, pad= "-  ", color="red"
      ),
      obj_screen_chr(
        cur.txt,  x@cur.exp, diffs=x@diffs@current, range=show.range,
        width=width, pad= "+  ", color="green"
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
# Try to use fancier word matching with vectors and matrices

.brack.pat <- "^  *\\[\\d+\\]"

diff_word_print <- function(target, current) {
  # Need to make these arguments
  width <- 80L
  frame <- parent.frame()
  cur.capt <- obj_capt(current, width - 3L, frame)
  tar.capt <- obj_capt(target, width - 3L, frame)
  # should check default:
  # - default method equal to selected method
  # - dealing with atomic?

  # Separate out the stuff that can wrap (starts with index headers vs. not)

  cur.head <- find_brackets(cur.capt)
  tar.head <- find_brackets(tar.capt)

  if(length(cur.head) && length(tar.head)) {
    cur.body <- regexpr(sprintf("%s\\K.*", .brack.pat), cur.capt[cur.head])
    tar.body <- regexpr(sprintf("%s\\K.*", .brack.pat), tar.capt[tar.head])

    body.diff <- word_diff(
      regmatches(tar.capt[tar.head], tar.body),
      regmatches(cur.capt[cur.head], cur.body),
      across.lines=TRUE
    )
    regmatches(tar.capt[tar.head], tar.body) <- body.diff$target
    regmatches(cur.capt[cur.head], cur.body) <- body.diff$current

    cur.rest <- -cur.head
    tar.rest <- -tar.head
  } else {
    cur.rest <- seq_along(cur.capt)
    tar.rest <- seq_along(tar.capt)
  }
  # Everything else gets a normal line diff

  stop("not implemented yet")

  diffs <- char_diff(tar.capt[tar.rest], cur.capt[cur.rest])

}
# Determine if a string contains what appear to be standard index headers
#
# Returns index of elements in string that start with index headers.
# Note that it is permissible to have ouput that doesn't match brackets
# provided that it starts with brackets (e.g. attributes shown after break
# pattern)

find_brackets <- function(x) {
  stopifnot(is.character(x), all(!is.na(x)))
  matches <- regexpr(.brack.pat,  x)
  vals <- regmatches(x, matches)
  # the matching section must be uninterrupted starting from first line
  # and must have consisten formatting

  brackets <- which(cumsum(!nzchar(vals)) == 0L)
  vals.in.brk <- vals[brackets]
  nums.in.brk <- regmatches(vals.in.brk, regexpr("\\d+", vals.in.brk))

  if(
    length(brackets) && length(unique(nchar(vals.in.brk)) == 1L) &&
    length(unique(diff(as.integer(nums.in.brk)))) <= 1L
  ) {
    brackets
  } else integer(0L)
}
# Diff by matching lines
#
# The key additional
diff_line <- function(target, current) {
}

# Apply diff algorithm within lines; need to preselect which lines to line up
# with each other.
#
# For each line, splits into characters and groups them into alike and diff
# groups, colors them, collapses each back into one character value, and
# returns a list with target values and current values word colored.  The
# vectors within the list will have the same # of elements as the inputs

diff_word <- function(target, current, across.lines=FALSE) {
  stopifnot(
    is.character(target), is.character(current),
    all(!is.na(target)), all(!is.na(current)),
    is.TF(across.lines)
  )
  # Compute the char by char diffs for each line

  reg <- "-?\\d+(\\.\\d+)?(e-?\\d{1,3})?|\\w+|\\d+|[^[:alnum:]_[:blank:]]+"
  tar.reg <- gregexpr(reg, target)
  cur.reg <- gregexpr(reg, current)

  tar.split <- regmatches(target, tar.reg)
  cur.split <- regmatches(current, cur.reg)

  # Collapse into one line if we want to do the diff across lines, but record
  # item counts so we can reconstitute the lines at the end

  if(across.lines) {
    tar.lens <- vapply(tar.split, length, integer(1L))
    cur.lens <- vapply(tar.split, length, integer(1L))

    tar.split <- list(unlist(tar.split))
    cur.split <- list(unlist(cur.split))
  }
  diffs <- Map(char_diff, tar.split, cur.split)

  # Color

  tar.colored <- lapply(
    seq_along(tar.split),
    function(i)
      diff_color(
        tar.split[[i]], diffs[[i]]@target, seq_along(tar.split[[i]]), "red"
      )
  )
  cur.colored <- lapply(
    seq_along(cur.split),
    function(i)
      diff_color(
        cur.split[[i]], diffs[[i]]@current, seq_along(cur.split[[i]]), "green"
      )
  )
  # Reconstitute lines if needed

  if(across.lines) {
    tar.colored <- split(
      tar.colored[[1L]], rep(seq_along(tar.lens), tar.lens)
    )
    cur.colored <- split(
      cur.colored[[1L]], rep(seq_along(cur.lens), cur.lens)
    )
  }
  # Merge back into original

  regmatches(target, tar.reg) <- tar.colored
  regmatches(current, cur.reg) <- cur.colored

  list(target=target, current=current)
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
#' Show Diffs Between the Screen Display Versions of Two Objects
#'
#' Designed to highlight at a glance the \bold{display} differences between
#' two objects.  Lack of visual differences is not guarantee that the objects
#' are the same.  These functions are designed to help you quickly understand
#' the nature of differences between objects when they are known to be different
#' (e.g. not \code{identical} or \code{all.equal}).  The diff algorithms are far
#' from perfect and in some cases will likely make seemingly odd choices on what
#' to highlight as being different.
#'
#' These functions focus on the first display difference between two objects.
#' If you want to see the full object diff try \code{\link{Rdiff_obj}}.
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
#' @note: differences shown or reported by these functions may not be the
#'   totality of the differences between objects since display methods may not
#'   display all differences.  This is particularly true when using \code{str}
#'   for comparisons with \code{max.level} since differences inside unexpanded
#'   recursive levels will not be shown at all.
#' @export
#' @param target the reference object
#' @param current the object being compared to \code{target}
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
  res <- as.character(
    diff_print_internal(
      target, current, tar.exp=substitute(target), frame=frame,
      cur.exp=substitute(current), context=context, width=width
    ),
    context=context,
    width=width
  )
  cat(res, sep="\n")
  invisible(res)
}
#' @rdname diff_obj
#' @export

diff_str <- function(target, current, context=NULL, max.level=10) {
  width <- getOption("width")
  frame <- parent.frame()
  res <- as.character(
    diff_str_internal(
      target, current, tar.exp=substitute(target),
      cur.exp=substitute(current), context=context, width=width,
      frame=frame, max.lines=NULL, max.level=max.level
    ),
    context=context,
    width=width
  )
  cat(res, sep="\n")
  invisible(res)
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
  cur.capt <- obj_capt(current, width - 3L, frame)
  tar.capt <- obj_capt(target, width - 3L, frame)
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

  prev.lvl <- 0L
  lvl <- 1L
  repeat{
    if(lvl > 100) lvl <- NA # safety valve
    obj.add.capt.str <-
      obj_capt(current, width - 3L, frame, mode="str", max.level=lvl)
    obj.rem.capt.str <-
      obj_capt(target, width - 3L, frame, mode="str", max.level=lvl)
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

    if(is.na(lvl) || lvl >= max.level) break
    if(
      identical(obj.add.capt.str.prev, obj.add.capt.str) &&
      identical(obj.rem.capt.str.prev, obj.rem.capt.str)
    ) {
      lvl <- prev.lvl
      break
    }
    # Run differences and iterate

    diffs.str <- char_diff(obj.rem.capt.str, obj.add.capt.str)
    obj.add.capt.str.prev <- obj.add.capt.str
    obj.rem.capt.str.prev <- obj.rem.capt.str
    prev.lvl <- lvl
    lvl <- lvl + 1
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

  # Choose which display to use; only favor res.str if it really is substantially
  # more compact and it does show an error and not possible to show full print
  # diff in context

  res <- if(
    (len.print <= len.max && any(res.print)) ||
    !any(res.str) ||
    (len.print < len.str * 3 && len.str > len.max)
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
  if(sum(!diffs.int[[1L]], na.rm=TRUE) != sum(!diffs.int[[2L]], na.rm=TRUE))
    stop(
      "Logic Error: diff produced unequal number of matching lines; contact ",
      "maintainer."
    )
  do.call("new", c(list("unitizerDiffDiffs"), diffs.int))
}
# Helper function encodes matches within mismatches so that we can later word
# diff the mismatches
match_mismatch <- function(x, y) {
  mis.overlap <- min(x, y)
  mis.extra <- max(x, y) - mis.overlap
  mis.seq <- seq_len(mis.overlap)
  mis.x <- x > y

  # construct final match vector, any additional mismatches in one or
  # other vector are mismatched and encoded as NAs

  x.d <- c(mis.seq, rep(NA_integer_, if(mis.x) mis.extra else 0L))
  y.d <- c(mis.seq, rep(NA_integer_, if(!mis.x) mis.extra else 0L))

  list(target=x.d, current=y.d)
}
char_diff_int <- function(x, y) {
  # REALLY WANT TO CHANGE THIS TO RETURN MISMATCHES LINED UP WITH EACH OTHER
  # SO THAT RETURN VALUE HAS 0L FOR MATCHES, #L FOR MISMATCHES THAT LINE UP
  # ACROSS THE TWO VECTORS, AND NA OR SOME OTHER VALUE FOR THOSE THAT DONT
  stopifnot(
    is.character(x), is.character(y), !any(is.na(c(x, y)))
  )
  # find first difference

  x.d <- y.d <- logical()
  len.match <- min(length(x), length(y))
  eq <- head(x, len.match) == head(y, len.match)
  diffs <- which(!eq)
  if(!length(diffs)) {
    # extras at end
    x.d <- c(rep(0L, len.match), c(rep(NA_integer_, length(x) - len.match)))
    y.d <- c(rep(0L, len.match), c(rep(NA_integer_, length(y) - len.match)))
  } else {
    first.diff <- diffs[[1L]]
    eq.so.far <- rep(0L, first.diff - 1L)
    eq.extra <- 0L

    # Try to see if difference exists in y, and if not see if any subsequent
    # line does exist, indicating deletions from x.  However, make sure that
    # we don't have the same number of matches in x as well as in y, as that
    # would suggest the match in y is just a coincidence

    # This grows vectors, but doesn't seem to be a huge performance issue at
    # the normal scale we run this.

    diff.found <- FALSE
    for(i in seq(first.diff, length(x), by=1L)) {
      n.match.self <- which(x[[i]] == tail(x, -i))
      n.match <- which(
        x[[i]] == tail(y, if(first.diff == 1L) Inf else  -first.diff + 1L)
      )
      if(length(n.match) && length(n.match) > length(n.match.self)) {
        tmp.res <- Recall(
          x[i:length(x)], y[(n.match[[1L]] + first.diff - 1L):length(y)]
        )
        # compute matched mismatches and line them up so they have the same
        # non-zero non-NA integer value in both x and y

        m.match <- match_mismatch(eq.extra, n.match[[1L]] - 1L)

        # re-adjust recursion values by how many matched mismatches we have
        m.max <- max(0L, unlist(m.match), na.rm=TRUE)
        tmp.res <- lapply(
          tmp.res, function(y) ifelse(!is.na(y) & !!y, y + m.max, y)
        )
        # construct final match vector, any additional mismatches in one or
        # other vector are mismatched and encoded as NAs
        x.d <- c(eq.so.far, m.match[[1L]], tmp.res[[1L]])
        y.d <- c(eq.so.far, m.match[[2L]], tmp.res[[2L]])
        diff.found <- TRUE
        break
      }
      eq.extra <- eq.extra + 1L
    }
    if(!diff.found) {  # Difference did not exist in y
      m.match <- match_mismatch(eq.extra, length(y) - first.diff + 1L)
      x.d <- c(eq.so.far, m.match[[1L]])
      y.d <- c(eq.so.far, m.match[[2L]])
    }
  }
  list(target=x.d, current=y.d)
}
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
  # remove trailing spaces; shouldn't have to do it but doing it since legacy
  # tests remove them and PITA to update those

  obj.out <- sub("\\s*$", "", obj.out)

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
  stopifnot(is.chr1(pad))
  pre <- post <- NULL
  pad.all <- pad.pre.post <- NULL
  obj.name.dep <- deparse(obj.name)[[1L]]
  extra <- character()
  len.obj <- length(obj.chr)

  if(len.obj) {
    pad.all <- character(length(diffs))
    pad.chars <- nchar(pad)
    if(!any(diffs)) {
      pad.all <- replicate(len.obj, cc(rep(" ", pad.chars)))
    } else {
      pad.all[diffs] <- pad
      pad.all <- format(pad.all)
    }
    pad.all[diffs] <- clr(pad.all[diffs], color)
    pad.pre.post <- paste0(rep(" ", pad.chars), collapse="")

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
        "~~ omitted ", omit.last, " line", if(omit.last != 1L) "s",
        if(diffs.last) cc(" w/ ", diffs.last, " diff") else " w/o diff",
        if(diffs.last != 1L) "s"
    ) }
    if(!is.null(post)) {
      post <- clr(
        paste0(
          pad.pre.post,
          word_wrap(paste0(post, extra, " ~~"), width - pad.chars)
        ),
        "silver"
    ) }
    if (!is.null(pre)) {
      pre <- clr(
        paste0(
          pad.pre.post,
          word_wrap(
            paste0(pre, if(is.null(post)) extra, " ~~"), width - pad.chars
        ) ),
        "silver",
    ) }
  }
  c(
    clr(paste0("@@ ", obj.name.dep, " @@"), "cyan"),
    paste0(
      c(pre, paste0(pad.all, obj.chr)[range[range <= len.obj]], post)
    )
  )
}
