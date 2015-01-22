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
#' Print an object to screen
#'
#' Similar to \code{\link{screen_out}}, but starts with an object instead of a
#' character vector, and has more manipulation options.  Used primarily to allow
#' us to show \code{.new} and \code{.ref} when tests fails
#'
#' @keywords internal

obj_out <- function(
  obj, add, extra, width=getOption("width"),
  max.len=getOption("unitizer.test.fail.out.lines"), file=stdout()
) {
  pre <- if(add) "+   " else "-   "
  obj.out <- obj_capt(obj, width - nchar(pre[[1L]]))
  obj_chr_out(
    obj.out, add, extra=extra, file=file, max.len=max.len,
    width=width - nchar(pre[[1L]])
  )
}
obj_chr_out <- function(
  obj.out, add=TRUE, file=stdout(), extra=".new", pre="+   ",
  width=getOption("width"),
  max.len=getOption("unitizer.test.fail.out.lines")
) {
  if(!is.logical(add) || length(add) != 1L || is.na(add))
    stop("Argument `add` must be integer(1L) and not NA.")
  if(!is.character(extra) || length(extra) != 1L)
    stop("Argument `extra` must be character(1L).")
  if(!is.numeric(max.len) || length(max.len) != 2L)
    stop("Argument `max.len` must be a one long numeric/integer.")
  if(!length(obj.out)) return(invisible(character(1L)))
  max.len <- pmax(max.len, c(1L, 1L))
  if(length(obj.out) > max.len[[1L]]) {
    obj.out <- obj.out[1:max.len[[2L]]]
    addendum <-
      paste0(
        "... truncated ", length(obj.out) - max.len[[2L]],
        " lines, use `", extra, "` to see full result."
  ) }
  res <- paste(pre, obj.out)
  cat(res, sep="\n", file=file)
  width.old <- getOption("width")
  on.exit(option(width=width.old))
  option(width=width)


  invisible(res)
}
obj_capt <- function(obj, width=getOption("width")) {
  if(!is.numeric(width) || length(width) != 1L)
    stop("Argument `width` must be a one long numeric/integer.")
  width.old <- getOption("width")
  on.exit(options(width=width.old))
  width <- max(width, 10L)

  options(width=width)
  obj.out <- capture.output(if(isS4(obj)) show(obj) else print(obj))
  options(width=width.old)
  on.exit(NULL)
  obj.out
}
#' Deparse, But Make It Look Like It Would On Prompt
#'
#' @keywords internal
#' @param expr an expression or call
#' @return character vector

deparse_prompt <- function(expr) {
  # if(!is.call(expr) || !is.expression(expr)) stop("Argument `expr` must be an expression ")
  prompt <- getOption("prompt")
  continue <- getOption("continue")
  pad.len <- max(nchar(c(prompt, continue)))
  expr.deparsed <- deparse(expr, width.cutoff=min(60L, (getOption("width") - pad.len)))
  if(length(expr.deparsed) < 1L) {
    stop("Logic Error: don't know what to do with zero length expr")
  }
  prompt.vec <- c(prompt, rep(continue, length(expr.deparsed) - 1L))
  paste0(prompt.vec, expr.deparsed)
}
#' Deparse, but only provide first X characters
#'
#' @keywords internal
#' @param expr a language object
#' @param len int a one length integer noting how many characters we want
#' @param width passed on to

deparse_peek <- function(expr, len, width=500L) {
  if(!is.integer(len) || length(len) != 1L || len < 4L)
    stop("Argument `len` must be an integer greater than four")
  if(!is.integer(width) || length(width) != 1L || width < 1L)
    stop("Argument `width` must be an integer greater than zero")
  chr <- paste0(sub("\n", " ", deparse(expr, width)), collapse="")
  if(nchar(chr) > len) {
    paste0(substr(chr, 1L, len -3L), "...")
  } else {
    chr
  }
}
#' Used to generate character values to store in cached deparse list
#'
#' @keywords internal
#' @param expr language to deparse
#' @return character(1L)

deparse_call <- function(expr) {
  paste0(deparse(expr), collapse="")
}

#' Print Only First X characters
#'
#' @keywords internal
#' @param x string to reduce length
#' @param nchar.max how many characters to reduce each string to
#' @param ctd 1 length character vector for what to use to indicate string truncated
#' @param disambig logical 1L whether to disambiguate strings that end up
#'   the same after truncation (not currently implemented)

strtrunc <- function(x, nchar.max=getOption("width"), ctd="...", disambig=FALSE) {
  if(!identical(disambig, FALSE)) stop("Parameter `disambig` not implemented")
  if(!is.character(x)) stop("Argument `x` must be character")
  if(!is.character(ctd) || !identical(length(ctd), 1L)) stop("Argument `ctd` must be 1 length character")
  if(!is.numeric(nchar.max) || !identical(length(nchar.max), 1L)) stop("Argument `nchar.max` must be 1 length numeric")

  len.target <- nchar.max - nchar(ctd)
  if(len.target < 1L) stop("`nchar.max` too small, make bigger or make `ctd` shorter.")
  ifelse(nchar(x) <= nchar.max, x, paste0(substr(x, 1, len.target), ctd))
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
#' @param x character vector
#' @param width what width to wrap at
#' @param tolerance how much earlier than \code{width} we're allowed to wrap
#' @param hyphens whether to allow hyphenation
#' @return character

word_wrap <- function(x, width, tolerance=8L, hyphens=TRUE) {
  if(!is.character(x) || !is.integer(width) || length(width) != 1L || is.na(width))
    stop("Invalid arguments")
  stopifnot(
    is.integer(tolerance) && length(tolerance) == 1L && !is.na(tolerance) &&
    tolerance >= 0L
  )
  stopifnot(width > 4L && width - tolerance > 2L)
  width <- as.integer(width)
  x.exp <- unlist(strsplit(gsub("\n", "\n\n", x), "\n"))     # replace new lines with 0 char item
  res <- character(as.integer(sum(nchar(x)) / width) * 1.2) # estimated result size
  res.cnt <- 1

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
  unlist(lapply(x, break_char))
}


#' Cat but split by words to allow wrapping
#'
#' Parameters are the same as \code{`\link{cat}`}.  Word splitting and auto-fill
#' only happens if the \code{`sep`} argument is not provided.
#'
#' @keywords internal
#' @seealso \code{`\link{cat}`}

word_cat <- function(..., fill=TRUE) {
  par.frame <- parent.frame()
  matched.args <- as.list(match.call(definition=cat, expand.dots=FALSE))[-1L]
  if(!"fill" %in% names(matched.args)) matched.args <- c(matched.args, fill=fill)
  if("sep" %in% names(matched.args)) {
    matched.call <- match.call(definition=cat)
    matched.call[[1]] <- quote(cat)
    invisible(return(eval(matched.call, envir=par.frame)));
  }
  cat.args <- formals(cat)
  args.to.parse <- c(
    matched.args[setdiff(names(matched.args), names(cat.args))],
    if("..." %in% names(matched.args)) matched.args[["..."]]
  )
  args.to.forward <- matched.args[
    names(cat.args)[!names(cat.args) %in% "..." &
    names(cat.args) %in% names(matched.args)]
  ]
  args.evaled <- try(
    lapply(
      args.to.parse,
      function(x) {
        x.eval <- eval(x, envir=par.frame)
        if(is.character(x.eval)) unlist(strsplit(x.eval, " ")) else x.eval
  } ) )
  args.fwd.evaled <- try(lapply(args.to.forward, eval, par.frame))
  if(inherits(args.evaled, "try-error") || inherits(args.fwd.evaled, "try-error"))
    stop("Problem evaluating `...` arguments; see previous errors")
  invisible(do.call(cat, c(args.fwd.evaled, args.evaled)))
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

  cat("\r", rep(" ", max(min.width, max.width)), sep="")
  cat(paste0("\r", substr(x, 1, max(min.width, max.width))))
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

#' Returns a Character Function Name From A Language Object
#'
#' Note this doesn't really try to check too hard whether the \code{`x`} is
#' indeed a function.
#'
#' @keywords internal
#' @param x a call or a symbol
#' @return character 1 length if a function name, NA if an anonymous function, or
#'   character(0L) if neither

deparse_fun <- function(x) {
  if(is.symbol(x)) {
    as.character(x)
  } else if (is.call(x)) {
    NA_character_
  } else {
    character(0L)
  }
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


