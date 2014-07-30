#' Deparse, But Make It Look Like It Would On Prompt
#'
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
    paste0(substr(chr, 1L, len), "...")
  } else {
    chr
  }
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
#' Print a header
#' 
#' @keywords internal
#' @aliases print.H2, print.H3, print.header
#' @param x a 1 length character vector
#' @param margin one of "both", "top", "bottom", "none", weather to add newlines at top or bottom
#' @return 1 length character vector
#' @export

print.H1 <- function(x, ...) {
  if(!is.character(x) || length(x) != 1L) stop("Argument `x` must be a 1 length character vector")
  if((width <- getOption("width")) < 5L) return(x)
  x <- c(
    paste0(c("+", rep("-", width - 2L), "+"), collapse=""), 
    paste0(
      "| ", 
      paste0(
        text.wrapped <- unlist(text_wrap(x, width - 4L), use.names=FALSE), 
        vapply((width - 4L) - nchar(text.wrapped), function(x) paste0(rep(" ", x), collapse=""), character(1L))
      ),
      " |"
    ),
    paste0(c("+", rep("-", width - 2L), "+"), collapse="")
  )
  NextMethod()
}
#' @export

print.H2 <- function(x, ...) {
  x <- header_help(x, pad.char="=")
  NextMethod()
}
#' @export

print.H3 <- function(x, ...) {
  x <- header_help(x, pad.char="-")
  NextMethod()
}
#' @export

print.header <- function(x, margin="bottom", ...) {
  if(!is.character(x)) stop("Argument `x` must be a character vector")
  margin.legal <- c("both", "none", "top", "bottom")
  if(!is.character(margin) || !isTRUE(margin %in% margin.legal)) stop("Argument `margin` must be in ", deparse(margin.legal))
  if(isTRUE(margin %in% c("both", "top"))) x <- paste0(c("", x), collapse="\n")
  if(isTRUE(margin %in% c("both", "bottom"))) x <- paste0(c(x, ""), collapse="\n")
  x <- paste0(c(x, ""), collapse="\n")
  cat(x)
  invisible(x)
}
#' Helper function for single line headers
#' 
#' @keywords internal
#' @param x the contents of the header
#' @param ... unused, for compatibility with print generic
#' @param pad.char which character to use to form the header structure

header_help <- function(x, ..., pad.char="-") {
  if(inherits(try(par.call <- sys.call(-1L), silent=TRUE), "try-error") ||
    !isTRUE(grepl("^print\\.H[1-9][0-9]*$", as.character(par.call[[1]])))
  ) {
    stop("This function may only be called from a print.H* function.")
  }
  stop2 <- function(msg) stop(simpleCondition(msg, par.call))
  if(!is.character(x) || length(x) != 1L) stop2("Argument `x` must be a 1 length character vector")
  if(!is.character(pad.char) || length(pad.char) != 1L || nchar(pad.char) != 1L) stop2("Argument `pad.char` must be a 1 length 1 character character vector.")
  if((width <- getOption("width")) < 8L) return(x)
  if(isTRUE(nchar(x) > width - 4L)) x <- paste0(substr(x, 1, width - 7L), "...")
  paste0(pad.char, " ", x, " ", paste0(rep_len(pad.char, width - 3L - nchar(x)), collapse=""), collapse="")
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
#' @export

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
  if(!is.character(x) || !is.numeric(width) || any(width < 1L) || !identical(round(width), as.numeric(width))) {
    stop("Arguments `x` and `width` must be character and integer like (all values >= 1) respectively")
  }
  if(!identical((length(x) %% length(width)), 0L)) {
    stop("Argument `x` must be a multiple in length of argument `width`")
  }
  mapply(
    x, width, SIMPLIFY=FALSE,
    FUN=function(x.sub, width.sub) {
      breaks <- ceiling(nchar(x.sub) / width.sub)
      substr(
        rep(x.sub, breaks), 
        start=(1:breaks - 1) * width.sub + 1, stop=(1:breaks) * width.sub
) } ) }

#' Compare Lists of Conditions
#' 
#' This is the default function for comparing conditions across the new and
#' reference runs of a test.  Comparison will fail if any of the conditions
#' are not all.equal.  Additionally, we check for the special "printed"
#' attribute on each conditions which indicate that the condition occurred
#' in the print/show methods as applied to the result of a call.
#' 
#' @keywords internal
#' @export
#' @param target the list of conditions that we are matching against
#' @param current the list of conditions we are checking
#' @return TRUE if the lists of conditions are equivalent, an character vector explaining
#'   why they are not otherwise

all.equal.condition_list <- function(target, current, ...) {
  if(
    !is.list(target) || !is.list(current) || 
    !all(vapply(target, inherits, FALSE, "condition")) ||
    !all(vapply(current, inherits, FALSE, "condition"))
  ) return("`target` or `current` are not both lists of conditions")

  print.show.err <- paste0(
    "Condition mismatch may involve print/show methods; carefully review ",
    "conditions with `getConds(.new)` and `getConds(.ref)` as just ",
    "typing `.ref` or `.new` at the prompt will invoke print/show methods, ",
    "which themselves may be the cause of the mismatch."
  )
  if(length(target) != length(current)) {
    return(
      c(
        paste0(
          "`target` and `current` do not have the same number of conditions (",
          length(target), " vs ", length(current), ")"
        ),
        if(any(unlist(lapply(append(target, current), attr, "printed")))) {
          print.show.err
        }
  ) ) }
  cond.len <- min(length(target), length(current))
  
  res <- lapply(
    seq(len=cond.len),
    function(x) {
      target.printed <- isTRUE(attr(target[[x]], "printed"))
      current.printed <- isTRUE(attr(current[[x]], "printed"))
      if(!is.null(attr(target[[x]], "printed"))) attr(target[[x]], "printed") <- NULL
      if(!is.null(attr(current[[x]], "printed"))) attr(current[[x]], "printed") <- NULL

      err.msg <- all.equal(target[[x]], current[[x]])
      if(!isTRUE(err.msg) && (target.printed || current.printed)) {
        err.msg <- c(err.msg, print.show.err)
      }
      err.msg
  } )
  errs <- which(vapply(res, is.character, logical(1L)))
  if((err.len <- length(errs)) == 1L) {
    err.msg <- "There is 1 condition mismatch; "
  } else  {
    err.msg <- paste0("There are ", err.len, " condition mismatches; ")
  }
  if(err.len) {
    return(
      c(
        paste0(err.msg, "showing first mismatch at condition #", errs[[1L]]),
        res[[errs[[1L]]]]
  ) ) }
  if(all(unlist(res))) return(TRUE)
  stop("Logic Error, unexpected return values from comparison function.")
}
#' Compare Conditions
#' 
#' @keywords internal
#' @export
#' @param target a condition
#' @param current another condition to compare
#' @return TRUE if conditions match, character vector describing differences otherwise

all.equal.condition <- function(target, current, ...) {
  if(!inherits(target, "condition") || !inherits(current, "condition")) 
    return("One of `target` or `current` is not a condition")
  if(
    !identical(
      type.targ <- get_condition_type(target), 
      type.curr <- get_condition_type(current)
    )
  ) {
    return(
      paste0(
        "Condition type mismatch, target is '", type.targ, 
        "', but current is '", type.curr, "'"
  ) ) }
  if(!isTRUE(all.equal(conditionMessage(target), conditionMessage(current))))
    return(paste0(type.targ, " condition messages do not match"))
  if(!isTRUE(all.equal(conditionCall(target), conditionCall(current))))
    return(paste0(type.targ, " condition calls do not match"))
  TRUE
}
#' Prints A list of Conditions
#' 
#' @keywords internal
#' @export
#' @param x a condition_list object (list of conditions)
#' @param width how many total chars the conditions should be displayed to
#' @return x, invisibly

print.condition_list <- function(x, width=getOption("width"), ...) {
  if(!length(x)) {
    cat("Empty condition list\n")
    return(invisible(x))
  }
  if(!is.list(x) || !all(vapply(x, inherits, logical(1L), "condition"))) {
    stop("Argument `x` must be a list of conditions")
  }
  out <- paste0(
    format(seq_along(x)), ": ", 
    ifelse(
      print.show <- vapply(x, function(x) isTRUE(attr(x, "printed")), logical(1L)),
      "[print] ", ""
    ),
    vapply(x, get_condition_type, character(1L)),
    " in "
  )
  desc.chars <- max(width - nchar(out), 20L)
  cond.detail <- vapply(x, FUN.VALUE=character(1L),
    function(y) {
      paste0(deparse(conditionCall(y))[[1L]], " : ", conditionMessage(y))
  } )
  out <- paste0(out, substr(cond.detail, 1, desc.chars))
  if(any(print.show)) {
    out <- c(out, "[print] means condition was issued in print/show method rather than in actual evaluation.")
  }
  cat(out, sep="\n")
  return(invisible(x))
}
#' Extracts Condition Type From Condition Classes
#' 
#' Type (e.g. Error, Warning), is taken to be the second to last class.
#' 
#' @keywords internal
#' @param x a condition
#' @return character 1 length the type of condition

get_condition_type <- function(x) {
  if(!inherits(x, "condition")) stop("Argument `x` must be a condition")
  classes <- rev(class(x))
  if(length(classes) < 2L || classes[[1L]] != "condition") "Unknown"
  cap_first(classes[[2L]])
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
#' @return character 1 length, NA if \code{`x`} can't possibly be a function

deparse_fun <- function(x) {
  if(is.symbol(x)) {
    as.character(x)
  } else if (is.call(x)) {
    "<anon.FUN>"
  } else {
    NA_character_
  }
}

#' Captalizes First Letter
#' 
#' @keywords internal
#' @param x character
#' @return character

cap_first <- function(x) {
  if(!is.character(x)) stop("Argument `x` must be a character vector.")
  ifelse(
    nchar(x) > 2L, 
    paste0(substr(toupper(x), 1L, 1L), substr(x, 2L, nchar(x))),
    ifelse(identical(nchar(x), 1L), substr(toupper(x), 1L, 1L), x)
  )
}
#' Retrieves Environment Ancestry
#'
#' @keywords internal
#' @param env the environment to start with
#' @param stop.env the environment to stop with

env_ancestry <- function(env, stop.env=globalenv()) {
  if(!is.environment(env) || !is.environment(stop.env)) stop("Arguments `env` and `stop.env` must both be environments")
  out <- character(0L)
  repeat {
    out <- c(out, env_name(env))
    if(identical(env, stop.env)) break
    if(identical(env, emptyenv())) stop("Hit empty environment while traveling up environment ancestry")
    env <- parent.env(env)
  }
  out
}
#' Gets Environment Name / Memory Code
#' 
#' Captures the name that \code{`\link{print.default}`} displays when one
#' prints and environment
#' 
#' @keywords internal
#' @param env an environemnt
#' @return character 1 length

env_name <- function(env) {
  if(!is.environment(env)) stop("Argument `env` must be an environment")
  sub("<environment: (.*)>", "\\1", capture.output(print.default(env)))
}

#' Functions To Ignore
#' 
#' Ignored functions are not considered tests if they are called from
#' the top level.
#' 
#' @keywords internal

funs.ignore <- list(base::`<-`, base::library)

#' Display helper function
#' 
#' @keywords internal

screen_out <- function(txt, max.len=getOption("unitizer.test.out.lines"), file=stdout()) {
  if(!is.numeric(max.len) || !length(max.len) == 2 || max.len[[1]] < max.len[[2]])
    stop("Argument `max.len` must be a two length numeric vector with first value greater than second")
  if(out.len <- length(txt)) {
    cat(txt[1L:min(out.len, if(out.len > max.len[[1]]) max.len[[2]] else Inf)], sep="\n", file=file)
    if(out.len > max.len[[1]]) {
      cat("... truncated", out.len - max.len[[2]], "lines, review object directly if you wish to see all output\n", file=stderr())
} } }

#' Overrides Default quit() Behavior
#' 
#' Necessary because quit short circuits the `on.exit` clean-up functions and 
#' would leave stuff in a weird state (history not reset, etc.).
#' 
#' This is used in \code{`\link{unitize}`}.
#'  
#' @keywords internal

unitizer_quit <- function(save = "default", status = 0, runLast = TRUE) {
  invokeRestart("quitExit", list(save=save, status=status, runLast=runLast))
}