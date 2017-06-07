# Returns a Character Function Name From A Language Object
#
# Note this doesn't really try to check too hard whether the \code{`x`} is
# indeed a function.
#
# @param x a call or a symbol
# @return character 1 length if a function name, NA if an anonymous function, or
#   character(0L) if neither

deparse_fun <- function(x) {
  if(is.symbol(x)) {
    as.character(x)
  } else if (is.call(x)) {
    NA_character_
  } else {
    character(0L)
  }
}
# Deparse, But Make It Look Like It Would On Prompt
#
# @param expr an expression or call
# @return character vector

deparse_prompt <- function(expr) {
  # if(!is.call(expr) || !is.expression(expr)) stop("Argument `expr` must be an expression ")
  prompt <- getOption("prompt")
  continue <- getOption("continue")
  pad.len <- max(nchar(c(prompt, continue)))
  expr.deparsed <-
    deparse(expr, width.cutoff=min(60L, (getOption("width") - pad.len)))
  if(length(expr.deparsed) < 1L) {
    # nocov start
    stop("Internal Error: don't know what to do with zero length expr")
    # nocov end
  }
  prompt.vec <- c(prompt, rep(continue, length(expr.deparsed) - 1L))
  paste0(prompt.vec, expr.deparsed)
}
# Remove any comment attributes
#
# Used by the internal deparse functions.  Really removes all attributes.
# Resorting to desperate measures due to the reference like behavior of
# expressions and messing with their attributes, most likely due to the
# srcref style environment attributes.

uncomment <- function(lang) {
  if(is.expression(lang)) {
    # should be a call or symbol or constant, not an expression
    # nocov start
    stop("Internal Error: unexpected expression; contact maintainer")
    # nocov end
  }
  lang.new <- if(!(missing(lang) || is.null(lang)))
   `attr<-`(lang, "comment", NULL) else lang
  if(is.call(lang.new) && length(lang.new) > 1)
    for(i in seq_along(lang.new)) {
      lang.tmp <- lang.new[[i]]
      if(!(missing(lang.tmp) || is.null(lang.tmp)))
        lang.new[[i]] <- Recall(lang.tmp)
    }
  lang.new
}
# Deparse, but only provide first X characters
#
# @param expr a language object
# @param len int a one length integer noting how many characters we want
# @param width passed on to

deparse_peek <- function(expr, len, width=500L) {
  if(!is.integer(len) || length(len) != 1L || len < 4L)
    stop("Argument `len` must be an integer greater than four")
  if(!is.integer(width) || length(width) != 1L || width < 1L)
    stop("Argument `width` must be an integer greater than zero")
  chr <- paste0(sub("\n", " ", deparse(uncomment(expr), width)), collapse="")
  if(nchar(chr) > len) {
    paste0(substr(chr, 1L, len -3L), "...")
  } else {
    chr
  }
}
# Used to generate character values to store in cached deparse list
#
# @param expr language to deparse
# @return character(1L)

deparse_call <- function(expr) paste0(deparse(uncomment(expr)), collapse="\n")

# Special Deparse
#
# Required to deal with language objects that contain non-language objects
# that have attributes.
#
# Not completely fool proof since you can probably created an object that nests
# call and non-call stuff repeatedly that would confuse this thing.
#
# This is just used to generate objects for tests, not actually part of
# \code{unitizer} proper

deparse_mixed <- function(expr, width.cutoff = 500L, control = "all", ...) {

  rec_lang <- function(expr) {
    if(!is.language(expr))
      stop("Internal Error: expecting language object")  # nocov
    if(length(expr) > 1L) {
      for(i in seq_along(expr)) {
        if(!is.language(expr[[i]])) {
          expr[[i]] <-
            parse(
              text=deparse(expr[[i]], width.cutoff, control, ...),
              keep.source=FALSE
            )[[1L]]
        } else expr[[i]] <- Recall(expr[[i]])
    } }
    expr
  }
  rec_norm <- function(expr) {
    if(is.recursive(expr) && !is.environment(expr)) {
      for(i in seq_along(expr)) {
        if(is.language(expr[[i]])) {
          expr[[i]] <- rec_lang(expr[[i]])
        } else {
          expr[[i]] <- Recall(expr[[i]])
    } } }
    expr
  }
  deparse(rec_norm(expr), width.cutoff=width.cutoff, control=control, ...)
}
