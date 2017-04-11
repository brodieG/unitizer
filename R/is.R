# Check Whether File Connection Are Valid
#
# Use \code{`is.open_con`} to verify that a connection is open in addition to being
# valid
#
# @keywords internal
# @aliases is.open_con
# @param x object to test
# @param file.name 1 length character the name of the file that \code{`x`}
#   must point to
# @param readable/writeable whether file must be readable or writeable NA if
#   you don't care
# @return TRUE if valid, 1 length character vector if not explaining why it's not

is.valid_con <- function(x, file.name=NULL, readable=NA, writeable=NA) {
  if(!is.null(file.name) && !is.chr1(file.name))
      stop("Argument `file.name` must be NULL or a one length character vector.")
  if(!is.lgl.1L(readable) || !is.lgl.1L(writeable))
    stop("Arguments `readable` and `writeable` must be logical(1L)")

  # Basic checks

  if(!inherits(x, c("file", "connection")))
    return("must inherit from \"file\" and \"connection\"")
  if(!is.integer(x)) return("must be an integer")
  if(inherits(try(x.chr <- as.character(x)), "try-error"))
    return("cannot retrieve connection name to test")
  cons <- showConnections(all=TRUE)
  if(!isTRUE(x.chr %in% rownames(cons)))
    return("connection does not exist in `showConnections`")

  # Check r/w status

  rw <- list(writeable=writeable, readable=readable)
  rw.s <- list(writeable="write", readable="read")
  for(i in names(rw))
    if(!is.na(rw[[i]]))
      if(
        (cons[x.chr, sprintf("can %s", rw.s[[i]])] == "yes") != rw[[i]]
      )
        return(
          cc(
            "connection is ", if(rw[[i]]) "not ", i, " but should ",
            if(!rw[[i]]) "not ", "be ", i
        ) )

  # Match file name

  if(!is.null(file.name)) {
    if(!identical(file.name, cons[x.chr, "description"]))
      return("file name does not match connection description")
  }
  return(TRUE)
}
is.open_con <- function(x, file=NULL, readable=NA, writeable=NA) {
  if(!isTRUE(msg <- is.valid_con(x, file, readable, writeable))) return(msg)
  if(!isOpen(x)) return("must be an open connection")
  return(TRUE)
}
# Confirm Object is In \code{package_version} form
# @keywords internal

is.package_version <- function(x)
  inherits(x, "package_version") && inherits(x, "numeric_version") &&
  is.list(x) && identical(length(x), 1L)

# Test for plain characterness
#
# Test for common scalar cases that we run into ALL THE TIME!!!
#
# @rdname is.simpleobj
# @keywords internal
# @param x object to test

is.chr1plain <- function(x)
  !is.object(x) && is.character(x) && identical(length(x), 1L)

# @rdname is.simpleobj
# @keywords internal

is.chr1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

# @rdname is.simpleobj
# @keywords internal

is.TF <- function(x) isTRUE(x) || identical(x, FALSE)

# @rdname is.simpleobj
# @keywords internal

is.lgl.1L <- function(x) is.logical(x) && length(x) == 1L

# @rdname is.simpleobj
# @keywords internal

is.int.pos.2L <- function(x)
  is.numeric(x) && length(x) == 2L && !any(is.na(x)) &&
  all.equal(x, round(x)) && all(x > 0L)

is.int.pos.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !any(is.na(x)) &&
  all.equal(x, round(x)) && all(x > 0L)

is.int.1L <- function(x)
  is.numeric(x) && length(x) == 1L && !any(is.na(x)) && all.equal(x, round(x))

is.screen.out.vec <- function(x)
  is.numeric(x) && length(x) == 2L && !any(is.na(x)) && all(x > 1) &&
  x[1] >= x[2] && all.equal(round(x), x)

is.context.out.vec <- function(x)
  is.numeric(x) && length(x) == 2L && !any(is.na(x)) && all(x > 0) &&
  x[1] >= x[2] && all.equal(round(x), x)

# Check Whether Provided Store ID Is in Default Form
#
# @keywords internal

is.default_unitizer_id <- function(x) is.chr1plain(x) && !is.na(x)

is.valid_capt_setting <- function(x) {
  if(
    !is.logical(x) || length(x) != 2L || any(is.na(x)) ||
    !identical(names(x), c("output", "message"))
  ) {
    meta_word_msg(
      "value must be logical(2L) containing TRUE ",
      "/ FALSE and with names `c(\"output\", \"message\")"
    )
    return(FALSE)
  }
  TRUE
}

is.two_arg_fun <- function(x) {
  if(!is.function(x)) {
    "is not a function"
  } else if(
    length(formals(x)) < 2L &&
    !identical(head(names(formals(x)), 1L), "...")
  ) {
    "does not have at least two arguments"
  } else {
    nm.forms <- vapply(formals(x), is.name, logical(1L))
    forms.chr <- character(length(nm.forms))
    forms.chr[nm.forms] <- as.character(formals(x)[nm.forms])
    if(
      any(
        tail(!nzchar(forms.chr) & nm.forms & names(nm.forms) != "..." , -2L)
      ) && !identical(head(names(nm.forms), 1L), "...")
    )
      "cannot have any non-optional arguments other than first two" else TRUE
  }
}

