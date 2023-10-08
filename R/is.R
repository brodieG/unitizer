# Copyright (C) Brodie Gaslam
#
# This file is part of "unitizer - Interactive R Unit Tests"
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

