# Copyright (C) 2020  Brodie Gaslam
# 
# This file is part of "unitizer"
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

## Helper Functions to Capture and Process stdout/err Output
##
## \code{set} functions set sinks, and \code{get} functions retrieve the
## captured output. There are two types of functions here:
##
## \itemize{
##   \item \code{set_text_capture} and \code{get_text_capture} are intended for
##     use in situations we know the code to be evaluated will not be setting
##     sinks of its own; note that \code{get_text_capture} undoes any sinks set
##     by \code{set_text_capture}
##   \item \code{set_capture} and \code{get_capture} are meant for use when
##     evaluating the tests as both stdout and stderr are handled
##     \code{get_capture} does *not* undo the sinks since we need to check after
##     getting the text that the sinks are still in a reasonable state
##     with \code{unsink_cons}
## }
## All output to \code{stdout} and \code{stderr} is capture in a single file for
## each of those streams.  The captures happen sequentially, and are read off
## by \code{\link{readChar}}.  It is important to note this method implies that
## the files grow throughout the entire test evaluation process, and are only
## dumped at the very end.  This is to avoid overhead from repeatedly creating
## and opening new connections.
##
## \code{set} functions will not actually set the sinks if the connections have
## a "waive" attribute set to TRUE.
##
## \code{get_text_capture} and set companion are kind of half-assed updated to
## use the new version of the \code{cons} argument.  The used to just take a
## simple connection, but the need to reset stderr output to the original error
## connection required the change, and it's a bit confusing because \code{cons}
## contains both connections, whereas these functions operate on one connection
## at a time.
##
## @param con either a file name or an open connection; make sure that you use
##   a \code{con} created by \code{set_text_capture} for \code{get_text_capture}
##   since \code{set_text_capture} detects whether sinking is already in process
##   and returns FALSE if it is, which then tells \code{get_text_capture} not to
##   undo the sink
## @param type character(1L) in \code{c("output", "message")}
## @param file.name character(1L) file location corresponding to \code{con}
## @param no.unsink logical(1L) for testing purposes so we don't release a sink
##   when none was actually set
## @return \itemize{
##   \item \code{set_text_capture}: a connection, with attribute "waive" set to
##     TRUE if the sink was already sunk and we did not sink it again
##   \item \code{get_text_capture}: character
## }
## @keywords internal

set_text_capture <- function(
  cons, type, capt.disabled=
    getOption("unitizer.disable.capt", c(output=FALSE, message=FALSE))
) {
  stopifnot(
    is(cons, "unitizerCaptCons"),
    type %in% c("output", "message"),
    is.valid_capt_setting(capt.disabled)
  )
  con <- slot(cons, if(type=="message") "err.c" else "out.c")
  if(!capt.disabled[[type]]) {
    sink(con, type=type)
  } else if(type == "output") sink(con, split=TRUE)
  return(cons)
}

get_text_capture <- function(
  cons, type, no.unsink=FALSE,
  chrs.max=getOption("unitizer.max.capture.chars", 200000L)
) {
  stopifnot(
    isTRUE(type %in% c("message", "output")),
    is(cons, "unitizerCaptCons"),
    is.TF(no.unsink)
  )
  if(
    !is.numeric(chrs.max) || length(chrs.max) != 1L || is.na(chrs.max) ||
    chrs.max < 100L
  ) {
    stop(
      "Argument `chrs.max` must be integer(1L) and greater ",
      "than 100L; using 200000L for now", immediate.=TRUE
  ) }
  if(identical(type, "message")) {
    if(!no.unsink) {
      sink(cons@stderr.con, type="message")
    }
  } else if (identical(type, "output")) {
    if(!no.unsink) sink()
  } else {
    # nocov start
    stop("Internal Error: unexpected connection type; contact maintainer.")
    # nocov end
  }
  return(get_text(slot(cons, if(type=="message") "err.c" else "out.c")))
}
# nocov start
# this stuff is all emergency sink release that can't be tested without messing
# up whatever test framework sink capture exists

release_sinks <- function(silent=FALSE) {
  release_stdout_sink(FALSE)
  release_stderr_sink(FALSE)
  if(!isTRUE(silent))
    message("All sinks released, even those established by test expressions.")
  NULL
}
release_stdout_sink <- function(silent=FALSE) {
  replicate(sink.number(), sink())
  if(!isTRUE(silent))
    message(
      "All stdout sinks released, even those established by test expressions."
    )
}
release_stderr_sink <- function(silent=FALSE) {
  if(!identical(sink.number(type="message"), 2L)) sink(type="message")
  if(!isTRUE(silent)) message("Stderr sink released.")
}
# nocov end
# Wrappers Around Capture functions
#
# These are intended specifically for calling during test evaluation since in
# that case we're trying to capture both streams, as opposed to during setup
# etc. where we just want to quickly capture some output like package warnings

get_capture <- function(
  cons, chrs.max=getOption("unitizer.max.capture.chars", 200000L)
) {
  stopifnot(is(cons, "unitizerCaptCons"))

  # Do message first, so we can see subsequent errors

  message <- get_text(cons@err.c, chrs.max=chrs.max)
  output <- get_text(cons@out.c, chrs.max=chrs.max)

  list(output=output, message=message)
}
set_capture <- function(
  cons, capt.disabled=
    getOption("unitizer.disable.capt", c(output=FALSE, message=FALSE))
) {
  stopifnot(is(cons, "unitizerCaptCons"), is.valid_capt_setting(capt.disabled))
  out.level <- sink.number()
  err.level <- sink.number(type="message")
  err.con <- try(getConnection(err.level))
  if(!identical(out.level, cons@stdout.level)) attr(cons@out.c, "waive") <- TRUE
  if(!identical(err.con, cons@stderr.con)) attr(cons@err.c, "waive") <- TRUE

  set_text_capture(cons, "message", capt.disabled=capt.disabled)
  set_text_capture(cons, "output", capt.disabled=capt.disabled)
  cons
}
# Just Pull Text From A Connection
#
# Need to make more robust...

get_text <- function(
  con, chrs.max=getOption("unitizer.max.capture.chars", 200000L)
) {

  # Read captured, do so with `readChar` for performance reasons, growing
  # buffer as needed up to maximum allowable capture

  chrs.prev <- 0
  chrs <- 1e4
  chrs.mlt <- 10
  res <- ""

  while(chrs.prev < chrs.max) {
    chrs <- min(chrs, chrs.max)
    chrs.extra <- chrs - chrs.prev
    capture <- readChar(con, chrs.extra)
    res <- paste0(res, capture)
    if(!length(capture) || nchar(capture) < chrs.extra) break
    chrs.prev <- chrs
    chrs <- chrs * chrs.mlt
  }
  if(chrs.prev >= chrs.max) {
    if((err.con.num <- sink.number(type="message")) != 2) {
      err.con <- getConnection(err.con.num)
      sink(type="message")  # temporarily clear so we can issue warning
    }
    warning(
      "Reached maximum text capture characters ", chrs.max,
      "; see `getOption(\"unitizer.max.capture.chars\")`",
      immediate. = TRUE
    )
    if(err.con.num != 2) sink(err.con, type="message")

    # Reset writing point to last point read

    pos <- seek(con, origin="current", rw="read")
    seek(con, pos, rw="write")
  }
  return(res)
}
# Unsink / Check That Sinks Still Reasonable
#
# If we can't account for sink status we need to waive all future capture
# attempts.

unsink_cons <- function(cons) {
  on.exit({
    # nocov start
    failsafe_con(cons)
    # nocov end
  })
  stopifnot(is(cons, "unitizerCaptCons"))
  out.level <- sink.number()
  err.level <- sink.number(type="message")
  err.con <- try(getConnection(err.level))

  # Checking stdout complicated because we have to verify the current sink is
  # actually pointed at the file we are using

  if(
    !identical(out.level, cons@stdout.level + 1L) ||
    !(inherits(cons@out.c, "file") && isOpen(cons@out.c))
  ) {
    attr(cons@out.c, "waive") <- TRUE
  } else {
    if(!is_stdout_sink(cons@out.c)) {
      attr(cons@out.c, "waive") <- TRUE
    } else sink()
  }
  # stderr check is pretty simple

  if(!isTRUE(all.equal(err.con, cons@err.c, check.attributes=FALSE))) {
    attr(cons@err.c, "waive") <- TRUE
  } else sink(cons@stderr.con, type="message")

  # Return possibly modified cons (waived)

  on.exit(NULL)
  cons
}
# Try to Deal With Sinks Gracefully on Failure
# nocov start
failsafe_con <- function(cons) {
  capt.try <- try(get_capture(cons))
  release_sinks()
  if(inherits(capt.try, "try-error")) {
    signalCondition(attr(capt.try, "condition"))
  } else {
    if(sum(nchar(capt.try$output))) cat(capt.try$output, "\n", sep="")
    if(sum(nchar(capt.try$message)))
      cat(capt.try$message, "\n", sep="", file=stderr())
  }
  meta_word_msg(
    "Problems managing stdout/stderr streams, so we have reset all sinks, ",
    "even those that may have been set prior to calling `unitizer`.", sep=""
  )
  invisible(NULL)
}
# nocov end

# Cleanup sink connections if possible
#
# @return logical(2L) indicating success in normal resetting of sinks

close_and_clear <- function(cons) {
  stopifnot(is(cons, "unitizerCaptCons"))
  status <- c(output=TRUE, message=TRUE)
  err.reset <- try(sink(cons@stderr.con, type="message"))
  if(inherits(err.reset, "try-error")) {
    status[["message"]] <- FALSE
    sink(type="message")
    meta_word_msg(
      "Unable to restore original message sink, setting back to normal stderr"
    )
  }
  if(isTRUE(attr(cons@out.c, "waive"))) {
    # if waived, we have not unsunk our original connection, so need to ensure
    # it is still around, @stdout.level refers to the level before we sunk it
    # ourselves, so we need the -1L to test that sink

    out.level <- sink.number()
    level.extra <- out.level - cons@stdout.level - 1L

    if(level.extra > 0) replicate(level.extra, sink())
    if(!is_stdout_sink(cons@out.c)){
      # nocov start
      replicate(sink.number(), sink())
      meta_word_msg(
        "Tests corrupted stdout sink stack; all stdout sinks cleared."
      )
      status[["output"]] <- FALSE
      # nocov end
    } else if(sink.number()) sink()
  }
  close(cons@err.c)
  close(cons@out.c)
  close(cons@dump.c)
  # Check to see if any output was stored in the dump files.  These in theory
  # should contain no output and are used primarily when running the comparisons
  # between new and reference objects

  if(length(dump.txt <- readLines(cons@dump.f))) {
    warning(
      "Test comparison functions appear to have produced output, which should ", 
      "not happen (see `?unitizer_sect` for more details).  If you did not ",
      "provide custom testing functions, contact maintainer.  First 50 lines ",
      "follow:\n",
      paste0(head(dump.txt, 50), "\n")
    )
  }
  file.remove(cons@err.f, cons@out.f, cons@dump.f)
  invisible(status)
}
# Check whether provided connection is active stdout capture stream
# note this writes to the connection to check, though it shouldn't matter
# in our typical use case since `get_text` should advance the pointer by
# what we write so when we retrieve the value our junk write should not be
# visible

is_stdout_sink <- function(con) {
  stopifnot(inherits(con,  "file") && isOpen(con))
  test.str <- "\n\n<unitizer sink test>\n\n"
  cat(test.str)
  test.str.echo <- get_text(con)
  identical(test.str, test.str.echo)
}
# Connection Tracking Objects

setClass(
  "unitizerCaptCons",
  slots=c(
    err.f="ANY", err.c="ANY", out.f="ANY", out.c="ANY",
    stdout.level="integer", stderr.level="integer",
    # whatever connection was set for sink #2 prior to running
    stderr.con="ANY",
    dump.f="ANY", dump.c="ANY"
  ),
  validity=function(object) {
    # Allow NULLs since that is how the con object is stored

    if(!is.null(object@err.f) && !is.chr1(object@err.f))
      return("Slot `err.f` must be character(1L)")
    if(
      !is.null(object@err.f) &&
      (!inherits(object@err.c, "file") || !isOpen(object@err.c))
    )
       return("Slot `err.c` must be an open file connection")
    if(!is.null(object@out.f) && !is.chr1(object@out.f))
      return("Slot `out.f` must be character(1L)")
    if(
      !is.null(object@out.f) &&
      (!inherits(object@out.c, "file") || !isOpen(object@out.c))
    )
       return("Slot `out.c` must be an open file connection")
    if(
      !is.null(object@dump.f) &&
      (!inherits(object@dump.c, "file") || !isOpen(object@dump.c))
    )
       return("Slot `dump.c` must be an open file connection")
    TRUE
    TRUE
  }
)
setMethod("initialize", "unitizerCaptCons", function(.Object, ...) {
  dots <- list(...)
  .Object@stdout.level <- sink.number()
  .Object@stderr.level <- sink.number(type="message")
  err.con <- try(getConnection(.Object@stderr.level))
  .Object@stderr.con <- if(!inherits(err.con, "try-error")) err.con

  if(length(dots)) callNextMethod() else {
    .Object@err.f <- tempfile()
    .Object@err.c <- file(.Object@err.f, "w+b")
    .Object@out.f <- tempfile()
    .Object@out.c <- file(.Object@out.f, "w+b")
    .Object@dump.f <- tempfile()
    .Object@dump.c <- file(.Object@dump.f, "w+b")
    .Object
  }
} )
setClassUnion("unitizerCaptConsOrNULL", c("unitizerCaptCons", "NULL"))

