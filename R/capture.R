#' Helper Functions to Capture and Process stdout/err Output
#'
#' \code{set} functions set sinks, and \code{get} functions retrieve the
#' captured output. There are two types of functions here:
#'
#' \itemize{
#'   \item \code{set_text_capture} and \code{get_text_capture} are intended for
#'     use in situations we know the code to be evaluated will not be setting
#'     sinks of its own; note that \code{get_text_capture} undoes any sinks set
#'     by \code{set_text_capture}
#'   \item \code{set_capture} and \code{get_capture} are meant for use when
#'     evaluating the tests as both stdout and stderr are handled
#'     \code{get_capture} does *not* undo the sinks since we need to check after
#'     getting the text that the sinks are still in a reasonable state
#'     with \code{unsink_cons}
#' }
#' All output to \code{stdout} and \code{stderr} is capture in a single file for
#' each of those streams.  The captures happen sequentially, and are read off
#' by \code{\link{readChar}}.  It is important to note this method implies that
#' the files grow throughout the entire test evaluation process, and are only
#' dumped at the very end.  This is to avoid overhead from repeatedly creating
#' and opening new connections.
#'
#' \code{set} functions will not actually set the sinks if the connections have
#' a "waive" attribute set to TRUE.
#'
#' @param con either a file name or an open connection; make sure that you use
#'   a \code{con} created by \code{set_text_capture} for \code{get_text_capture}
#'   since \code{set_text_capture} detects whether sinking is already in process
#'   and returns FALSE if it is, which then tells \code{get_text_capture} not to
#'   undo the sink
#' @param type charcter(1L) in \code{c("output", "message")}
#' @param file.name character(1L) file location corresponding to \code{con}
#' @param no.unsink logical(1L) for testing purposes so we don't release a sink
#'   when none was actually set
#' @return \itemize{
#'   \item \code{set_text_capture}: a connection, with attribute "waive" set to
#'     TRUE if the sink was already sunk and we did not sink it again
#'   \item \code{get_text_capture}: character
#' }
#' @keywords internal
#' @aliases get_text_capture, get_capture, release_sinks, release_stdout_sink,
#'   release_stderr_sink

set_text_capture <- function(
  con, type, capt.disabled=getOption("unitizer.disable.capt", FALSE)
) {
  stopifnot(
    inherits(con, "file") && isOpen(con),
    type %in% c("output", "message"),
    is.TF(capt.disabled)
  )
  if(!capt.disabled) sink(con, type=type)
  return(con)

  # Deal with different scenarios
  # message: easy, we have the old connection
  # output: a bit tougher, variations
  # - sink_num greater than expected: undo sinks back to our number, check that
  #   our number is what we think it is
  # - sink_num expected: check that our sink is what it should be
  # - sink_num less than expected: freak out?
  # Gets more complicated since we have to account that this can all change
  # between tests.  Do we not respect sinks established by tests across multiple
  # test expressions?
  # - If we don't respect, then process is pretty simple, mostly, just pull
  #   back to original sink level
  #
  # Either way:
  # - track original sink level and sink message file
  # - if sink level increases or sink message connection changes:
  #     - engage waive mode
  #     - when test file complete, restore originals
  #     - for output sink, confirm that once we get back to original sink number
  #       file is what we think it is
  #     - if not issue warning, after restoring sink message
  #
  # So, when we first create connections, we have to record sink num and prev
  # sink file.
}
#' @rdname set_text_capture
#' @keywords internal

get_text_capture <- function(
  con, file.name, type, no.unsink=FALSE,
  chrs.max=getOption("unitizer.max.capture.chars", 200000L)
) {
  if(
    !isTRUE(type %in% c("message", "output")) || !is.character(file.name) ||
    length(file.name) != 1L || !(inherits(con, "file") && isOpen(con) ||
    identical(con, FALSE))
  ) {
    stop("Logic Error: invalid arguments; contact maintainer.")
  }
  if(
    !is.numeric(chrs.max) || length(chrs.max) != 1L || is.na(chrs.max) ||
    chrs.max < 100L
  ) {
    stop(
      "Argument `chrs.max` must be integer(1L) and greater ",
      "than 100L; using 200000L for now", immediate.=TRUE
  ) }
  if(inherits(con, "file") && isOpen(con)) {
    if(identical(type, "message")) {
      if(!no.unsink) sink(type="message")
    } else if (identical(type, "output")) {
      if(!no.unsink) sink()
    } else {
      stop("Logic Error: unexpected connection type; contact maintainer.")
    }
    return(get_text(con))
  } else {
    stop("Logic Error: argument `con` must be an open file connection or FALSE")
  }
  return("") # used to be character()
}
release_sinks <- function(silent=FALSE) {
  release_stdout_sink(FALSE)
  release_stderr_sink(FALSE)
  if(!isTRUE(silent)) message("All sinks released, even those established by test expressions.")
  NULL
}
release_stdout_sink <- function(silent=FALSE) {
  if(!isTRUE(silent)) message("All stdout sinks released, even those established by test expressions.")
  replicate(sink.number(), sink())
}
release_stderr_sink <- function(silent=FALSE) {
  if(!isTRUE(silent)) message("Stderr sink released.")
  if(!identical(sink.number(type="message"), 2L)) sink(type="message")
}
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
  cons, capt.disabled=getOption("unitizer.disable.capt", FALSE)
) {
  stopifnot(is(cons, "unitizerCaptCons"), is.TF(capt.disabled))
  out.level <- sink.number()
  err.level <- sink.number(type="message")
  err.con <- try(getConnection(err.level))
  if(!identical(out.level, cons@stdout.level)) attr(cons@out.c, "waive") <- TRUE
  if(!identical(err.con, cons@stderr.con)) attr(cons@err.c, "waive") <- TRUE

  cons@err.c <-
    set_text_capture(cons@err.c, "message", capt.disabled=capt.disabled)
  cons@out.c <-
    set_text_capture(cons@out.c, "output", capt.disabled=capt.disabled)
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
    failsafe_con(cons)
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
    # Verify still sunk to proper file

    test.str <- "\n\n<unitizer sink test>\n\n"
    cat(test.str)
    test.str.echo <- get_text(cons@out.c)
    if(!identical(test.str, test.str.echo)) {
      attr(cons@out.c, "waive") <- TRUE
    } else sink()
  }
  # stderr check is pretty simple

  if(!isTRUE(all.equal(err.con, cons@err.c, check.attributes=FALSE))) {
    attr(cons@err.c, "waive") <- TRUE
  } else sink(type="message")

  # Return possibly modified cons (waived)

  on.exit(NULL)
  cons
}
# Try to Deal With Sinks Gracefully on Failure
#
failsafe_con <- function(cons) {
  capt.try <- try(get_capture(cons))
  release_sinks()
  if(inherits(capt.try, "try-error")) {
    signalCondition(attr(capt.try, "condition"))
  } else {
    cat(capt.try$output, "\n", sep="")
    cat(capt.try$message, "\n", sep="", file=stderr())
  }
  word_msg(
    "Problems managing stdout/stderr streams, so we have reset all sinks, ",
    "even those that may have been set prior to calling `unitizer`.", sep=""
  )
  invisible(NULL)
}
close_and_clear <- function(cons) {
  stopifnot(is(cons, "unitizerCaptCons"))
  err.reset <- try(sink(cons@stderr.con, type="message"))
  if(inherits(err.reset, "try-error")) sink(type="message")

  if(isTRUE(attr(cons@out.c, "waive"))) {
    # if waived, we have not unsunk our original connection, so need to ensure
    # it is still around

    out.level <- sink.number()
    level.extra <- out.level - cons@stdout.level

    if(level.extra > 0) replicate(level.extra, sink())
    test.str <- "\n\n<unitizer sink test>\n\n"
    cat(test.str)
    test.str.echo <- get_text(cons@out.c)
    if(!identical(test.str, test.str.echo)) {
      replicate(sink.number(), sink())
      word_msg("Tests corrupted stdout sink stack; all stdout sinks cleared.")
    } else sink()
  }
  close(cons@err.c)
  close(cons@out.c)
  file.remove(cons@err.f, cons@out.f)
}
# Connection Tracking Objects

setClass(
  "unitizerCaptCons",
  slots=c(
    err.f="ANY", err.c="ANY", out.f="ANY", out.c="ANY",
    stdout.level="integer", stderr.level="integer",
    stderr.con="ANY"  # this is whatever connection was set for sink #2 prior to running
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
    .Object
  }
} )
setClassUnion("unitizerCaptConsOrNULL", c("unitizerCaptCons", "NULL"))

