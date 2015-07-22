#' Helper Functions to Capture and Process stdout/err Output
#'
#' \code{`set_text_capture`} sets the sinks, while \code{`get_text_capture`}
#' retrieves the captured tests and releases the sinks with \code{`release_sinks`}.
#' \code{`get_capture`} is just a wrapper around \code{`get_text_capture`}.
#'
#' A lot of the logic here is devoted to detecting whether users set their
#' own sinks in the course of execution.
#'
#' All output to \code{stdout} and \code{stderr} is capture in a single file for
#' each of those streams.  The captures happen sequentially, and are read off
#' by \code{\link{readChar}}.  It is important to note this method implies that
#' the files grow throughout the entire test evaluation process, and are only
#' dumped at the very end.  This is to avoid overhead from repeatedly creating
#' and opening new connections.
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
#' @keywords internal
#' @aliases get_text_capture, get_capture, release_sinks, release_stdout_sink, release_stderr_sink

set_text_capture <- function(
  con, type, capt.disabled=getOption("unitizer.disable.capt", FALSE)
) {
  if(identical(type, "message")) {
    waive.capt <- !identical(sink.number(type="message"), 2L)
  } else if (identical(type, "output")) {
    waive.capt <- isTRUE(sink.number() > 0L)
  } else {
    stop("Argument `type` must be either \"message\" or \"output\"")
  }
  if(!(inherits(con, "file") && isOpen(con)))
    stop("Argument `con` must be an open file connection.")
  if(!waive.capt) {
    sink(con, type=type)
    return(con)
  }
  return(FALSE)
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
    if(!as.character(con) %in% rownames(showConnections()) ||
      !identical(showConnections()[as.character(con), 1], file.name)
    ) {
      stop(
        "Logic Error: ", type, " capture connection has been subverted; ",
        "either test code is manipulating connections, or there is a bug in ",
        "this code."
    ) }
    if(identical(type, "message")) {
      if(!no.unsink) release_stderr_sink(silent=TRUE)  # close sink since it is the same we opened
    } else if (identical(type, "output")) {
      if(isTRUE(sink.number() > 1L)) {  # test expression added a diversion
        stop(
          "Test expressions introduced additional stdout diversions, which is ",
          "not supported. If you don't believe the test expressions ",
          "introduced diversions (i.e. used `sink`), contact maintainer."
      ) }
      if(!no.unsink) sink()
    } else {
      stop("Logic Error: unexpected connection type; contact maintainer.")
    }
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
  } else if (!identical(con, FALSE)) {
    stop("Logic Error: argument `con` must be a file connection or FALSE")
  }
  return(character())
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
get_capture <- function(
  cons, display=getOption("unitizer.show.output", TRUE),
  chrs.max=getOption("unitizer.max.capture.chars", 200000L)
) {
  message <- get_text_capture(    # Do message first, so we can see subsequent errors
    cons$err.c, cons$err.f, "message", chrs.max=chrs.max
  )
  output <- get_text_capture(
    cons$out.c, cons$out.f, "output", chrs.max=chrs.max
  )
  if(isTRUE(display)) {
    cat(c(message, "\n"), file=stderr(), sep="\n")
    cat(c(output, "\n"), sep="\n")
  }
  list(output=output, message=message)
}
close_and_clear <- function(cons) {
  close(cons$err.c)
  close(cons$out.c)
  file.remove(cons$err.f, cons$out.f)
}

