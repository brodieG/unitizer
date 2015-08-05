#' Functions To Assist With Demo
#'
#' \code{fastlm_dir} returns the directory that the \code{unitizer.fastlm}
#' package is in, \code{prompt_to_proceed} stops demo evaluation until
#' user presses ENTER, \code{show_file}
#'
#' @note these functions are not for use outside of the unitizer demo
#'
#' @aliases prompt_to_proceed, fastlm_dir, show_file
#' @name unitizer_demo
#' @rdname unitizer_demo
#' @param version a number in 0:2
#' @return character(1L)
#' @export

fastlm_dir <- function(version) {
  unitizer.dir <- system.file(package="unitizer")
  paste0(unitizer.dir, "/example.pkgs/fastlm.", version)
}

#' @export
#' @rdname unitizer_demo

`[Press ENTER to Continue]` <- readline

#' @export
#' @rdname unitizer_demo

show_file <- function(f, width=getOption("width", 80L)) {
  stopifnot(is.chr1(f))
  txt <- try(readLines(f))
  if(inherits(txt, "try-error")) stop("Unable to open file")
  line.num <- seq_along(txt)
  line.chars <- max(nchar(line.num))
  txt.wrap <- lapply(txt, word_wrap, width=width - 7L)
  txt.wrap.lines <- vapply(txt.wrap, length, integer(1L))
  line.txt <-format(
    unlist(
      Map(function(x, y) c(x, rep("", y - 1L)), line.num, txt.wrap.lines)
    ),
    justify="right"
  )
  line.txt.chars <- nchar(line.txt[[1L]]) + 2L
  file.disp <- word_wrap(f, width=width - 4L, hyphens=FALSE)
  body <- paste0("| ", line.txt, " | ", format(unlist(txt.wrap)), " |")
  body.chrs <- nchar(body[[1L]])
  bar <- paste0(
    c(
      "+", rep("-", line.txt.chars), "+",
      rep("-",  body.chrs - line.txt.chars - 3L), "+"
    ),
    collapse=""
  )
  top.bar <- paste0(c("+", rep("-",  body.chrs - 2L), "+"), collapse="")
  file.disp[[1L]] <- paste0(
    c(file.disp[[1L]], rep(" ", body.chrs - nchar(file.disp[[1L]]) - 4L)),
    collapse=""
  )
  res <- c(top.bar, paste0("| ", format(file.disp), " |"), bar, body, bar)
  cat(res, sep="\n")
  invisible(res)
}
#' @export
#' @rdname unitizer_demo

copy_fastlm_to_tmpdir <- function() {
  dir <- tempfile()
  if(inherits(try(dir.create(dir)), "try-error"))
    stop("Unable to create temporary directory '", dir, "'")
  untz.dir <- system.file(package="unitizer")
  fastlm.dir <- file.path(untz.dir, "example.pkgs", "fastlm.0", "/")
  if(inherits(try(file.copy(fastlm.dir, dir, recursive=TRUE)), "try-error"))
    stop("Unable to copy `fastlm` sources")
  dir
}
