#' Functions To Assist With Demo
#'
#' \code{fastlm_dir} returns the directory that the \code{unitizer.fastlm}
#' package is in, \code{prompt_to_proceed} stops demo evaluation until
#' user presses ENTER, \code{show_file}
#'
#' @note these functions are not for use outside of the unitizer demo
#'
#' @aliases prompt_to_proceed, fastlm_dir, show_file
#' @name demo
#' @rdname demo
#' @param version a number in 0:2
#' @return character(1L)
#' @export

fastlm_dir <- function(version) {
  unitizer.dir <- system.file(package="unitizer")
  paste0(unitizer.dir, "/example.pkgs/fastlm.", version)
}

#' @export
#' @rdname demo

`[Press ENTER to Continue]` <- function() invisible(readline())

#' @export
#' @rdname demo

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
#' @rdname demo

copy_fastlm_to_tmpdir <- function() {
  dir <- tempfile()
  if(inherits(try(dir.create(dir)), "try-error"))
    stop("Unable to create temporary directory '", dir, "'")
  untz.dir <- system.file(package="unitizer")
  fastlm.dir <- file.path(untz.dir, "example.pkgs", "fastlm.0")
  fastlm.files <- list.files(
    fastlm.dir, full.names=TRUE, include.dirs=TRUE, no..=TRUE
  )
  if(inherits(try(file.copy(fastlm.files, dir, recursive=TRUE)), "try-error"))
    stop("Unable to copy `fastlm` sources")
  dir
}
#' @export
#' @rdname demo

update_fastlm <- function(dir, version) {
  stopifnot(
    version %in% c("0.1.1", "0.1.2"),
    file_test("-d", dir),
    file_test("-f", file.path(dir, "DESCRIPTION")),
    file_test("-f", file.path(dir, "R", "fastlm.R"))
  )
  lm.dir <- switch(
    version, "0.1.1"="fastlm.1", "0.1.2"="fastlm.2",
    stop("Logic Error; unknown version")
  )
  untz.dir <- system.file(package="unitizer")
  lm.dir.full <- file.path(untz.dir, "example.pkgs", lm.dir)
  cpy.files <- c("DESCRIPTION", file.path("R", "fastlm.R"))
  cpy.from <- file.path(lm.dir.full, cpy.files)
  cpy.to <- file.path(dir, cpy.files)

  invisible(file.copy(cpy.from, cpy.to, overwrite=TRUE))
}
#' @export
#' @rdname demo

unitizer_check_demo_state <- function() {
  vars <- c(".unitizer.fastlm", ".unitizer.test.file")
  vars.exist <- logical(length(vars))
  for(i in seq_along(vars))
    vars.exist[[i]] <- exists(vars[[i]], envir=parent.frame(), inherits=FALSE)
  if(any(vars.exist)) {
    word_msg(
      "Variables", paste0("`", vars, "`", collapse=", "), " already exist, but",
      "must be overwritten for demo to proceed.  These could have been left",
      "over by a previous run of the demo that did not complete properly.\n"
    )
    choice <- simple_prompt("Overwrite variables?")
    if(!identical(choice, "Y")) stop("Cannot continue demo.")
    rm(list=vars[vars.exist], envir=parent.frame())
  }
}

#' @export
#' @rdname demo

unitizer_cleanup_demo <- function() {
  vars <- c(".unitizer.fastlm", ".unitizer.test.file")
  remove.packages("unitizer.fastlm", .libPaths()[[1L]])
  unlink(.unitizer.fastlm, recursive=TRUE)
  rm(list=vars, envir=parent.frame())
}

