#' Demo Details and Helper Functions
#'
#' \code{unitizer} provides an interactive demo you can run with
#' \code{demo("unitizer")}.
#'
#' @section Demo Details:
#'
#' The demo centers around simulated development of the \code{unitizer.fastlm}
#' package.  \code{unitizer} includes in its sources three copies of the source
#' code for the \code{unitizer.fastlm} package, each at a different stage of
#' development.  This allows us to create reference \code{unitizer} tests under
#' one version, move to a new version and check for regressions, and finally
#' fix the regressions with the last version.  The version switching is
#' intended to represent the package development process.
#'
#' The demo manages the \code{unitizer.fastlm} code changes, but between each
#' update allows the user to interact with \code{unitizer}.  The demo operates
#' under the assumption that the user will accept the first set of tests and
#' reject the failing tests after the first update.  If the user does anything
#' different then the demo commentary may not apply anymore.
#'
#' @section \code{unitizer.fastlm}:
#'
#' \code{unitizer.fastlm} is a "dummy" package that implements a faster
#' computation of slope, intercept, and R^2 for single variable linear
#' regressions than is available via \code{summary(lm()...)}.
#'
#' @section Helper Functions:
#'
#' \code{copy_fastlm_to_tmpdir} copies the initial version of the
#' \code{unitizer.fastlm} sources to a temporary directory, \code{show_file}
#' displays the contents of a source code file, \code{update_fastlm} changes the
#' source code of \code{unitizer.fastlm}, and \code{unitizer_check_demo_state}
#' and \code{unitizer_cleanup_demo} perform janitorial functions.  None of
#' these functions are intended for use outside of the unitizer demo.
#'
#' @aliases fastlm_dir show_file unitizer_check_demo_state unitizer_cleanup_demo
#'   `[Press ENTER to Continue]`
#' @rdname demo
#' @name demo
#' @param f path to a file
#' @param dir path to the temporary package
#' @param width display width in characters
#' @param version one of "0.1.0", "0.1.1", "0.1.2"
#' @return character(1L)

NULL

# nocov start
#' @export
#' @rdname demo

`[Press ENTER to Continue]` <- function() invisible(readline())
# nocov end

#' @export
#' @rdname demo

show_file <- function(f, width=getOption("width", 80L)) {
  stopifnot(is.chr1(f))
  txt <- try(readLines(f))
  pkg.dir <- get_package_dir(f)
  if(length(pkg.dir)) f <- relativize_path(f, pkg.dir)
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
  body <- paste0("| ", line.txt, " | ", format(unlist(txt.wrap)), " |")
  body.chrs <- nchar(body[[1L]])
  file.disp <- word_wrap(f, width=nchar(body[[1L]]) - 4L, hyphens=FALSE)
  bar <- paste0(
    c(
      "+", rep("-", line.txt.chars), "+",
      rep("-",  body.chrs - line.txt.chars - 3L), "+"
    ),
    collapse=""
  )
  top.bar <- paste0(c("+", rep("-",  body.chrs - 2L), "+"), collapse="")
  file.disp[[1L]] <- paste0(
    c(
      file.disp[[1L]],
      rep(" ", max(body.chrs - nchar(file.disp[[1L]]) - 4L, 0L))
    ),
    collapse=""
  )
  res <- c(top.bar, paste0("| ", format(file.disp), " |"), bar, body, bar)
  cat(res, sep="\n")
  invisible(res)
}
#' @export
#' @rdname demo

copy_fastlm_to_tmpdir <- function() {
  dir <- file.path(tempfile(), "unitizer.fastlm")
  if(inherits(try(dir.create(dir, recursive=TRUE)), "try-error"))
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
# Helper fun for update_fastlm_*

.test.core.files <- c(
  "DESCRIPTION", file.path("R", "fastlm.R"),
  file.path(
    "tests", "unitizer", c("fastlm1.R", "fastlm2.R", "unitizer.fastlm.R")
) )
check_test_dir <- function(dir) {
  stopifnot(
    file_test("-d", dir),
    file_test("-d", file.path(dir, "tests", "unitizer")),
    all(file_test("-f", file.path(dir, .test.core.files)))
  )
}
#' @export
#' @rdname demo

update_fastlm <- function(dir, version) {
  check_test_dir(dir)
  stopifnot(version %in% c("0.1.0", "0.1.1", "0.1.2"))
  lm.dir <- switch(
    version, "0.1.0"="fastlm.0", "0.1.1"="fastlm.1", "0.1.2"="fastlm.2",
    stop("Logic Error; unknown version")
  )
  untz.dir <- system.file(package="unitizer")
  lm.dir.full <- file.path(untz.dir, "example.pkgs", lm.dir)
  cpy.files <- .test.core.files
  cpy.from <- file.path(lm.dir.full, cpy.files)
  cpy.to <- file.path(dir, cpy.files)

  invisible(file.copy(cpy.from, cpy.to, overwrite=TRUE))
}
# copy extra file for tests, this is primarily just for the section tests and
# should be used with care as it will mess up all the other tests by adding
# an extra file.  This also installs version 0.1.2

update_fastlm_extra <- function(dir) {
  check_test_dir(dir)
  lm.dir <- "fastlm.2"
  untz.dir <- system.file(package="unitizer")
  lm.dir.full <- file.path(untz.dir, "example.pkgs", lm.dir)
  file.extra <- file.path("tests", "unitizer", "unitizer.fastlm2.R")
  stopifnot(
    file_test("-f", file.path(lm.dir.full, file.extra))
  )
  cpy.files <- c(.test.core.files, file.extra)
  cpy.from <- file.path(lm.dir.full, cpy.files)
  cpy.to <- file.path(dir, cpy.files)

  invisible(file.copy(cpy.from, cpy.to, overwrite=TRUE))
}
# nocov start
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
# nocov end

#' @export
#' @rdname demo

unitizer_cleanup_demo <- function() {
  vars <- c(".unitizer.fastlm", ".unitizer.test.file")
  try(detach("package:unitizer.fastlm"), silent=TRUE)
  try(unloadNamespace("unitizer.fastlm"), silent=TRUE)
  remove.packages("unitizer.fastlm", .libPaths()[[1L]])
  pkg.dir <- try(get(".unitizer.fastlm", envir=parent.frame()))
  if(
    !inherits(pkg.dir, "try-error") && is.chr1plain(pkg.dir) &&
    file_test("-d", pkg.dir) && grepl("unitizer\\.fastlm$", pkg.dir)
  ) unlink(pkg.dir, recursive=TRUE)
  rm(list=vars, envir=parent.frame())
}
