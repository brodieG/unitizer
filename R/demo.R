#' Functions To Assist With Demo
#'
#' \code{`fastlm_dir`} returns the directory that the \code{`unitizer.fastlm`}
#' package is in, and \code{`prompt_to_proceed`} stops demo evaluation until
#' user presses ENTER.
#'
#' @note these functions are not for use outside of the unitizer demo
#'
#' @aliases prompt_to_proceed
#' @param version a number in 0:2
#' @return character(1L)
#' @export

fastlm_dir <- function(version) {
  unitizer.dir <- system.file(package="unitizer")
  paste0(unitizer.dir, "/example.pkgs/fastlm.", version)
}

#' @export

prompt_to_proceed <- function()
  invisible(readline("[Press ENTER Continue]"))  # helper fun
