#' @include options.R
#' @include global.R

NULL

.loaded <- FALSE
.onLoad <- function(libname, pkgname) {
  options(
    .unitizer.opts.default[
      setdiff(names(.unitizer.opts.default), names(options()))
    ]
  )
  .loaded <<- TRUE
}
.onUnload <- function(libpath) {
}
.onAttach <- function(libname, pkgname) {
  if(is.null(getOption('unitizer.state'))) {
    packageStartupMessage(
      "State tracking is disabled by default to comply with CRAN policies; ",
      "For more reproducible tests we recommend you enable state tracking."
    )
  }
}
