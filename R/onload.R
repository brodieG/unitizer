#' @include options.R
#' @include global.R

NULL

.loaded <- FALSE
.onLoad <- function(libname, pkgname) {
  # nocov start
  options(
    .unitizer.opts.default[
      setdiff(names(.unitizer.opts.default), names(options()))
    ]
  )
  .loaded <<- TRUE
  # nocov end
}
.onUnload <- function(libpath) {
}
.onAttach <- function(libname, pkgname) {
  if(is.null(getOption('unitizer.state'))) {
    packageStartupMessage(
      "State tracking is disabled by default to comply with CRAN policies. ",
      "Add `options(unitizer.state='recommended')` to your 'Rprofile' file ",
      "to enable, or `options(unitizer.state='off')` to quash this message ",
      "without enabling.  Prior to enabling, be sure to read `?unitizerState`,",
      "in particular the 'CRAN non-compliance' section."
    )
  }
}
