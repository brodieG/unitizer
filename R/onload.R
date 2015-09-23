#' @include options.R

NULL

.onLoad <- function(libname, pkgname) {
  options(
    .unitizer.opts.default[
      setdiff(names(.unitizer.opts.default), names(options()))
    ]
  )
}
.onUnload <- function(libpath) {
}

