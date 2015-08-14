#' @include options.R

NULL

.onLoad <- function(libname, pkgname) {
  if(is.null(getOption("unitizer.state")))
    packageStartupMessage(
      "`unitizer` state tracking defaults to \"",
      .unitizer.opts.default[["unitizer.state"]], "\", use ",
      "`option(unitizer.state=...)` to override; see `?unitizerState` ",
      "for more details."
    )
  options(
    .unitizer.opts.default[
      setdiff(names(.unitizer.opts.default), names(options()))
    ]
  )
}

.onUnload <- function(libpath) {
}

