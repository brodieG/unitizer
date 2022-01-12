# Copyright (C) 2022 Brodie Gaslam
# 
# This file is part of "unitizer"
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

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
      "Add `options(unitizer.state='suggested')` to your 'Rprofile' file ",
      "to enable, or `options(unitizer.state='off')` to quash this message ",
      "without enabling.  Prior to enabling, be sure to read `?unitizerState`,",
      "in particular the 'CRAN non-compliance' section."
    )
  }
}
