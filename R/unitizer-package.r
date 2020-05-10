# Copyright (C) 2020  Brodie Gaslam
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

#' unitizer
#'
#' Simplifies regression tests by comparing objects produced by test
#' code with earlier versions of those same objects.  If objects are unchanged
#' the tests pass.  `unitizer` provides an interactive interface to review
#' failing tests or new tests.  See vignettes for details.
#'
#' @import methods
#' @import stats
#' @import diffobj
#' @importFrom utils capture.output file_test getParseData installed.packages
#'   loadhistory modifyList object.size packageVersion remove.packages
#'   savehistory
#' @name unitizer
#' @docType package

NULL
