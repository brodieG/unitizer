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

setClassUnion("listOrExpression", c("list", "expression"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("environmentOrNULL", c("environment", "NULL"))
setClassUnion("languageOrNULL", c("language", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("integerOrNULL", c("integer", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("subIndex", c("character", "logical", "numeric", "missing"))
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))
setClassUnion("DiffOrNULL", c("Diff", "NULL"))

# setOldClass("file")
# setOldClass(c('package_version', 'numeric_version'))
# setClassUnion("fileOrNULL", c("file", "NULL"))  # removed due to conflicts with RJSONIO

#' Documentation Block for Internal S4 Methods
#'
#' R insists these need to be documented as user facing, but they are not really
#' so were throwing them all in here.  Actual docs are in non roxygen comments
#' by fun definitions.
#'
#' Put in this file because this file is included by almost every other file
#'
#' @name unitizer_s4method_doc
#' @rdname unitizer_s4method_doc
#' @keywords internal

NULL
