setOldClass("expression")
setClassUnion("listOrExpression", c("list", "expression"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("environmentOrNULL", c("environment", "NULL"))
setClassUnion("subIndex", c("character", "logical", "numeric", "missing"))
setOldClass("package_version")

#' @exportClass file

setOldClass("file")

#' @exportClass fileOrNULL

setClassUnion("fileOrNULL", c("file", "NULL"))