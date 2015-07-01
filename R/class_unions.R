setClassUnion("listOrExpression", c("list", "expression"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("environmentOrNULL", c("environment", "NULL"))
setClassUnion("languageOrNULL", c("language", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("subIndex", c("character", "logical", "numeric", "missing"))
# setOldClass("file")
# setOldClass(c('package_version', 'numeric_version'))
# setClassUnion("fileOrNULL", c("file", "NULL"))  # removed due to conflicts with RJSONIO
