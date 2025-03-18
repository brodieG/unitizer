source(file.path("_helper", "init.R"))
# - "int.pos" ------------------------------------------------------------------

unitizer:::is.int.pos.1L(c(1, 2, 3))  # FALSE
unitizer:::is.int.pos.1L(1)
unitizer:::is.int.pos.1L(1)
unitizer:::is.int.pos.1L(-1)           # FALSE
unitizer:::is.int.pos.1L(NA_integer_)  # FALSE
unitizer:::is.int.pos.2L(1:2)
unitizer:::is.int.pos.2L(c(1, 2))

# - "is.valid_two_arg" ---------------------------------------------------------

f1 <- function(x, y) NULL
f2 <- function(...) NULL
f3 <- function(x, ...) NULL
f4 <- function(x, y, z) NULL
f5 <- function(x, y, z = 3) NULL
f6 <- function(x) NULL
unitizer:::is.two_arg_fun(f1)
unitizer:::is.two_arg_fun(f2)
unitizer:::is.two_arg_fun(f3)
unitizer:::is.two_arg_fun(f4)
unitizer:::is.two_arg_fun(f5)
unitizer:::is.two_arg_fun(f6)
unitizer:::is.two_arg_fun(1)

# - "is.valid_capt_setting" ----------------------------------------------------

capt.test <- unitizer:::is.valid_capt_setting(c(T, T))
capt.test
