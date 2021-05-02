source(file.path("_helper", "init.R"))

# - "Rename Works" -------------------------------------------------------------

x <- readRDS("_helper/trivial.unitizer/data.rds")
x.edit <- editCalls(x, quote(x), quote(y), interactive.only = FALSE)
x.edit@items.ref.calls.deparse
!identical(x@items.ref.calls.deparse, x.edit@items.ref.calls.deparse)
identical(
  x.edit@items.ref.calls.deparse,
  gsub("\\bx\\b", "y", x@items.ref.calls.deparse)
)

unitizer:::read_line_set_vals("Y")  # warn
x.edit2 <- editCalls(x, quote(x), quote(y), interactive.only = TRUE)
unitizer:::read_line_set_vals("N")  # message
x.edit3 <- editCalls(x, quote(x), quote(y), interactive.only = TRUE)
identical(x.edit3, x)

unitizer:::read_line_set_vals(NULL)
x.edit@items.ref.calls.deparse
