source(file.path("_helper", "init.R"))

# - "read_line works" ----------------------------------------------------------

# read through prompt vals

unitizer:::read_line_set_vals(letters[1:3])
u.ns <- asNamespace("unitizer")
unitizer:::read_line()
identical(u.ns$.global$prompt.vals, letters[2:3])
unitizer:::read_line()
u.ns$.global$prompt.vals
unitizer:::read_line()
u.ns$.global$prompt.vals

try(unitizer:::read_line())

# - "simple prompts" -----------------------------------------------------------

unitizer:::read_line_set_vals(c("y", "Y", "n", "N"))
# expect_error(unitizer:::simple_prompt(1:5))
try(unitizer:::simple_prompt(1:5))
# expect_error(unitizer:::simple_prompt("hello", attempts = 1:5))
try(unitizer:::simple_prompt("hello", attempts = 1:5))
# expect_error(unitizer:::simple_prompt("hello", values = NA_character_))
try(unitizer:::simple_prompt("hello", values = NA_character_))
# expect_error(unitizer:::simple_prompt("hello", case.sensitive = 1))
unitizer:::simple_prompt("hello", case.sensitive = 1)

capture.output(expect_identical("Y", unitizer:::simple_prompt("hello")))
capture.output(expect_identical("Y", unitizer:::simple_prompt("hello")))
capture.output(expect_identical("N", unitizer:::simple_prompt("hello")))
capture.output(expect_identical("N", unitizer:::simple_prompt("hello")))
unitizer:::read_line_set_vals(c("y", "y", "n"))
# expect_error(capture.output(unitizer:::simple_prompt("hello", 
#     attempts = 1L, case.sensitive = TRUE)), "Gave up trying to collect")
capture.output(unitizer:::simple_prompt("hello", attempts = 1L, 
    case.sensitive = TRUE))
# expect_output(try(unitizer:::simple_prompt("hello", attempts = 1L, 
#     case.sensitive = TRUE), silent = TRUE), "hello\n.*Invalid input, please select one of: Y, N")
try(unitizer:::simple_prompt("hello", attempts = 1L, case.sensitive = TRUE), 
    silent = TRUE)
# expect_error(capture.output(unitizer:::simple_prompt("hello", 
#     attempts = 1L, case.sensitive = TRUE)), "Gave up trying to collect")
capture.output(unitizer:::simple_prompt("hello", attempts = 1L, 
    case.sensitive = TRUE))
# test_that("faux prompt", {
#     unitizer:::read_line_set_vals(c("1 +", "1"))
#     expect_output(res <- unitizer:::faux_prompt(prompt = "> ", 
#         continue = "+ ")[[1L]], "> 1 \\+\n\\+ 1")
#     expect_identical(res, quote(1 + 1))
#     unitizer:::read_line_set_vals(c("(})"))
#     expect_error(capture.output(unitizer:::faux_prompt(prompt = "> ", 
#         continue = "+ ")), "unexpected '\\}'")
# })
# - "faux prompt" --------------------------------------------------------------


unitizer:::read_line_set_vals(c("1 +", "1"))
# expect_output(res <- unitizer:::faux_prompt(prompt = "> ", continue = "+ ")[[1L]], 
#     "> 1 \\+\n\\+ 1")
res <- unitizer:::faux_prompt(prompt = "> ", continue = "+ ")[[1L]]
# expect_identical(res, quote(1 + 1))
res
unitizer:::read_line_set_vals(c("(})"))
# expect_error(capture.output(unitizer:::faux_prompt(prompt = "> ", 
#     continue = "+ ")), "unexpected '\\}'")
capture.output(unitizer:::faux_prompt(prompt = "> ", continue = "+ "))
# Some of this needs to be done outside of testthat due to sinking
suppressWarnings(glob <- unitizer:::unitizerGlobal$new())
unitizer:::read_line_set_vals(c("1 +", "1", "H", "Y"))
txt <- capture.output(res <- unitizer:::unitizer_prompt("hello", 
    valid.opts = c(Y = "[Y]es", N = "[N]o"), global = glob))
unitizer:::read_line_set_vals(c("1 +", "1", "H", "Q"))
txt2 <- capture.output(res2 <- unitizer:::unitizer_prompt("hello", 
    valid.opts = c(Y = "[Y]es", N = "[N]o"), help = "This is all the help you get", 
    global = glob))
unitizer:::read_line_set_vals(c("hell())", "Q"))
txt3 <- unitizer:::capture_output(unitizer:::unitizer_prompt("hello", 
    valid.opts = c(Y = "[Y]es", N = "[N]o"), global = glob))
# and multiline stuff (#242)
unitizer:::read_line_set_vals(c("{\n  1 + 1\n  2 + 1\n}", "N"))
txt4 <- capture.output(res <- unitizer:::unitizer_prompt("hello", 
    valid.opts = c(Y = "[Y]es", N = "[N]o"), global = glob))
# test_that("unitizer prompt", {
#     expect_equal(txt, c("unitizer> 1 +", "+ 1", "[1] 2", "unitizer> H", 
#         "", "| No help available.", "| ", "| hello ([Y]es, [N]o, [Q]uit, [H]elp)?", 
#         "", "unitizer> Y", ""))
#     expect_equal(txt2, c("unitizer> 1 +", "+ 1", "[1] 2", "unitizer> H", 
#         "", "| This is all the help you get", "| ", "| hello ([Y]es, [N]o, [Q]uit, [H]elp)?", 
#         "", "unitizer> Q", ""))
#     expect_equal(txt3$message, c("Error in \"hell())\": <text>:1:7: unexpected ')'", 
#         "1: hell())", "          ^"))
#     expect_equal(txt4, c("unitizer> {", "  1 + 1", "  2 + 1", 
#         "}", "[1] 3", "unitizer> N", ""))
#     expect_error(unitizer:::unitizer_prompt("hello", valid.opts = c(Y = "[Y]es", 
#         N = "[N]o"), browse.env = "not an env", global = glob), 
#         "Argument `browse.env` must be an environment")
#     expect_identical(res2, "Q")
#     unitizer:::read_line_set_vals(character())
#     expect_error(unitizer:::unitizer_prompt("hello", valid.opts = c(Y = "[Y]es", 
#         N = "[N]o"), global = glob), "Internal Error: ran out of predefined readline input", 
#         class = "readError")
#     unitizer:::read_line_set_vals("1L")
#     expect_error(capture.output(unitizer:::unitizer_prompt("hello", 
#         valid.opts = c(Y = "[Y]es", N = "[N]o"), exit.condition = unitizer:::exit_fun, 
#         valid.vals = 2:3, global = glob)), "Internal Error: ran out of predefined readline input")
#     unitizer:::read_line_set_vals("2L")
#     capture.output(res <- unitizer:::unitizer_prompt("hello", 
#         valid.opts = c(Y = "[Y]es", N = "[N]o"), exit.condition = unitizer:::exit_fun, 
#         valid.vals = 2:3, global = glob))
#     expect_equal(res, 2L)
# })
# - "unitizer prompt" ----------------------------------------------------------


# expect_equal(txt, c("unitizer> 1 +", "+ 1", "[1] 2", "unitizer> H", 
#     "", "| No help available.", "| ", "| hello ([Y]es, [N]o, [Q]uit, [H]elp)?", 
#     "", "unitizer> Y", ""))
txt
# expect_equal(txt2, c("unitizer> 1 +", "+ 1", "[1] 2", "unitizer> H", 
#     "", "| This is all the help you get", "| ", "| hello ([Y]es, [N]o, [Q]uit, [H]elp)?", 
#     "", "unitizer> Q", ""))
txt2
# expect_equal(txt3$message, c("Error in \"hell())\": <text>:1:7: unexpected ')'", 
#     "1: hell())", "          ^"))
txt3$message
# expect_equal(txt4, c("unitizer> {", "  1 + 1", "  2 + 1", "}", 
#     "[1] 3", "unitizer> N", ""))
txt4
# expect_error(unitizer:::unitizer_prompt("hello", valid.opts = c(Y = "[Y]es", 
#     N = "[N]o"), browse.env = "not an env", global = glob), "Argument `browse.env` must be an environment")
unitizer:::unitizer_prompt("hello", valid.opts = c(Y = "[Y]es", 
    N = "[N]o"), browse.env = "not an env", global = glob)
# expect_identical(res2, "Q")
res2
unitizer:::read_line_set_vals(character())
# expect_error(unitizer:::unitizer_prompt("hello", valid.opts = c(Y = "[Y]es", 
#     N = "[N]o"), global = glob), "Internal Error: ran out of predefined readline input", 
#     class = "readError")
unitizer:::unitizer_prompt("hello", valid.opts = c(Y = "[Y]es", 
    N = "[N]o"), global = glob)
unitizer:::read_line_set_vals("1L")
# expect_error(capture.output(unitizer:::unitizer_prompt("hello", 
#     valid.opts = c(Y = "[Y]es", N = "[N]o"), exit.condition = unitizer:::exit_fun, 
#     valid.vals = 2:3, global = glob)), "Internal Error: ran out of predefined readline input")
capture.output(unitizer:::unitizer_prompt("hello", valid.opts = c(Y = "[Y]es", 
    N = "[N]o"), exit.condition = unitizer:::exit_fun, valid.vals = 2:3, 
    global = glob))
unitizer:::read_line_set_vals("2L")
capture.output(res <- unitizer:::unitizer_prompt("hello", valid.opts = c(Y = "[Y]es", 
    N = "[N]o"), exit.condition = unitizer:::exit_fun, valid.vals = 2:3, 
    global = glob))
# expect_equal(res, 2L)
res
