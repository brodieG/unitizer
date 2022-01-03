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
try(unitizer:::simple_prompt(1:5))
try(unitizer:::simple_prompt("hello", attempts = 1:5))
try(unitizer:::simple_prompt("hello", values = NA_character_))
try(unitizer:::simple_prompt("hello", case.sensitive = 1))

unitizer:::simple_prompt("hello")
unitizer:::simple_prompt("hello")#
unitizer:::simple_prompt("hello")
unitizer:::read_line_set_vals(c("y", "y", "n"))
try(unitizer:::simple_prompt("hello", attempts = 1L, case.sensitive = TRUE))
try(unitizer:::simple_prompt("hello", attempts = 1L, case.sensitive = TRUE),
    silent = TRUE)
try(unitizer:::simple_prompt("hello", attempts = 1L, case.sensitive = TRUE))

# - "faux prompt" --------------------------------------------------------------

unitizer:::read_line_set_vals(c("1 +", "1"))
unitizer:::faux_prompt(prompt = "> ", continue = "+ ")[[1L]]
unitizer:::read_line_set_vals(c("(})"))
try(unitizer:::faux_prompt(prompt = "> ", continue = "+ "))

## Test the new readLines based read_line
## This test will not work in interactive mode, requiring input
unitizer:::read_line_set_vals(c("1 +", "1"))
unitizer:::faux_prompt()

## This one embeds a CTRL+C to test interrupt, but we can't test this without
## read_line_setvals
unitizer:::read_line_set_vals(c("1 +", "\x03", "2 + ", "1"))
unitizer:::faux_prompt()

unitizer:::read_line_set_vals(c("\x03", "2 + ", "1"))
unitizer:::faux_prompt()

## Test that changing language doesn't affect partial parsing
lang <- Sys.getenv("LANGUAGE", unset=NA)
Sys.setenv("LANGUAGE"="fr")
unitizer:::read_line_set_vals(c("1 +", "1"))
unitizer:::faux_prompt(prompt = "> ", continue = "+ ")
if(is.na(lang)) Sys.unsetenv("LANGUAGE") else Sys.setenv("LANGUAGE"=lang)

# - "unitizer prompt" ----------------------------------------------------------

# Some of this needs to be done outside of testthat due to sinking
suppressWarnings(glob <- unitizer:::unitizerGlobal$new())
unitizer:::read_line_set_vals(c("1 +", "1", "H", "Y"))
unitizer:::unitizer_prompt(
  "hello", valid.opts = c(Y = "[Y]es", N = "[N]o"), global = glob
)

unitizer:::read_line_set_vals(c("1 +", "1", "H", "Q"))
unitizer:::unitizer_prompt("hello",
    valid.opts = c(Y = "[Y]es", N = "[N]o"), help = "This is all the help you get",
    global = glob)

unitizer:::read_line_set_vals(c("hell())", "Q"))
txt3 <- unitizer:::capture_output(unitizer:::unitizer_prompt("hello",
    valid.opts = c(Y = "[Y]es", N = "[N]o"), global = glob))
txt3$message

# and multiline stuff (#242)
unitizer:::read_line_set_vals(c("{\n  1 + 1\n  2 + 1\n}", "N"))
unitizer:::unitizer_prompt(
  "hello", valid.opts = c(Y = "[Y]es", N = "[N]o"), global = glob
)

try(
  unitizer:::unitizer_prompt(
    "hello", valid.opts = c(Y = "[Y]es", N = "[N]o"),
    browse.env = "not an env", global = glob
) )

unitizer:::read_line_set_vals(character())
try(
  unitizer:::unitizer_prompt(
    "hello", valid.opts = c(Y = "[Y]es", N = "[N]o"), global = glob
) )
unitizer:::read_line_set_vals("1L")
try(
  unitizer:::unitizer_prompt(
    "hello",
    valid.opts = c(Y = "[Y]es", N = "[N]o"),
    exit.condition = unitizer:::exit_fun,
    valid.vals = 2:3, global = glob
) )
unitizer:::read_line_set_vals("2L")
unitizer:::unitizer_prompt("hello", valid.opts = c(Y = "[Y]es",
    N = "[N]o"), exit.condition = unitizer:::exit_fun, valid.vals = 2:3,
    global = glob)

