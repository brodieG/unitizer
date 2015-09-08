library(unitizer)

test_that("read_line works", {
  unitizer:::read_line_set_vals(letters[1:3])
  u.ns <- asNamespace("unitizer")
  expect_identical(unitizer:::read_line(), "a")
  expect_identical(u.ns$.global$prompt.vals, letters[2:3])
  expect_identical(unitizer:::read_line(), "b")
  expect_identical(u.ns$.global$prompt.vals, letters[3])
  expect_identical(unitizer:::read_line(), "c")
  expect_identical(u.ns$.global$prompt.vals, character())
  expect_error(unitizer:::read_line(), "ran out of predefined readline input")
})

test_that("simple prompts", {
  unitizer:::read_line_set_vals(c("y", "Y", "n", "N"))
  expect_error(unitizer:::simple_prompt(1:5))
  expect_error(unitizer:::simple_prompt("hello", attempts=1:5))
  expect_error(unitizer:::simple_prompt("hello", values=NA_character_))
  expect_error(unitizer:::simple_prompt("hello", case.sensitive=1))
  expect_identical("Y", unitizer:::simple_prompt("hello"))
  expect_identical("Y", unitizer:::simple_prompt("hello"))
  expect_identical("N", unitizer:::simple_prompt("hello"))
  expect_identical("N", unitizer:::simple_prompt("hello"))
  unitizer:::read_line_set_vals(c("y", "y", "n"))
  expect_error(
    unitizer:::simple_prompt("hello", attempts=1L, case.sensitive=TRUE),
    "Gave up trying to collect"
  )
  expect_output(
    try(unitizer:::simple_prompt("hello", attempts=1L, case.sensitive=TRUE)),
    "hello\n.*Invalid input, please select one of: Y, N"
  )
  expect_error(
    unitizer:::simple_prompt("hello", attempts=1L, case.sensitive=TRUE),
    "Gave up trying to collect"
  )
})

test_that("faux prompt", {
  unitizer:::read_line_set_vals(c("1 +", "1"))
  expect_output(
    res <- unitizer:::faux_prompt(prompt="> ", continue="+ ")[[1L]],
    "> 1 \\+\n\\+ 1"
  )
  expect_identical(res, quote(1 + 1))
  unitizer:::read_line_set_vals(c("(})"))
  expect_error(
    unitizer:::faux_prompt(prompt="> ", continue="+ "),
    "Error in \"\\(\\}\\)\""
  )
})
# Some of this needs to be done outside of testthat due to sinking

unitizer:::read_line_set_vals(c("1 +", "1", "H", "Y"))
txt <- capture.output(
  res <- unitizer:::unitizer_prompt(
      "hello", valid.opts=c(Y="[Y]es", N="[N]o")
  )
)
unitizer:::read_line_set_vals(c("1 +", "1", "H", "Q"))
txt2 <- capture.output(
  res2 <- unitizer:::unitizer_prompt(
    "hello", valid.opts=c(Y="[Y]es", N="[N]o"),
    help="This is all the help you get"
  )
)
test_that("unitizer prompt", {
  expect_equal(
    txt,
    c("unitizer> 1 +", "+ 1", "[1] 2", "unitizer> H", "No help available.", "", "hello ([Y]es, [N]o, [Q]uit, [H]elp)?", "unitizer> Y")
  )
  expect_equal(
    txt2,
    c("unitizer> 1 +", "+ 1", "[1] 2", "unitizer> H", "This is all the help you get", "", "hello ([Y]es, [N]o, [Q]uit, [H]elp)?", "unitizer> Q")
  )
  expect_error(
    unitizer:::unitizer_prompt(
      "hello", valid.opts=c(Y="[Y]es", N="[N]o"), browse.env="not an env"
    ),
    "Argument `browse.env` must be an environment"
  )
  expect_identical(res2, "Q")
  unitizer:::read_line_set_vals(character())
  expect_error(
    unitizer:::unitizer_prompt("hello", valid.opts=c(Y="[Y]es", N="[N]o")),
    "Error : Logic Error: ran out of predefined readline input"
  )
  unitizer:::read_line_set_vals("1L")
  expect_error(
    unitizer:::unitizer_prompt(
      "hello", valid.opts=c(Y="[Y]es", N="[N]o"),
      exit.condition=unitizer:::exit_fun, valid.vals=2:3
    ),
    "Error : Logic Error: ran out of predefined readline input"
  )
  unitizer:::read_line_set_vals("2L")
  expect_equal(
    unitizer:::unitizer_prompt(
      "hello", valid.opts=c(Y="[Y]es", N="[N]o"),
      exit.condition=unitizer:::exit_fun, valid.vals=2:3
    ),
    2L
  )
})

unitizer:::read_line_set_vals(NULL)  # Clear out for normal use
