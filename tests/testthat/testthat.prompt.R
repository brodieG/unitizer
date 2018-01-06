library(unitizer)
context("Prompt")

local({
  on.exit(
    unitizer:::read_line_set_vals(NULL)  # Clear out for normal use
  )
  test_that("read_line works", {
    unitizer:::read_line_set_vals(letters[1:3])
    u.ns <- asNamespace("unitizer")
    capture.output(expect_identical(unitizer:::read_line(), "a"))
    capture.output(expect_identical(u.ns$.global$prompt.vals, letters[2:3]))
    capture.output(expect_identical(unitizer:::read_line(), "b"))
    capture.output(expect_identical(u.ns$.global$prompt.vals, letters[3]))
    capture.output(expect_identical(unitizer:::read_line(), "c"))
    capture.output(expect_identical(u.ns$.global$prompt.vals, character()))
    expect_error(unitizer:::read_line(), "ran out of predefined readline input")
  })

  test_that("simple prompts", {
    unitizer:::read_line_set_vals(c("y", "Y", "n", "N"))
    expect_error(unitizer:::simple_prompt(1:5))
    expect_error(unitizer:::simple_prompt("hello", attempts=1:5))
    expect_error(unitizer:::simple_prompt("hello", values=NA_character_))
    expect_error(unitizer:::simple_prompt("hello", case.sensitive=1))
    capture.output(expect_identical("Y", unitizer:::simple_prompt("hello")))
    capture.output(expect_identical("Y", unitizer:::simple_prompt("hello")))
    capture.output(expect_identical("N", unitizer:::simple_prompt("hello")))
    capture.output(expect_identical("N", unitizer:::simple_prompt("hello")))
    unitizer:::read_line_set_vals(c("y", "y", "n"))
    expect_error(
      capture.output(
        unitizer:::simple_prompt("hello", attempts=1L, case.sensitive=TRUE)
      ),
      "Gave up trying to collect"
    )
    expect_output(
      try(
        unitizer:::simple_prompt("hello", attempts=1L, case.sensitive=TRUE),
        silent=TRUE
      ),
      "hello\n.*Invalid input, please select one of: Y, N"
    )
    expect_error(
      capture.output(
        unitizer:::simple_prompt("hello", attempts=1L, case.sensitive=TRUE)
      ),
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
      capture.output(
        unitizer:::faux_prompt(prompt="> ", continue="+ ")
      ),
      "unexpected '\\}'"
    )
  })
  # Some of this needs to be done outside of testthat due to sinking

  suppressWarnings(glob <- unitizer:::unitizerGlobal$new())
  unitizer:::read_line_set_vals(c("1 +", "1", "H", "Y"))
  txt <- capture.output(
    res <- unitizer:::unitizer_prompt(
      "hello", valid.opts=c(Y="[Y]es", N="[N]o"), global=glob
    )
  )
  unitizer:::read_line_set_vals(c("1 +", "1", "H", "Q"))
  txt2 <- capture.output(
    res2 <- unitizer:::unitizer_prompt(
      "hello", valid.opts=c(Y="[Y]es", N="[N]o"),
      help="This is all the help you get", global=glob
    )
  )
  unitizer:::read_line_set_vals(c("hell())", "Q"))
  txt3 <- unitizer:::capture_output(
    unitizer:::unitizer_prompt(
      "hello", valid.opts=c(Y="[Y]es", N="[N]o"), global=glob
    )
  )
  # and multiline stuff (#242)

  unitizer:::read_line_set_vals(c("{\n  1 + 1\n  2 + 1\n}", 'N'))
  txt4 <- capture.output(
    res <- unitizer:::unitizer_prompt(
      "hello", valid.opts=c(Y="[Y]es", N="[N]o"), global=glob
    )
  )
  test_that("unitizer prompt", {
    expect_equal(
      txt,
      c("unitizer> 1 +", "+ 1", "[1] 2", "unitizer> H", "", "| No help available.", "| ", "| hello ([Y]es, [N]o, [Q]uit, [H]elp)?", "", "unitizer> Y", "")

    )
    expect_equal(
      txt2,
      c("unitizer> 1 +", "+ 1", "[1] 2", "unitizer> H", "", "| This is all the help you get", "| ", "| hello ([Y]es, [N]o, [Q]uit, [H]elp)?", "", "unitizer> Q", "")
    )
    expect_equal(
      txt3$message,
      c("Error in \"hell())\": <text>:1:7: unexpected ')'", "1: hell())", "          ^")
    )
    expect_equal(
      txt4,
      c("unitizer> {", "  1 + 1", "  2 + 1", "}", "[1] 3", "unitizer> N", "")
    )
    expect_error(
      unitizer:::unitizer_prompt(
        "hello", valid.opts=c(Y="[Y]es", N="[N]o"), browse.env="not an env",
        global=glob
      ),
      "Argument `browse.env` must be an environment"
    )
    expect_identical(res2, "Q")
    unitizer:::read_line_set_vals(character())
    expect_error(
      unitizer:::unitizer_prompt(
        "hello", valid.opts=c(Y="[Y]es", N="[N]o"), global=glob
      ),
      "Internal Error: ran out of predefined readline input"
    )
    unitizer:::read_line_set_vals("1L")
    expect_error(
      capture.output(
        unitizer:::unitizer_prompt(
          "hello", valid.opts=c(Y="[Y]es", N="[N]o"),
          exit.condition=unitizer:::exit_fun, valid.vals=2:3, global=glob
      ) ),
      "Internal Error: ran out of predefined readline input"
    )
    unitizer:::read_line_set_vals("2L")
    capture.output(
      res <- unitizer:::unitizer_prompt(
        "hello", valid.opts=c(Y="[Y]es", N="[N]o"),
        exit.condition=unitizer:::exit_fun, valid.vals=2:3, global=glob
    ) )
    expect_equal(res, 2L)
  })
})
