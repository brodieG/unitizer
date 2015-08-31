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
    "hello\nInvalid input, please select one of: Y, N"
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
})
# Some of this needs to be done outside of testthat due to sinking

unitizer:::read_line_set_vals(c("1 +", "1", "H", "Y"))
txt <- capture.output(split=TRUE,
  res <- unitizer:::unitizer_prompt(
      "hello", valid.opts=c(Y="[Y]es", N="[N]o")
  )
)
test_that("unitizer prompt", {
  expect_equal(
    txt,
    c("unitizer> 1 +", "+ 1", "[1] 2", "unitizer> H", "No help available.", "", "hello ([Y]es, [N]o, [Q]uit, [H]elp)?", "unitizer> Y")
  )
})

unitizer:::read_line_set_vals(NULL)  # Clear out for normal use
