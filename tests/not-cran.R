if(!nzchar(Sys.getenv('NOT_CRAN'))) q()

source(file.path("_helper", "init.R"))

# - "test file / store manip" ------------------------------------------------

as.character.custstore <- function(x, ...) x
stopifnot(
  all.equal(unitizer:::as.store_id_chr(file.path(getwd(), "hello")), "hello"),
  grepl(
    "Unable to convert store id to character",
    try(unitizer:::as.store_id_chr(structure("hello", class = "untz_stochrerr"))),
  ),
  all.equal(
    unitizer:::best_store_name(
      structure(list("hello", class = "custstore")), "hello"
    ),
    "unitizer for test file 'hello'"
  ),
  all.equal(
    unitizer:::best_store_name(
      structure(list("hello", class = "custstore")), NA_character_
    ),
    "<untranslateable-unitizer-id>"
  ),
  all.equal(
    unitizer:::best_file_name(
      structure(list("hello", class = "custstore")), NA_character_
    ),
    "<unknown-test-file>"
  )
)

# - read only ------------------------------------------------------------------

# try to write to read only
# seems to be case that root can always write so defeats this test
if (identical(.Platform$OS.type, "unix")) {
  toy.path <- file.path("_helper", "unitizers", "misc.unitizer")
  toy.stor <- readRDS(file.path(toy.path, "data.rds"))

  ro.dir <- tempfile()
  on.exit(unlink(ro.dir))
  dir.create(ro.dir, mode = "0500")
  if (!identical(try(system("whoami", intern = TRUE), silent = TRUE), "root")) {
    err <- try(
      capture.output(set_unitizer(ro.dir, toy.stor), type = "message"),
    )
    stopifnot(
      grepl("Failed setting unitizer", conditionMessage(attr(err, 'condition')))
    )
  }
}
# - "as.state" -----------------------------------------------------------------

# This fails on winbuilder machines? Not entirely clear why it would given that
# the only obvious difference in the dir structure is that the test dirs are
# tests_x64, etc., instead of just tests, but the code doesn't care about that.
# A bit of a red herring is that the winbuilder artifact dir is not actually
# the directory the tests are run in (we know because we ran a pwd() in one of
# our tests).

in.pkg.state <- unitizer:::as.state(
  unitizer:::unitizerStateRaw(par.env = in_pkg()), test.files = getwd()
)
stopifnot(
  identical(
    in.pkg.state,
    unitizer:::unitizerStateProcessed(par.env = getNamespace("unitizer"))
  )
)
