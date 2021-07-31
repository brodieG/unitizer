if(!nzchar(Sys.getenv('NOT_CRAN'))) q()

source(file.path("_helper", "init.R"))

# - "test file / store manip" ------------------------------------------------

unitizer:::as.store_id_chr(file.path(getwd(), "hello"))
try(unitizer:::as.store_id_chr(structure("hello", class = "untz_stochrerr")))
as.character.custstore <- function(x, ...) x
unitizer:::best_store_name(
  structure(list("hello", class = "custstore")), "hello"
)
unitizer:::best_store_name(
  structure(list("hello", class = "custstore")), NA_character_
)
unitizer:::best_file_name(
  structure(list("hello", class = "custstore")), NA_character_
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
  if (!identical(try(system("whoami", intern = TRUE), silent = TRUE),
      "root")) {
      try(capture.output(set_unitizer(ro.dir, toy.stor), type = "message"))
  }
}
