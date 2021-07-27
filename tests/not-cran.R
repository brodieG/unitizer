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

