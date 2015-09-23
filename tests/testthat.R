library(testthat)

old.ctype <- Sys.getlocale("LC_CTYPE")
old.collate <- Sys.getlocale("LC_COLLATE")
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
Sys.setlocale("LC_COLLATE", "en_US.UTF-8")

run_tests <- function() {
  on.exit({
    Sys.setlocale("LC_CTYPE", old.ctype)
    Sys.setlocale("LC_COLLATE", old.collate)
  } )
  test_dir("testthat")
}
run_tests()
