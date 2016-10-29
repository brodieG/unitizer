# Simulate a full install and upgrade cycle

library("unitizer")
if(interactive()) {
  if("testpkg1" %in% rownames(installed.packages()))
    stop("`testpkg1` already installed")
  if(!identical(basename(getwd()), "tests")) stop("Should be in /tests")
  if(!file_test("-d", file.path(getwd(), "test_pkgs", "testpkg1")))
    stop("`testpkg1` dir not found")

  local({
    par.dir <- tempfile()
    on.exit({
      setwd(old.dir)
      if("testpkg1" %in% search()) try(detach("package:testpkg1"))
      try(unloadNamespace("testpkg1"))
      try(remove.packages("testpkg1"))
      unlink(par.dir, recursive=TRUE)
    })
    dir.create(par.dir)

    # old.dir should be package directory with test files

    old.dir <- setwd(par.dir)
    lib.tmp <- file.path(par.dir, "lib")
    test.tmp <- file.path(par.dir, "tests")
    pre.file <- file.path(test.tmp, "unitizer", "_pre")
    source.tests <-
      file.path(old.dir, "test_pkgs", "testpkg1", "testpkg1.0", "tests")

    # copy files to new directory

    dir.create(lib.tmp)
    dir.create(test.tmp)
    file.copy(
      list.files(source.tests, full.names=TRUE), test.tmp, recursive=TRUE
    )
    dir.create(pre.file, recursive=TRUE)
    cat("library(testpkg1)\n", file=file.path(pre.file, "lib.R"))
    tp.0 <- file.path(old.dir, "test_pkgs", "testpkg1", "testpkg1.0")
    devtools::install(tp.0)
    unitize_dir(file.path(test.tmp, "unitizer"), auto.accept="new")

    # Upgrade the package, note we test against same store

    if("testpkg1" %in% search()) try(detach("package:testpkg1"))
    try(unloadNamespace("testpkg1"))
    try(remove.packages("testpkg1"))
    tp.1 <- file.path(old.dir, "test_pkgs", "testpkg1", "testpkg1.1")
    devtools::install(tp.1)
    unitize_dir(file.path(test.tmp, "unitizer"))
  })
}
