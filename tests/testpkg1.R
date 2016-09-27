# Simulate a full install and upgrade cycle

library("unitizer")
if(interactive()) {
  if("testpkg1" %in% loadedNamespaces()) stop("`testpkg1` already loaded")
  if(!identical(basename(getwd()), "tests")) stop("Should be in /tests")
  if(!file_test("-d", file.path(getwd(), "test_pkgs", "testpkg1")))
    stop("`testpkg1` dir not found")

  local({
    par.dir <- tempfile()
    on.exit({
      setwd(old.dir)
      try(detach("package:testpkg1", unload=TRUE), silent=TRUE)
      try(remove.packages("testpkg1", lib=lib.tmp))
      unlink(par.dir, recursive=TRUE)
    })
    dir.create(par.dir)
    old.dir <- setwd(par.dir)
    lib.tmp <- file.path(par.dir, "lib")
    test.tmp <- file.path(par.dir, "tests")
    pre.file <- file.path(test.tmp, "unitizer", "_pre")
    source.tests <-
      file.path(old.dir, "test_pkgs", "testpkg1", "testpkg1.0", "tests")
    dir.create(lib.tmp)
    dir.create(test.tmp)
    file.copy(
      list.files(source.tests, full.names=TRUE), test.tmp, recursive=TRUE
    )
    dir.create(pre.file, recursive=TRUE)
    cat(
      sprintf(
        "library(testpkg1, lib='%s')\n",
        lib.tmp
      ),
      file=file.path(pre.file, "lib.R")
    )
    tp.0 <- file.path(old.dir, "test_pkgs", "testpkg1", "testpkg1.0")
    install.packages(pkgs=tp.0, lib=lib.tmp, repos=NULL, type="src")
    unitize_dir(file.path(test.tmp, "unitizer"), auto.accept="new")

    # Upgrade the package, note we test against same store

    try(detach("package:testpkg1", unload=TRUE), silent=TRUE)
    try(remove.packages("testpkg1", lib=lib.tmp))
    tp.1 <- file.path(old.dir, "test_pkgs", "testpkg1", "testpkg1.1")
    install.packages(pkgs=tp.1, lib=lib.tmp, repos=NULL, type="src")
    unitize_dir(file.path(test.tmp, "unitizer"))
  })
}
