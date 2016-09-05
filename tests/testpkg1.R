# Simulate a full install and upgrade cycle

library("unitizer")
if(interactive()) {
 if("testpkg1" %in% loadedNamespaces()) stop("`testpkg1` already loaded")
  if(!identical(basename(getwd()), "tests")) stop("Should be in /tests")

  par.dir <- tempfile()
  on.exit({
    try(detach("package:testpkg1", unload=TRUE))
    try(remove.packages("testpkg1", lib=lib.tmp))
    unlink(par.dir, recursive=TRUE)
  })
  dir.create(par.dir)
  lib.tmp <- file.path(par.dir, "lib")
  untz.tmp <- file.path(par.dir, "unitizer")
  dir.create(lib.tmp)
  dir.create(untz.tmp)
  store <- file.path(untz.tmp, "testpgk1.unitizer")

  tp.0 <- file.path("test_pkgs", "testpkg1", "testpkg1.0")
  install.packages(pkgs=tp.0, lib=lib.tmp, repos=NULL, type="src")
  library("testpkg1", lib.loc=lib.tmp)
  unitize(
    file.path(tp.0, "tests", "tests.R"), store.id=store, state="off",
    auto.accept="new"
  )

  # Upgrade the package, note we test against same store

  detach("package:testpkg1", unload=TRUE)
  remove.packages("testpkg1", lib=lib.tmp)
  tp.1 <- file.path("test_pkgs", "testpkg1", "testpkg1.1")
  install.packages(pkgs=tp.1, lib=lib.tmp, repos=NULL, type="src")
  library("testpkg1", lib.loc=lib.tmp)
  unitize(file.path(tp.1, "tests", "tests.R"), store.id=store, state="off")
}
