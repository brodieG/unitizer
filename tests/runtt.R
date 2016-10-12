library(testthat)
library(unitizer)

local({
  # Install test packages, and set them up for removal, here in package name
  # =dirname format

  cat("setup packages\n")
  tmp.pkgs <- c(
    unitizerdummypkg1="unitizerdummypkg1",
    unitizerdummypkg2="unitizerdummypkg2",
    unitizer.fastlm="fastlm.0"
  )
  lib <- head(.libPaths(), 1L)
  if(
    any(which.inst <- names(tmp.pkgs) %in% rownames(installed.packages()))
  ) {
    # remove.packages(sprintf("unitizerdummypkg%d", 1:2))
    stop(
      "Packages\n",
      paste0(
        deparse(names(tmp.pkgs)[which.inst], width.cutoff=500), collapse=""
      ),
      "\nalready installed; cannot proceed with tests"
  ) }

  unitizer.dir <- system.file(package="unitizer")
  pkg.dirs <- file.path(unitizer.dir, "example.pkgs", tmp.pkgs)
  lib <- head(.libPaths(), 1L)

  pkg.inst <- try(
    install.packages(pkg.dirs, repos=NULL, lib=lib, type="src", quiet=TRUE)
  )
  if(inherits(pkg.inst, "try-error")) stop("Unable to install test packages")
  cat("setup demos\n")

  on.exit({
    for(i in names(tmp.pkgs)) {
      try(detach(sprintf("package:%s", i)), silent=TRUE)
      try(unloadNamespace(i), silent=TRUE)
    }
    remove.packages(names(tmp.pkgs), lib=lib)
  })
  # Setup the demo files used by a number of tests

  .unitizer.fastlm <- copy_fastlm_to_tmpdir()
  test.dir <- file.path(.unitizer.fastlm, "tests", "unitizer")
  .unitizer.test.file <- file.path(test.dir,  "fastlm1.R")
  .unitizer.test.store <- file.path(test.dir,  "fastlm1.unitizer")
  on.exit(unitizer_cleanup_demo(), add=TRUE)

  # Run tests

  test.res <- test_dir(
    "testthat",
    filter=paste(sep="|",
      # "browse",
      # "capture",
      # "change",
      # "demo",
      # "exec",
      # "get",
      # "global",
      # "handledruns"
      # "inpkg",
      # "ischecks",
      # "item",
      # "list",
      # "misc",
      # "options",
      # "parse",
      # "prompt",
      # "rename",
      # "repairenvs",
      # "search",
      # "section",
      "shim"
      # "state",
      # "text",
      # "translate",
      # "unitize",
      # "unitize2",
      # "upgrade",
      # "zzrunlast"
    )
  )
  # Check for failures and throw error if they exist since test_dir does not
  # do so on its own

  with(
    as.data.frame(test.res), {
      fail <- sum(failed)
      err <- sum(error)
      if(fail != 0 || err) stop("Errors: ", err, " Failures: ", fail)
  })
})
