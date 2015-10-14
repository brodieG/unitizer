library(unitizer)
library(testthat)
context("Get")

local({
  toy.stor <- readRDS("../interactive/unitizer/misc.unitizer/data.rds")
  test_that("Error Cases", {
    expect_error(get_unitizer(1), "No method defined")
    expect_error(get_unitizer(letters), "Argument `store.id`")
    expect_error(get_unitizer("../interactive"), "does not appear to refer to a unitizer directory")
    expect_error(get_unitizer("testthat.get.R"), "does not appear to refer to a unitizer directory")
    expect_error(set_unitizer(1), "No method defined")
    expect_error(set_unitizer(letters), "Argument `store.id` must be a 1 length character vector")
    expect_error(set_unitizer("a"), "argument \"unitizer\" is missing, with no default")
    expect_error(set_unitizer("a", "blergh"), "Argument `unitizer` must be a unitizer")
    expect_true(!file.exists("a"))
    expect_error(set_unitizer("tests/# ;!./# \\/", toy.stor), "Could not create")
  } )
  test_that("Get works as expected", {
    expect_false(get_unitizer("asldkfjskfa"))
    expect_equal(get_unitizer("../interactive/unitizer/misc.unitizer"), toy.stor)
    expect_true(is(toy.stor, "unitizer"))
  } )
  tmp.dir <- tempdir()
  tmp.sub.dir <- paste0(tmp.dir, "/get.test.dir")
  tmp.sub.dir2 <- paste0(tmp.dir, "/get.test.dir2")
  tmp.sub.dir3 <- paste0(tmp.dir, "/load.dirs")
  dir.create(tmp.dir)

  test_that("Set works as expected", {
    dir.create(tmp.sub.dir)
    expect_true(set_unitizer(tmp.sub.dir, toy.stor))
    expect_equal(readRDS(paste0(tmp.sub.dir, "/data.rds")), toy.stor)
  } )
  test_that("load/store_unitizer", {

    # Several different stores in different states (i.e. requiring upgrade,
    # not unitizers, etc.)

    dir.create(tmp.sub.dir3)
    make.path <- lapply(file.path(tmp.sub.dir3, dir("helper/load/")), dir.create)
    if(!all(unlist(make.path))) stop("Failed making paths")
    file.copy(
      list.files("helper/load", full.names=TRUE),
      tmp.sub.dir3, recursive=TRUE
    )
    par.frame <- new.env()
    store.ids <- as.list(list.files(tmp.sub.dir3, full.names=TRUE))

    expect_error(
      unitizer:::load_unitizers(
        store.ids, rep(NA_character_, length(store.ids)), par.frame=par.frame,
        interactive.mode=FALSE, mode="unitize", force.upgrade=FALSE
      ),
      "Cannot upgrade .* in non-interactive"
    )
    unitizer:::read_line_set_vals("N")
    untzs0 <- unitizer:::load_unitizers(
      store.ids, rep(NA_character_, length(store.ids)), par.frame=par.frame,
      interactive.mode=TRUE, mode="unitize"
    )
    unitizer:::read_line_set_vals(NULL)
    expect_true(all(vapply(unitizer:::as.list(untzs0), is, logical(1L), "unitizerLoadFail")))
    expect_true(
      all(
        vapply(unitizer:::as.list(untzs0[-(1:2)]), slot, character(1L), "reason") ==
          "User elected not to upgrade unitizers"
      )
    )
    untzs <- unitizer:::load_unitizers(
      store.ids, rep(NA_character_, length(store.ids)), par.frame=par.frame,
      interactive.mode=FALSE, mode="unitize", force.upgrade=TRUE
    )
    untzs.classes <- vapply(unitizer:::as.list(untzs), class, character(1L))
    expect_equal(
      untzs.classes,
      c(
        "unitizerLoadFail", "unitizerLoadFail", "unitizerLoadFail", "unitizer",
        "unitizer", "unitizer"
    ) )
    old.width <- options(width=80L)
    txt1 <- paste0(collapse=";", capture.output(show(untzs[[1L]])))
    txt2 <- paste0(collapse=";", capture.output(show(untzs[[3L]])))
    expect_match(
      txt1, "Failed Loading Unitizer:;- Test file.*;- Store.*;- Reason: `get_unitizer` error: Retrieved object is not a unitizer store"
    )
    expect_match(
      txt2, "Failed Loading Unitizer:;- Test file.*;- Store.*;- Reason: Upgrade failed: no slot of name \"items.ref\" for this object"
    )
    options(old.width)
    untzs2 <- unitizer:::load_unitizers(
      list(tmp.sub.dir2), NA_character_, par.frame, interactive.mode=FALSE,
      mode="unitize", force.upgrade=FALSE
    )
    expect_true(is(untzs2[[1L]], "unitizer"))
    expect_identical(parent.env(untzs2[[1L]]@zero.env), par.frame)
    untzs2[[1L]]@eval.time <- 33  # something that won't get rest on load so we can check our re-load

    expect_true(unitizer:::store_unitizer(untzs2[[1L]]))
    untzs2.1 <- unitizer:::load_unitizers(
      list(tmp.sub.dir2), NA_character_, par.frame, interactive.mode=FALSE,
      mode="unitize", force.upgrade=FALSE
    )
    expect_equal(untzs2.1[[1L]]@eval.time, 33)

    # Purposefully increase version to something ridiculously large to test that
    # we can't load a newer store with an older package

    untz.tmp <- readRDS(file.path(tmp.sub.dir2, "data.rds"))
    untz.tmp@version <- "9999.0.0"
    saveRDS(untz.tmp, file.path(tmp.sub.dir2, "data.rds"))

    expect_is(
      unitizer:::load_unitizers(
        list(tmp.sub.dir2), NA_character_, par.frame, interactive.mode=FALSE,
        mode="unitize", force.upgrade=FALSE
      )[[1L]],
      "unitizerLoadFail"
    )
  } )
  unlink(c(tmp.sub.dir2, tmp.sub.dir3, tmp.sub.dir), recursive=TRUE)

  test_that("is_package", {
    expect_true(unitizer:::is_package_dir(system.file(package="stats")))
    expect_true(unitizer:::is_package_dir(system.file(package="methods")))
    expect_true(unitizer:::is_package_dir(system.file(package="stats"), has.tests=TRUE))
    expect_true(unitizer:::is_package_dir(system.file(package="methods"), has.tests=TRUE))
    expect_equal(
      unitizer:::pretty_path(
        file.path(system.file(package="stats"), "DESCRIPTION")
      ),
      "package:stats/DESCRIPTION"
    )
    old.wd <- getwd()
    setwd(system.file(package="stats"))
    expect_equal(
      unitizer:::pretty_path(
        file.path(system.file(package="stats"), "DESCRIPTION")
      ),
      "DESCRIPTION"
    )
    expect_equal(
      unitizer:::pretty_path(
        file.path(system.file(package="stats"))
      ),
      "."
    )
    setwd(old.wd)
    expect_match(
      unitizer:::is_package_dir(file.path(system.file(package="stats"), "R")),  # just picked some folder we know will not work
      "No DESCRIPTION file"
    )
    expect_equal(
      unitizer:::is_package_dir("ASDFASDF"), "not an existing directory"
    )
    expect_match(
      unitizer:::is_package_dir(
        file.path(
          system.file(package="unitizer"), "example.pkgs", "baddescription1"
      ) ),
      "DESCRIPTION file did not have a package name entry"
    )
    # *get_*package_dir

    pkg.f <- file.path(
      system.file(package="unitizer"), "tests", "interactive", "run.R"
    )
    expect_true(length(unitizer:::get_package_dir(pkg.f)) == 1L)
    expect_true(length(unitizer:::get_package_dir(dirname(pkg.f))) == 1L)
    f <- tempfile()
    cat("helloworld", file=f)
    expect_true(length(unitizer:::get_package_dir(f)) == 0L)
    unlink(f)
    expect_equal(unitizer:::get_package_dir(f), character(0L))
  } )
  test_that("is_unitizer_dir", {
    base.dir <- file.path(
      system.file(package="unitizer"), "example.pkgs", "infer"
    )
    expect_false(unitizer:::is_unitizer_dir(base.dir))
    expect_true(
      unitizer:::is_unitizer_dir(
        file.path(base.dir, "tests", "unitizer", "infer.unitizer")
    ) )

  })
  test_that("infer_unitizer_location", {
    infer <- function(...) infer_unitizer_location(..., interactive.mode=FALSE)
    base.dir <- file.path(
      system.file(package="unitizer"), "example.pkgs", "infer"
    )
    # Verify package is still in state we built tests on

    expect_equal(
      c("aaa.R", "aaa.unitizer", "abc.R", "abc.unitizer", "inf.R", "inf.unitizer", "infer.R", "infer.unitizer", "zzz.R", "zzz.unitizer"),
      list.files(file.path(base.dir, "tests", "unitizer"))
    )
    # Package dir

    expect_match(infer(base.dir), "tests/unitizer/infer\\.R$")
    expect_match(infer(base.dir, type="d"), "tests/unitizer$")
    expect_match(infer(base.dir, type="u"), "tests/unitizer/infer\\.unitizer$")

    expect_warning(
      inf.dir <- infer(file.path(base.dir, "*")), "5 possible targets"
    )
    expect_equal(file.path(base.dir, "*"), inf.dir)
    expect_match(infer(file.path(base.dir, "z")), "tests/unitizer/zzz\\.R$")
    expect_match(
      infer(file.path(base.dir, "z"), type="u"),
      "tests/unitizer/zzz\\.unitizer$"
    )
    # Normal dir

    base.dir2 <- file.path(base.dir, "tests", "unitizer")
    expect_warning(inf.dir2 <- infer(base.dir2), "5 possible targets")  # note don't need * to generate warning
    expect_equal(base.dir2, inf.dir2)
    expect_warning(infer(file.path(base.dir2, "a")), "2 possible targets")
    expect_warning(
      infer(file.path(base.dir2, "a"), type="u"), "2 possible targets"
    )
    expect_match(infer(file.path(base.dir2, "z")), "tests/unitizer/zzz\\.R$")
    expect_match(
      infer(file.path(base.dir2, "z"), type="u"),
      "tests/unitizer/zzz\\.unitizer$"
    )
    # Interactive mode

    unitizer:::read_line_set_vals(c("26", "Q"))
    expect_equal(
      unitizer:::infer_unitizer_location(file.path(base.dir, "*"), type="f", interactive.mode=TRUE),
      file.path(base.dir, "*")
    )
    unitizer:::read_line_set_vals(c("5"))
    expect_equal(
      unitizer:::infer_unitizer_location(file.path(base.dir, "*"), type="f", interactive.mode=TRUE),
      file.path(base.dir, "tests", "unitizer", "zzz.R")
    )
    unitizer:::read_line_set_vals(NULL)
  })
  test_that("test file / store manip", {
    expect_identical(unitizer:::as.store_id_chr(file.path(getwd(), "hello")), "hello")
    expect_error(unitizer:::as.store_id_chr(structure("hello", class="untz_stochrerr")), "Unable to convert")
    as.character.custstore <- function(x, ...) x
    expect_match(
      unitizer:::best_store_name(structure(list("hello", class="custstore")), "hello"),
      "unitizer for .*hello"
    )
    expect_match(
      unitizer:::best_store_name(structure(list("hello", class="custstore")), NA_character_),
      "untranslateable"
    )
    expect_match(
      unitizer:::best_file_name(structure(list("hello", class="custstore")), NA_character_),
      "unknown-test-file"
    )
  })
} )
