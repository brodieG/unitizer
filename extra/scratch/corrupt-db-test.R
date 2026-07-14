# source("../extra/scratch/corrupt-db-test.R", echo=TRUE)
# Overflow tests from testthat.unitize.R

source(file.path("_helper", "init.R"))
source(file.path("_helper", "pkgs.R"))

# ns <- asNamespace('utzflm')
# .getNamespaceInfo(ns, "path")

x <- 1:10
y <- x ^ 3

library(unitizer)

library(utzflm, lib.loc=TMP.LIB)

fastlm(x, y)

rdb.file <- file.path(TMP.LIB, 'utzflm/R/utzflm.rdb')
rdx.file <- file.path(TMP.LIB, 'utzflm/R/utzflm.rdx')
rdx.0 <- readRDS(rdx.file)
key.0 <- rdx.0$variables$get_intercept

orig.md5 <- tools::md5sum(rdb.file)
orig.size <- file.size(rdb.file)
lazyLoadDBfetch(key.0, rdb.file, 1L, NULL) # Works

unlink(list.dirs(FLM.TEST.DIR, recursive = FALSE), recursive = TRUE)
{
  dir <- FLM
  unitizer:::check_test_dir(dir)
  detach("package:utzflm", unload=TRUE)
  lm.dir <- "flm1"
  untz.dir <- system.file(package="unitizer")
  lm.dir.full <- file.path(untz.dir, "expkg", lm.dir)
  cpy.files <- unitizer:::.test.core.files
  cpy.from <- file.path(lm.dir.full, cpy.files)
  cpy.to <- file.path(dir, cpy.files)

  invisible(file.copy(cpy.from, cpy.to, overwrite=TRUE))
}
install.packages(FLM, repos=NULL, type='src', lib=TMP.LIB, quiet=TRUE)

new.md5 <- tools::md5sum(rdb.file)
new.size <- file.size(rdb.file)

rdx.1 <- readRDS(rdx.file)
rdx.1$variables
key.1 <- rdx.1$variables$get_intercept

rbind(orig.md5, new.md5)
rbind(orig.size, new.size)

try(lazyLoadDBfetch(key.0, rdb.file, 1L, NULL))  # Works
try(lazyLoadDBfetch(key.1, rdb.file, 1L, NULL))  # Error

stop()

# So even though the file itself has clearly changed, and even though we're
# directly providing the file name to `lazyLoadDBfetch`, the database itself is
# not being updated.  Which is kind of bizarre because the logic itself reads
# from a file if we have the tracing right.

# The file sizes clearly track.  But for some reason in the code we're getting
# the old file instead of the new.

orig.size == sum(rdx.0$variables[[length(rdx.0$variables)]])
new.size == sum(rdx.1$variables[[length(rdx.1$variables)]])

# This is the value we get from the original file, so the issue is that the
# rdb.file is somehow getting cached by something?  And not replaced

lazyLoadDBfetch(
  c(3681L,1081L), rdb.file, 1L, NULL
)

# Problem is that in base/R/namespace.R: 270:
#
#            setNamespaceInfo(env, "path",
#                             normalizePath(file.path(lib, name), "/", TRUE))
# 
# but in that's not done in base/R/library.R:760, so then
#
#     .getNamespaceInfo(asNamespace('utzflm'), "path")
#
# and the path provided to `install.packages(..., lib=)` mismatch

# library(utzflm, lib.loc=TMP.LIB)
# fastlm
# test.file.1 <- file.path(FLM.TEST.DIR, "unitizer.fastlm.R")
# test.file.2 <- file.path(FLM.TEST.DIR, "unitizer.fastlm2.R")
# test.store <- file.path(FLM.TEST.DIR, "store2.unitizer")
# # First auto accept all initial tests, and then re-run with second version to
# # make sure deleted tests are where we think they should be
# out.1 <-
#   unitizer:::capture_output(unitize(test.file.1, test.store, auto.accept = "new"))
# unitizer:::read_line_set_vals(c("B", "Q"))
# out.2 <- unitizer:::capture_output(
#   untz.2 <- unitize(test.file.2, test.store, interactive.mode = TRUE)
# )
# attributes(untz.2) <- NULL
# untz.2
