# source("../extra/scratch/corrupt-db-test.R", echo=TRUE)
# Overflow tests from testthat.unitize.R

source(file.path("_helper", "init.R"))
source(file.path("_helper", "pkgs.R"))

x <- 1:10
y <- x ^ 3

library(unitizer)

# library(utzflm, lib.loc=getOption('unitizer.tmp.lib.loc'))
# detach('package:utzflm', unload=TRUE)

library(utzflm, lib.loc=TMP.LIB)
fastlm(x, y)

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
install.packages(FLM, repos=NULL, type='src', lib=TMP.LIB)

# lazyLoadDBfetch(ekey, datafile, compressed, envhook)


library(utzflm, lib.loc=TMP.LIB)
fastlm


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
