library(unitizer)
library(testthat)
# context("Repair Envs")
exps <- expression(1 + 1, a <- 54, b <- 38, a + b, e <- 5 * a, 
    a^2, f <- e * a, matrix(rep(f, 20)))
my.unitizer <- new("unitizer", id = 1, zero.env = new.env())
# add ref.exps as new items
capture.output(my.unitizer <- my.unitizer + exps)
my.unitizer2 <- new("unitizer", id = 2, zero.env = new.env())
# now convert them to reference items
capture.output(my.unitizer2 <- my.unitizer2 + my.unitizer@items.new)
# test_that("messed up env ancestry repair works", {
#     parent.env(my.unitizer2@items.ref[[2]]@env) <- baseenv()
#     expect_warning(capture.output(x <- unitizer:::healEnvs(my.unitizer2@items.ref, 
#         my.unitizer2), type = "message"), "Detected corrupted environment history")
#     old.opt <- options(unitizer.max.env.depth = 20)
#     on.exit(old.opt)
#     expect_warning(capture.output(res <- unitizer:::healEnvs(my.unitizer2@items.ref, 
#         my.unitizer2), type = "message"), "Detected corrupted environment history")
#     expect_true(is(res, "unitizerItems"))
#     ref.anc <- unitizer:::env_ancestry(x@base.env)
#     itm.anc <- unitizer:::env_ancestry(x[[1L]]@env)
#     expect_identical(rev(ref.anc), head(rev(itm.anc), length(ref.anc)))
# })
# - "messed up env ancestry repair works" --------------------------------------


# Purposefully mess up the environments
parent.env(my.unitizer2@items.ref[[2]]@env) <- baseenv()
# expect_warning(capture.output(x <- unitizer:::healEnvs(my.unitizer2@items.ref, 
#     my.unitizer2), type = "message"), "Detected corrupted environment history")
capture.output(x <- unitizer:::healEnvs(my.unitizer2@items.ref, 
    my.unitizer2), type = "message")
old.opt <- options(unitizer.max.env.depth = 20)
on.exit(old.opt)
# expect_warning(capture.output(res <- unitizer:::healEnvs(my.unitizer2@items.ref, 
#     my.unitizer2), type = "message"), "Detected corrupted environment history")
capture.output(res <- unitizer:::healEnvs(my.unitizer2@items.ref, 
    my.unitizer2), type = "message")
# expect_true(is(res, "unitizerItems"))
is(res, "unitizerItems")
ref.anc <- unitizer:::env_ancestry(x@base.env)
itm.anc <- unitizer:::env_ancestry(x[[1L]]@env)
# Items should belong to base env for reference
# expect_identical(rev(ref.anc), head(rev(itm.anc), length(ref.anc)))
rev(ref.anc)
# test_that("re-assigning to ignored environments handled properly", 
#     {
#         items.picked <- my.unitizer@items.new[-3L]
#         expect_error(items.heal <- unitizer:::healEnvs(items.picked, 
#             my.unitizer), NA)
#     })
# - "re-assigning to ignored environments handled properly" --------------------


# now `a + b` could try to re-assign to `a <- 54`, but that is same env as
# `a + b` b/c it is ignored
items.picked <- my.unitizer@items.new[-3L]
# expect_error(items.heal <- unitizer:::healEnvs(items.picked, 
#     my.unitizer), NA)
items.heal <- unitizer:::healEnvs(items.picked, my.unitizer)
# test_that("full repair process works", {
#     expect_true(file_test("-d", file.path("helper")))
#     tmp.dir <- tempfile()
#     store <- file.path("helper", "trivial.unitizer")
#     store.new <- file.path(tmp.dir, store)
#     dir.create(store.new, recursive = TRUE)
#     cpy.files <- c(list.files(store, full.names = TRUE), file.path("helper", 
#         "trivial.R"))
#     file.copy(cpy.files, file.path(tmp.dir, cpy.files), overwrite = TRUE)
#     expect_warning(untz <- unitizer:::load_unitizers(list(store.new), 
#         NA_character_, par.frame = .GlobalEnv, interactive.mode = TRUE, 
#         mode = "unitize"), "Instantiated global object")
#     expect_identical(untz[[1L]]@items.ref.calls.deparse[[5L]], 
#         "y * x")
#     parent.env(untz[[1L]]@items.ref[[5L]]@env) <- baseenv()
#     unitizer:::store_unitizer(untz[[1L]])
#     expect_warning(untz.rep <- repair_environments(store.new), 
#         "Instantiated global object without|Detected corrupted environment")
#     expect_warning(unitizer:::healEnvs(untz.rep@items.ref, untz.rep), 
#         NA)
#     unlink(tmp.dir, recursive = TRUE)
# })
# - "full repair process works" ------------------------------------------------


# copy files and then try messing up environment for the object
# expect_true(file_test("-d", file.path("helper")))
file_test("-d", file.path("helper"))
tmp.dir <- tempfile()
store <- file.path("helper", "trivial.unitizer")
store.new <- file.path(tmp.dir, store)
dir.create(store.new, recursive = TRUE)
cpy.files <- c(list.files(store, full.names = TRUE), file.path("helper", 
    "trivial.R"))
file.copy(cpy.files, file.path(tmp.dir, cpy.files), overwrite = TRUE)
# expect_warning(untz <- unitizer:::load_unitizers(list(store.new), 
#     NA_character_, par.frame = .GlobalEnv, interactive.mode = TRUE, 
#     mode = "unitize"), "Instantiated global object")
untz <- unitizer:::load_unitizers(list(store.new), NA_character_, 
    par.frame = .GlobalEnv, interactive.mode = TRUE, mode = "unitize")
# Break env chain, store, and reload
# expect_identical(untz[[1L]]@items.ref.calls.deparse[[5L]], "y * x")
untz[[1L]]@items.ref.calls.deparse[[5L]]
parent.env(untz[[1L]]@items.ref[[5L]]@env) <- baseenv()
unitizer:::store_unitizer(untz[[1L]])
# expect_warning(untz.rep <- repair_environments(store.new), "Instantiated global object without|Detected corrupted environment")
untz.rep <- repair_environments(store.new)
# this should not give warnings
# expect_warning(unitizer:::healEnvs(untz.rep@items.ref, untz.rep), 
#     NA)
unitizer:::healEnvs(untz.rep@items.ref, untz.rep)
unlink(tmp.dir, recursive = TRUE)
