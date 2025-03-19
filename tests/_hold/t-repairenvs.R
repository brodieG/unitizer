source(file.path("_helper", "init.R"))

exps <- expression(1 + 1, a <- 54, b <- 38, a + b, e <- 5 * a,
    a^2, f <- e * a, matrix(rep(f, 20)))
my.unitizer <- new("unitizer", id = 1, zero.env = new.env())
# add ref.exps as new items
coi(my.unitizer <- my.unitizer + exps)
my.unitizer2 <- new("unitizer", id = 2, zero.env = new.env())
# now convert them to reference items
coi(my.unitizer2 <- my.unitizer2 + my.unitizer@items.new)
# - "messed up env ancestry repair works" --------------------------------------

# # Purposefully mess up the environments
# # UPDATE: these tests don't work since parent.env<- added checks for circular
# # environment chains in r86545.  We could probably restore functionality by
# # using a different parent env but would have to figure out what the intended
# # logic was.
# parent.env(my.unitizer2@items.ref[[2]]@env) <- baseenv()
# x <- unitizer:::healEnvs(my.unitizer2@items.ref, my.unitizer2)
# old.opt <- options(unitizer.max.env.depth = 20)
# res <- unitizer:::healEnvs(my.unitizer2@items.ref, my.unitizer2)
# is(res, "unitizerItems")
# ref.anc <- unitizer:::env_ancestry(x@base.env)
# itm.anc <- unitizer:::env_ancestry(x[[1L]]@env)
# # Items should belong to base env for reference
# identical(rev(ref.anc), head(rev(itm.anc), length(ref.anc)))
# options(old.opt)

# - "re-assigning to ignored environments handled properly" --------------------

# now `a + b` could try to re-assign to `a <- 54`, but that is same env as
# `a + b` b/c it is ignored
items.picked <- my.unitizer@items.new[-3L]
# expect_error(items.heal <- unitizer:::healEnvs(items.picked,
#     my.unitizer), NA)
# no error
items.heal <- unitizer:::healEnvs(items.picked, my.unitizer)

# - "full repair process works" ------------------------------------------------

# copy files and then try messing up environment for the object
file_test("-d", file.path("_helper"))
store <- file.path("_helper", "unitizers", "trivial.unitizer")
store.new <- file.path(TMP.DIR, store)
dir.create(store.new, recursive = TRUE)
cpy.files <- c(
  list.files(store, full.names = TRUE),
  file.path("helper", "unitizers", "trivial.R")
)
file.copy(cpy.files, file.path(TMP.DIR, cpy.files), overwrite = TRUE)

untz <- unitizer:::load_unitizers(
  list(store.new), NA_character_,
  par.frame = .GlobalEnv, interactive.mode = TRUE, mode = "unitize",
  show.progress=0L, transcript=FALSE
)
# Break env chain, store, and reload
untz[[1L]]@items.ref.calls.deparse[[5L]]
parent.env(untz[[1L]]@items.ref[[5L]]@env) <- baseenv()
# warning
unitizer:::store_unitizer(untz[[1L]])
untz.rep <- repair_environments(store.new)
# this should not give warnings
unitizer:::healEnvs(untz.rep@items.ref, untz.rep)

