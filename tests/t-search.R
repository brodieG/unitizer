source(file.path("_helper", "init.R"))
source(file.path("_helper", "pkgs.R"))

unitizer.dummy.list <- list(A = 1, B = 2, C = 3)
unitizer.dummy.list.2 <- list(A = 13, B = 24, C = 35)
# can't unload `unitizer`, ruins `covr`
try(detach("package:unitizer"), silent = TRUE)
try(detach("package:unitizerdummypkg1", unload = TRUE), silent = TRUE)
try(detach("package:unitizerdummypkg2", unload = TRUE), silent = TRUE)
while ("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))
state.set <- setNames(rep(2L, length(unitizer:::.unitizer.global.settings.names)),
    unitizer:::.unitizer.global.settings.names)
library(unitizer)
library(unitizerdummypkg1, lib.loc = TMP.LIB)
library(unitizerdummypkg2, lib.loc = TMP.LIB)

# - "Detecting packages" -------------------------------------------------------

unitizer:::is.loaded_package("package:unitizer")
unitizer:::is.loaded_package("unitizer")   # FALSE
unitizer:::is.loaded_package("package:stats")
try(unitizer:::is.loaded_package(1))
try(unitizer:::is.loaded_package(letters))
unitizer:::is.loaded_package("Autoloads")  # FALSE
is.list(pkg.dat <- unitizer:::get_package_data())
all(
  vapply(
    pkg.dat, function(x) is.list(x) && identical(names(x),
    c("names", "lib.loc", "version")), logical(1L)
) )

# - "Path Compression" ---------------------------------------------------------

search.init.full <- unitizer:::search_as_envs()
search.init <- search.init.full$search.path

head(unitizer:::compress_search_data(search.init.full), 3L)

# - "Moving Objects on Search Path Works" --------------------------------------

if (length(search.init) < 6L) stop("Unexpetedly short search path")
untz.glob <- unitizer:::unitizerGlobal$new(enable.which = state.set,
    set.global = TRUE)

try(unitizer:::move_on_path(5L, 2L, untz.glob))
try(unitizer:::move_on_path(1L, 2L, untz.glob))
unitizer:::move_on_path(2L, 5L, untz.glob)
# can't compare actual environments as they change when detached and
# re-attached

all.equal(
  names(unitizer:::search_as_envs()$search.path),
  names(search.init[c(1L, 5L, 2L:4L, 6L:length(search.init))])
)
# Now let's undo the previous move, by pushing second pack back to
# original position
for (i in rep(5L, 3L)) unitizer:::move_on_path(2L, 5L, untz.glob)
unitizer:::search_dat_equal(unitizer:::search_as_envs(), search.init.full)
untz.glob$release()

# - "Search Path Journaling Works" ---------------------------------------------

try(detach("package:unitizer"), silent = TRUE)
try(detach("package:unitizerdummypkg1", unload = TRUE), silent = TRUE)
try(detach("package:unitizerdummypkg2", unload = TRUE), silent = TRUE)
library(unitizer)
# Initialize a global tracking object.  Doing it funny here because we don't
# want to run the search_path_trim command yet, and that would happen if we
# did a normal init
# will be modified later
search.ref <- NULL
search.init <- unitizer:::search_as_envs()
untz.glob <- unitizer:::unitizerGlobal$new(enable.which = state.set,
    set.global = TRUE)

stat.tpl <- new("unitizerGlobalStatus", search.path = 2L, working.directory = 2L,
    options = 2L, random.seed = 2L, namespaces = 2L)
# these need to be done outside of `test_that` b/c `test_that` sets the
# rlang_trace_top_env option
st.0 <- untz.glob$indices.last
st.1 <- untz.glob$state()

# Note, these are intended to be run without the shimming in place
identical(untz.glob$status, stat.tpl)

# state should only be recorded if it changes
st.0
identical(st.0, st.1)
# Add a package
library("unitizerdummypkg1", lib.loc = TMP.LIB)
st.2 <- untz.glob$state()
# have two recorded states
st.2@search.path
# should have one more item
diff(sapply(untz.glob$tracking@search.path, function(x) length(x$search.path)))
environmentName(untz.glob$tracking@search.path[[2L]]$search.path[[2L]])
sp.tmp <- untz.glob$tracking@search.path
# note we compare attribute separately because subsetting drops them
identical(sp.tmp[[1L]]$search.path, sp.tmp[[2L]]$search.path[-2L])

identical(
  sp.tmp[[1L]]$ns.dat,
  sp.tmp[[2L]]$ns.dat[names(sp.tmp[[2L]]$ns.dat) != "unitizerdummypkg1"]
)
# Add another package at a different position
library("unitizerdummypkg2", pos = 4L, lib.loc = TMP.LIB)
st.3 <- untz.glob$state()
diff(sapply(untz.glob$tracking@search.path, function(x) length(x$search.path)))
environmentName(
  untz.glob$tracking@search.path[[st.3@search.path]]$search.path[[4L]]
)
# Attach a list
attach(unitizer.dummy.list)
search.ref <- untz.glob$state()
environmentName(
  untz.glob$tracking@search.path[[search.ref@search.path]]$search.path[[2L]]
)
identical(
  as.list(
    untz.glob$tracking@search.path[[search.ref@search.path]]$search.path[[2L]]
  ),
  unitizer.dummy.list
)
# And one more, but modified
unitizer.dummy.list.2 <- list(A = 13, B = 24, C = 35)
attach(unitizer.dummy.list.2, pos = 4L, name = "unitizer.dummy.list")
st.4 <- untz.glob$state()
curr.sp.ind <- untz.glob$indices.last@search.path
environmentName(untz.glob$tracking@search.path[[curr.sp.ind]]$search.path[[4L]])
# Make sure search path is lining up
all.equal(
  names(untz.glob$tracking@search.path[[curr.sp.ind]]$search.path), search()
)
identical(
  as.list(untz.glob$tracking@search.path[[curr.sp.ind]]$search.path[[4L]]),
  unitizer.dummy.list.2
)
identical(
  as.list(untz.glob$tracking@search.path[[curr.sp.ind]]$search.path[[2L]]),
  unitizer.dummy.list
)
# should still point to same environment
identical(
  untz.glob$tracking@search.path[[curr.sp.ind - 1L]]$search.path[[2L]],
  untz.glob$tracking@search.path[[curr.sp.ind]]$search.path[[2L]]
)
# state shouldn't have changed
identical(untz.glob$state(), st.4)

# detach some stuff
# this is the first list
detach(2L)
untz.glob$state()
curr.sp.ind <- untz.glob$indices.last@search.path
identical(
  untz.glob$tracking@search.path[[curr.sp.ind]]$search.path,
  untz.glob$tracking@search.path[[curr.sp.ind - 1L]]$search.path[-2L]
)
detach("package:unitizerdummypkg2")
untz.glob$state()
curr.sp.ind <- untz.glob$indices.last@search.path
identical(
  untz.glob$tracking@search.path[[curr.sp.ind]]$search.path,
  untz.glob$tracking@search.path[[curr.sp.ind - 1L]]$search.path[-5L]
)

# - "Resetting search path" ----------------------------------------------------

identical(
  as.list(as.environment("unitizer.dummy.list")), unitizer.dummy.list.2
)
# set to just after we added the original dummy list
untz.glob$reset(search.ref)
identical(as.list(as.environment("unitizer.dummy.list")), unitizer.dummy.list)
# Confirm we actually set to expected path
# NOTE: not sure if with updates this can work
all.equal(
  names(unitizer:::search_as_envs()$search.path),
  names(untz.glob$tracking@search.path[[search.ref@search.path]]$search.path)
)
# Reset to very beginning
untz.glob$resetFull()
untz.glob$release()
# compare with all.equal to make sure we use S4 method
unitizer:::search_dat_equal(unitizer:::search_as_envs(), search.init)

# - "Search Path Trim / Restore" -----------------------------------------------

search.init <- unitizer:::search_as_envs()
untz.glob <- unitizer:::unitizerGlobal$new(enable.which = state.set,
    set.global = TRUE)
library(unitizerdummypkg1, lib.loc = TMP.LIB)
library(unitizerdummypkg2, lib.loc = TMP.LIB)
unitizer:::search_path_trim(global = untz.glob)
untz.glob$state()
sp.keep <- unitizer:::keep_sp_default()
identical(
  search(), 
  sp.keep[match(names(search.init$search.path), sp.keep, nomatch = 0L)]
)
untz.glob$resetFull()
untz.glob$release()
unitizer:::search_dat_equal(unitizer:::search_as_envs(), search.init)
try(detach("package:unitizerdummypkg1", unload = TRUE), silent = TRUE)
try(detach("package:unitizerdummypkg2", unload = TRUE), silent = TRUE)
while ("unitizer.dummy.list" %in% search()) try(detach("unitizer.dummy.list"))

# - "Loaded Namespaces don't cause issues" -------------------------------------

# had a problem earlier trying to re-attach namespaces
loadNamespace("unitizerdummypkg1", lib.loc = TMP.LIB)
untz.glob <- unitizer:::unitizerGlobal$new(enable.which = state.set,
    set.global = TRUE)
unitizer:::search_path_trim(global = untz.glob)
unitizer:::namespace_trim(global = untz.glob)
untz.glob$state()
loadNamespace("unitizerdummypkg2", lib.loc = TMP.LIB)
untz.glob$state()
"unitizerdummypkg1" %in% loadedNamespaces()  # FALSE
"unitizerdummypkg2" %in% loadedNamespaces()
untz.glob$resetFull()
untz.glob$release()
"unitizerdummypkg1" %in% loadedNamespaces()
"unitizerdummypkg2" %in% loadedNamespaces()  # FALSE
unloadNamespace("unitizerdummypkg1")

# - "Prevent Namespace Unload Works" -------------------------------------------

old.opt <- options(unitizer.namespace.keep = "unitizerdummypkg1")
loadNamespace("unitizerdummypkg1", lib.loc = TMP.LIB)
glb <- unitizer:::unitizerGlobal$new(set.global = TRUE)
glb$status@options <- 2L
unitizer:::unload_namespaces("unitizerdummypkg1", global = glb)
glb$ns.opt.conflict@conflict
glb$ns.opt.conflict@namespaces
unloadNamespace("unitizerdummypkg1")
options(old.opt)
glb$release()

# - "Generate unique names" ----------------------------------------------------

unitizer:::unitizerUniqueNames(list(search.path = c(goodbye = "0",
    hello = "1", goodbye = "2", goodbye = "3")))

# - "Fake Package Re-attach" ---------------------------------------------------

# Make sure that aspects of search path management other than search path
# survive a failure caused by bad search path env (#252, #253).

owd <- getwd()
test.f <- paste0(tempfile(), ".R")
writeLines("
f <- tempfile()
dir.create(f)
setwd(f)
# Package assumed non-existing; R could disallow this in the future
# which could change the test.
attach(list(x=42), name='package:adfaadcxuqyojfnkfadsf')
1 + 1", test.f)
out <- unitizer:::capture_output(
  try(unitize(test.f, state='recommended', interactive.mode=FALSE))
)
any(grepl("mismatch between actual search path and tracked", out$message))
identical(owd, getwd())  # confirm working directory restored

