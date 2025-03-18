source(file.path("_helper", "init.R"))
source(file.path("aammrtf", "mock.R"))

toy.path <- file.path("_helper", "unitizers", "misc.unitizer")
toy.stor <- readRDS(file.path(toy.path, "data.rds"))

# - "Error Cases" --------------------------------------------------------------

try(get_unitizer(1))
try(get_unitizer(letters))
try(get_unitizer("_helper"))
try(get_unitizer("t-get.R"))
try(set_unitizer(1))
try(set_unitizer(letters))
# 4.3 changed reporting of missing argument errors
tryCatch(set_unitizer("a"), error=function(e) conditionMessage(e))
try(set_unitizer("a", "blergh"))
!file.exists("a") # TRUE
try(suppressWarnings(set_unitizer("tests/# ;!./# \\/", toy.stor)))

# - "Get works as expected" ----------------------------------------------------

tmp.dir <- tempfile()
dir.create(tmp.dir)
tmp.sub.dir <- paste0(tmp.dir, "/get.test.dir")
tmp.fake.utz <- paste0(tmp.dir, "/fake.unitizer")

# expect_false(get_unitizer("asldkfjskfa"))
get_unitizer("asldkfjskfa") # FALSE
all.equal(get_unitizer(toy.path), toy.stor)
is(toy.stor, "unitizer")
dir.create(tmp.fake.utz)
fake.utz <- file.path(tmp.fake.utz, "data.rds")
cat("# this is not an RDS\n", file = fake.utz)
# expect_error(capture.output(get_unitizer(tmp.fake.utz), type = "message"),
#     "Failed loading unitizer")
try(capture.output(get_unitizer(tmp.fake.utz), type = "message"))

tmp.sub.dir <- paste0(tmp.dir, "/get.test.dir")
tmp.sub.dir2 <- paste0(tmp.dir, "/get.test.dir2")
tmp.sub.dir3 <- paste0(tmp.dir, "/load.dirs")

# - "Set works as expected" ----------------------------------------------------

dir.create(tmp.sub.dir)
set_unitizer(tmp.sub.dir, toy.stor)
all.equal(readRDS(paste0(tmp.sub.dir, "/data.rds")), toy.stor)
just.a.file <- tempfile()
on.exit(unlink(just.a.file))
cat("just a file\n", file = just.a.file)
err <- capture.output(try(set_unitizer(just.a.file, toy.stor)), type='message')
any(grepl('not a directory', err))

# - "load/store_unitizer" ------------------------------------------------------

# Several different stores in different states (i.e. requiring upgrade,
# not unitizers, etc.)
dir.create(tmp.sub.dir3)
make.path <- lapply(file.path(tmp.sub.dir3, dir("_helper/ref-objs/load/")),
    dir.create)
if (!all(unlist(make.path))) stop("Failed making paths")
file.copy(list.files("_helper/ref-objs/load", full.names = TRUE), tmp.sub.dir3,
    recursive = TRUE)
par.frame <- new.env()
store.ids <- as.list(list.files(tmp.sub.dir3, full.names = TRUE))

# must be upgraded, but cannot
load.try <- unitizer:::capture_output(
  try(
    unitizer:::load_unitizers(store.ids, rep(NA_character_,
      length(store.ids)), par.frame = par.frame, interactive.mode = FALSE,
      mode = "unitize", force.upgrade = FALSE, show.progress=0L, transcript=FALSE
) ) )
any(grepl('could not be loaded', load.try$message))
any(grepl('could not be upgraded', load.try$message))
any(grepl('Cannot proceed', load.try$message))

# handle failure in store_unitizer, we just try this on one of the store ids

out <- unitizer:::capture_output(
  unitizer:::load_unitizers(
    store.ids[4], rep(NA_character_, length(store.ids))[4],
    par.frame = par.frame, interactive.mode = FALSE, mode = "unitize",
    force.upgrade = TRUE, show.progress=0L, transcript=FALSE
  )
)
any(grepl('Upgraded test file does not match original', out$message))

# try weird store ids
out <- unitizer:::capture_output(
  invalid.store <- try(
    unitizer:::load_unitizers(
      list(structure("hello", class = "unitizer_error_store")),
      NA_character_, par.frame = par.frame,
      interactive.mode = FALSE, mode = "unitize", force.upgrade = FALSE,
      show.progress=0L, transcript=FALSE
  ) )
)
inherits(invalid.store, "try-error")
any(grepl("returned something other than", out$message))

# Load mix of loadable and not loadable objects
glob <- suppressWarnings(unitizer:::unitizerGlobal$new())
# with warning: "does not exist|test file does not")
out <- unitizer:::capture_output(
  untzs <- try(
    unitizer:::load_unitizers(
      store.ids, rep(NA_character_, length(store.ids)), par.frame = par.frame,
      interactive.mode = FALSE, mode = "unitize", force.upgrade = TRUE,
      global = glob, show.progress=0L, transcript=FALSE
) ) )
inherits(untzs, "try-error")
any(grepl('could not be loaded', out$message))
any(grepl('could not be upgraded', out$message))
any(grepl('Cannot proceed', out$message))

# Test failure of storage of a loaded and upgraded unitizers

untzs <- unitizer:::load_unitizers(
  store.ids[4], NA_character_, par.frame = par.frame,
  interactive.mode = FALSE, mode = "unitize", force.upgrade = TRUE,
  global = glob, show.progress=0L, transcript=FALSE
)
mock(unitizer:::set_unitizer, quote(stop("set fail")))
try(unitizer:::store_unitizer(untzs[[1]]))
unmock(unitizer:::set_unitizer)

# Try reloading already loaded unitisers
reload <- unitizer:::as.list(untzs)
# this creates a global object, hence warning
untzs1a <- unitizer:::load_unitizers(
  reload, rep(NA_character_, length(reload)), par.frame = par.frame,
  interactive.mode = FALSE, mode = "unitize", force.upgrade = FALSE,
  show.progress=0L, transcript=FALSE
)
all(vapply(unitizer:::as.list(untzs1a), is, logical(1L), "unitizer"))

# misc tests
# warning Instantiated global object without

untzs2 <- unitizer:::load_unitizers(
  list(tmp.sub.dir2), NA_character_, par.frame, interactive.mode = FALSE,
  mode = "unitize", force.upgrade = FALSE, show.progress=0L, transcript=FALSE
)
is(untzs2[[1L]], "unitizer")
identical(parent.env(untzs2[[1L]]@zero.env), par.frame)

# something that won't get reset on load so we can check our re-load
untzs2[[1L]]@eval.time <- 33
unitizer:::store_unitizer(untzs2[[1L]])

# warning Instantiated global object without
untzs2.1 <- unitizer:::load_unitizers(
  list(tmp.sub.dir2), NA_character_, par.frame, interactive.mode = FALSE,
  mode = "unitize", force.upgrade = FALSE, show.progress=0L, transcript=FALSE
)
untzs2.1[[1L]]@eval.time # 33
unlink(c(tmp.sub.dir2, tmp.sub.dir3, tmp.sub.dir), recursive = TRUE)

# - "is_package" ---------------------------------------------------------------

unitizer:::is_package_dir(system.file(package = "stats"))
unitizer:::is_package_dir(system.file(package = "methods"))

## Seems like some change now tests no longer installed by default with
## packages, at least in the unix distros, so can't easily test with
## has.tests==TRUE

unitizer:::pretty_path(file.path(system.file(package = "stats"),
    "DESCRIPTION"))
old.wd <- getwd()
setwd(system.file(package = "stats"))
unitizer:::pretty_path(file.path(system.file(package = "stats"), "DESCRIPTION"))
unitizer:::pretty_path(file.path(system.file(package = "stats")))
setwd(old.wd)

# just picked some folder we know will not work (No Desc)
unitizer:::is_package_dir(file.path(system.file(package = "stats"), "R"))
unitizer:::is_package_dir("ASDFASDF")
unitizer:::is_package_dir(file.path(system.file(package = "unitizer"),
    "expkg", "baddescription1"))
# *get_*package_dir
pkg.f <- file.path(system.file(package = "unitizer"), "tests",
    "interactive", "run.R")
length(unitizer:::get_package_dir(pkg.f)) == 1L
length(unitizer:::get_package_dir(dirname(pkg.f))) == 1L
f <- tempfile()
cat("helloworld", file = f)
length(unitizer:::get_package_dir(f)) == 0L
unlink(f)

# some more tests moved to t-demo.R to avoid reloading pkgs

# - "is_unitizer_dir" ----------------------------------------------------------

base.dir <- file.path(system.file(package = "unitizer"), "expkg", "infer")
unitizer:::is_unitizer_dir(base.dir) # FALSE
unitizer:::is_unitizer_dir(
  file.path(base.dir, "tests", "unitizer", "infer.unitizer")
)
# - "infer_unitizer_location" --------------------------------------------------

infer <- function(...) infer_unitizer_location(..., interactive.mode = FALSE)
base.dir <- file.path(system.file(package = "unitizer"), "expkg", "infer")

# Verify package is still in state we built tests on; need to sort b/c
# different platforms have different lexical sorts
identical(
  sort(c("aaa.R", "aaa.unitizer", "abc.R", "abc.unitizer", "inf.R",
      "inf.unitizer", "infer.R", "infer.unitizer", "zzz.R", "zzz.unitizer")),
  list.files(file.path(base.dir, "tests", "unitizer"))
)
# Package dir
unitizer:::capture_output(inf <- infer(base.dir))
basename(inf)
unitizer:::capture_output(inf <- infer(base.dir, type = "d"))
basename(inf)
unitizer:::capture_output(inf <- infer(base.dir, type = "u"))
basename(inf)

inf.dir <- infer(file.path(base.dir, "*")) # warn
identical(file.path(base.dir, "*"), inf.dir)

unitizer:::capture_output(inf <- infer(file.path(base.dir, "z")))
basename(inf)
unitizer:::capture_output(inf <- infer(file.path(base.dir, "z"), type = "u"))
basename(inf)

# Normal dir
base.dir2 <- file.path(base.dir, "tests", "unitizer")
# note don't need * to generate warning
out <- unitizer:::capture_output(inf.dir2 <- infer(base.dir2))  # warn
any(grepl("5 possible targets", out$message))
identical(base.dir2, inf.dir2)
out <- unitizer:::capture_output(infer(file.path(base.dir2, "a")))
any(grepl("2 possible targets", out$message))
out <- unitizer:::capture_output(infer(file.path(base.dir2, "a"), type = "u"))
any(grepl("2 possible targets", out$message))
out <-
  unitizer:::capture_output(fname <- basename(infer(file.path(base.dir2, "z"))))
fname
any(grepl('Inferred test file location:', out))
out <- unitizer:::capture_output(
  fname <- basename(infer(file.path(base.dir2, "z"), type="u"))
)
fname
any(grepl('Inferred unitizer location:', out))

# Random file without setting working dir first, in order for this to work
# non-interactivel we need it to work with the R CMD check dir structure,
# and possibly with the covr dir structure
if (interactive())  infer("tests2")

# Interactive mode
unitizer:::read_line_set_vals(c("26", "Q"))
# warn/output
select <- unitizer:::infer_unitizer_location(
  file.path(base.dir, "*"), type = "f", interactive.mode = TRUE
)
identical(select, file.path(base.dir, "*"))

unitizer:::read_line_set_vals(c("5"))
# output
sel.loc <- unitizer:::infer_unitizer_location(file.path(base.dir,
    "*"), type = "f", interactive.mode = TRUE)
basename(sel.loc)
unitizer:::read_line_set_vals(NULL)

# Non standard inferences
# warn
out <- unitizer:::capture_output(
  unitizer:::infer_unitizer_location(NULL, interactive = FALSE)
)
any(grepl("too many to unambiguously", out$message))

fake.class <- structure(list(), class = "thisclassdoesn'texist")
identical(infer(fake.class), fake.class)

# no match since file can't exist (warn)
f <- tempfile()
out <- capture.output(
  invisible(unitizer:::infer_unitizer_location(f)), type='message'
)
any(grepl("No possible matching files", out))


unlink(tmp.dir, recursive = TRUE)

