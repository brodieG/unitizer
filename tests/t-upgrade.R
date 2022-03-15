source(file.path("_helper", "init.R"))
blat_vers <- function(x) sub("'\\d+(?:\\.\\d+)*'", "'<version>'", x)

# - "Upgrade works" ------------------------------------------------------------

# this is also now tested as part of load
unitizer <-
  get_unitizer(file.path("_helper", "unitizers", "trivial.unitizer.0.4.2"))
try(validObject(unitizer, complete = TRUE))
as.character(unitizer@version)
unitizer.up <- unitizer:::upgrade_internal(unitizer) # warning
validObject(unitizer.up)
identical(unitizer.up@version, as.character(packageVersion("unitizer")))

# - Upgrade Warnings in Unitize ------------------------------------------------

tdir <- tempfile()
dir.create(tdir)
dir.create(file.path(tdir, "trivial.unitizer"))
file.copy(file.path("_helper", "unitizers", "trivial.R"), tdir)
file.copy(
  file.path("_helper", "unitizers", "trivial.unitizer.0.4.2", "data.rds"),
  file.path(tdir, "trivial.unitizer")
)
odir <- setwd(tdir)
unitizer:::read_line_set_vals('N')
out <- unitizer:::capture_output(
  try(unitize(file.path(tdir, "trivial.R"), interactive.mode=TRUE))
)
out[] <- lapply(out, blat_vers)
out

unitizer:::read_line_set_vals(c('Y','Q'))
out <- unitizer:::capture_output(
  unitize(file.path(tdir, "trivial.R"), interactive.mode=TRUE)
)
out[] <- lapply(out, blat_vers)
out
unitizer:::read_line_set_vals(NULL)
setwd(odir)
unlink(tdir, recursive=TRUE)

# - Upgrade Multiple Unitizers Unitize -----------------------------------------

tdir <- tempfile()
dir.create(tdir)
dir.create(file.path(tdir, "trivial1.unitizer"))
dir.create(file.path(tdir, "trivial2.unitizer"))
file.copy(
  file.path("_helper", "unitizers", "trivial.R"),
  file.path(tdir, c("trivial1.R", "trivial2.R"))
)
file.copy(
  file.path("_helper", "unitizers", "trivial.unitizer.0.4.2", "data.rds"),
  file.path(tdir, "trivial1.unitizer")
)
file.copy(
  file.path("_helper", "unitizers", "trivial.unitizer.0.4.2", "data.rds"),
  file.path(tdir, "trivial2.unitizer")
)
odir <- setwd(tdir)
unitizer:::read_line_set_vals(c('Y','Q'))
out <- unitizer:::capture_output(
  unitize_dir(tdir, interactive.mode=TRUE)
)
out[] <- lapply(out, blat_vers)
out
unitizer:::read_line_set_vals(NULL)
setwd(odir)
unlink(tdir, recursive=TRUE)

# - "Rename" -------------------------------------------------------------------

setClass("untzUpgrTest", slots = c(a = "character"))
x <- new("untzUpgrTest", a = letters)
validObject(x)
setClass("untzUpgrTest", slots = c(b = "character"))
try(validObject(x))
try(capture.output(unitizer:::renameSlot(x, "c", "b"), type = "message"))
x.rename <- unitizer:::renameSlot(x, "a", "b")
validObject(x.rename)

# - "Later but valid version" --------------------------------------------------

test.file <- file.path(TMP.DIR, "tests.R")
cat("1 + 1", file = test.file)
unitizer:::capture_output(unitize(test.file, auto.accept = "new"))
version <- unlist(strsplit(as.character(packageVersion("unitizer")),
    ".", fixed = TRUE))
version[1] <- as.character(as.numeric(version[1]) + 1)
version.new <- paste0(version, collapse = ".")
unitizer.rds <- readRDS(file.path(TMP.DIR, "tests.unitizer", "data.rds"))
unitizer.rds@version <- version.new
# this should work
!nchar(unitizer:::unitizer_valid(unitizer.rds))
# now lets cause an error
unitizer.rds@eval.time <- runif(5)
grepl("NB: ", unitizer:::unitizer_valid(unitizer.rds))

# - "Failing Test w/ Upgrade" --------------------------------------------------

# Unitizer will fail, but also requires an upgrade.  This ensures the failure is
# shown despite the need for an upgrade.
tdir <- tempfile()
dir.create(tdir)
dir.create(file.path(tdir, "fail-and-upgrade.unitizer"))
file.copy(file.path("_helper", "unitizers", "fail-and-upgrade.R"), tdir)
file.copy(
  file.path("_helper", "unitizers", "fail-and-upgrade.unitizer", "data.rds"),
  file.path(tdir, "fail-and-upgrade.unitizer")
)
odir <- setwd(tdir)
try(unitize(file.path("fail-and-upgrade.R")))

# Confirm upgrade needed
capture.output(unitizer:::read_line_set_vals(c('Y', 'Q')))
out <- unitizer:::capture_output(
  unitize(file.path("fail-and-upgrade.R"), interactive.mode=TRUE)
)
out[] <- lapply(out, blat_vers)
out

unitizer:::read_line_set_vals(NULL)
setwd(odir)
unlink(tdir, recursive=TRUE)

