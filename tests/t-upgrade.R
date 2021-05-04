source(file.path("_helper", "init.R"))

# - "Upgrade works" ------------------------------------------------------------

# this is also now tested as part of load
unitizer <- get_unitizer(file.path("_helper", "trivial.unitizer.0.4.2"))
try(validObject(unitizer, complete = TRUE))
as.character(unitizer@version)
unitizer.up <- unitizer:::upgrade_internal(unitizer) # warning
validObject(unitizer.up)
identical(unitizer.up@version, as.character(packageVersion("unitizer")))

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

