source(file.path("_helper", "init.R"))

# - "random seed" --------------------------------------------------------------

dir <- file.path(TMP.DIR, "randdir")
dir.create(dir)
file <- file.path(dir, "randtest.R")
cat("sample(1:100)\n", file = file)
set.seed(1)
coi(unitize(file, auto.accept = "new"))
# changing seed should have no effect on result
set.seed(23)
coi(res <- unitize(file))
# expect_equal(as.character(res$status), "Passed")
as.character(res$status)

