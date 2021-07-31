# Setup helper packages used in some tests

USE.PKGS <- TRUE
writeLines("Install Packages")

TMP.PKGS <- c(
  unitizerdummypkg1="unitizerdummypkg1",
  unitizerdummypkg2="unitizerdummypkg2",
  utzflm="flm0"
)
UNITIZER.DIR <- system.file(package="unitizer")
PKG.DIRS <- file.path(UNITIZER.DIR, "expkg", TMP.PKGS)

if(
  any(which.inst <- names(TMP.PKGS) %in% rownames(installed.packages()))
) {
  stop(
    "Packages\n",
    paste0(
      deparse(names(TMP.PKGS)[which.inst], width.cutoff=500), collapse=""
    ),
    "\nalready installed; cannot proceed with tests"
) }
# install.packages does not work within R CMD check, and it does not
# appear to be by design?

inst_pak <- function(pkg) {
  old.val <- Sys.getenv("R_TESTS", unset=NA)
  on.exit(
    if(is.na(old.val)) Sys.unsetenv("R_TESTS")
    else Sys.setenv(R_TESTS=old.val)
  )
  Sys.setenv(R_TESTS="")
  pkg.inst <- try(
    install.packages(pkg, repos=NULL, type='src', lib=TMP.LIB, quiet=TRUE)
  )
  if(inherits(pkg.inst, "try-error")) stop("install error")
}
inst_pak(PKG.DIRS)

writeLines("Setup Demos")

# Setup the demo files used by a number of tests.  All references
# should be relative to FLM (i.e. start with (setwd(FLM)).  This
# will avoid the temp file of the directory showing up in the files.

FLM <- copy_fastlm_to_tmpdir()
FLM.TEST.DIR <- file.path(FLM, "tests", "unitizer")
FLM.TEST.FILE <- file.path(FLM.TEST.DIR, "fastlm1.R")
FLM.TEST.STORE <- file.path(FLM.TEST.DIR, "fastlm1.unitizer")

