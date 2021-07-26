# Temporary dirs, etc., to cleanup on exit.  See _helper/pkgs.R for their
# use.

TMP.DIR <- tempfile()
TMP.LIB <- file.path(TMP.DIR, 'utz-tmp-lib')
if(!isTRUE(dir.create(TMP.DIR)))
  stop("Unable to create temp directory")
if(!isTRUE(dir.create(TMP.LIB)))
  stop("Unable to create temp library directory")

TMP.PKGS <- c(
  unitizerdummypkg1="unitizerdummypkg1",
  unitizerdummypkg2="unitizerdummypkg2",
  utzflm="flm0"
)
UNITIZER.DIR <- system.file(package="unitizer")
PKG.DIRS <- file.path(UNITIZER.DIR, "expkg", TMP.PKGS)
USE.PKGS <- FALSE

# Global options

options(
  useFancyQuotes=FALSE,
  unitizer.tmp.lib.loc=TMP.LIB,
  unitizer.restarts.ok=TRUE,
  unitizer.state='recommended',
  diffobj.pager='off',
  unitizer.show.progress=FALSE
)
if(isTRUE(getOption("showErrorCalls"))) options(showErrorCalls=FALSE)
library(unitizer)

suppressWarnings(RNGversion("3.5.2"));

# Cleanup on exit

FIN.ENV <- new.env()
reg.finalizer(
  FIN.ENV,
  function(e) {
    if(isTRUE(USE.PKGS))  {
      for(i in names(TMP.PKGS)) {
        try(detach(sprintf("package:%s", i)), silent=TRUE)
        try(unloadNamespace(i), silent=TRUE)
      }
      suppressWarnings(remove.packages(names(TMP.PKGS), lib=TMP.LIB))
    }
    unlink(TMP.DIR, recursive=TRUE)
    writeLines('CLEANUP COMPLETE')
  },
  onexit=TRUE
)
# misc helper funs

coi <- function(x) invisible(capture.output(x))


