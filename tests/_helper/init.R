# Temporary dirs, etc., to cleanup on exit.  See _helper/pkgs.R for their
# use.

START.DIR <- getwd()
TMP.DIR <- tempfile()
TMP.LIB <- file.path(TMP.DIR, 'utz-tmp-lib')
if(!isTRUE(dir.create(TMP.DIR)))
  stop("Unable to create temp directory")
if(!isTRUE(dir.create(TMP.LIB)))
  stop("Unable to create temp library directory")

USE.PKGS <- FALSE

# Global options

options(
  useFancyQuotes=FALSE,
  unitizer.tmp.lib.loc=TMP.LIB,
  unitizer.state='recommended',
  diffobj.pager='off',
  unitizer.show.progress=FALSE,
  unitizer.color = FALSE,
  unitizer.transcript = FALSE,
  width = 80L,
  crayon.enabled = FALSE,
  diffobj.term.colors = 1,
  digits=3,
  warn=1,
  aammrtf.ref.objs=file.path("_helper/ref-objs")
)
if(isTRUE(getOption("showErrorCalls"))) options(showErrorCalls=FALSE)
library(unitizer)

suppressWarnings(RNGversion("3.5.2"));

# Cleanup on exit; no output here or Rdiff will include timing

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
  },
  onexit=TRUE
)
# misc helper funs

coi <- function(x) invisible(capture.output(x))


