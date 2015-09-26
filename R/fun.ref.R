# List of functions we intend to shim, original values so we can make sure they
# haven't changed before we try to shim them.
#
# Code is copied directly from R 3.2.2

.unitizer.base.funs.ref <- list(

  # WARNING WARNING WARNING: if you change these make sure you check the shim dat
  # to make sure tracers are introduced in right place!!!!!

  library=
    eval(
      parse(
        file=system.file(package="unitizer", file.path("refsource", "library.R")),
        keep.source=FALSE
    ) ),
  attach=
    eval(
      parse(
        file=system.file(package="unitizer", file.path("refsource", "attach.R")),
        keep.source=FALSE
    ) ),
  detach=
    eval(
      parse(
        file=system.file(package="unitizer", file.path("refsource", "detach.R")),
        keep.source=FALSE
    ) )
)

