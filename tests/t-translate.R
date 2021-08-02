source(file.path("_helper", "init.R"))
source(file.path("aammrtf", "ref.R")); make_ref_obj_funs("refobjs")

test.file <- file.path(
  "_helper", "ref-objs", "translate", "testthat", "test-translate2.R"
)
target.dir.base <- file.path(TMP.DIR, basename(tempfile()))
target.dir <- file.path(target.dir.base, "helper", "translate", "unitizer")

# - "Prompt to create dir" -----------------------------------------------------

try(
  testthat_translate_file(
    test.file, target.dir, prompt = "always", interactive.mode = FALSE
) )
# translations have to be outside of `testthat`; second translation should fail
# except we allow manual input

# - "translate a file" ---------------------------------------------------------

unitizer:::capture_output({
    unitizer:::read_line_set_vals(c("Y"))
    res1 <- testthat_translate_file(test.file, target.dir, prompt = "always",
        interactive.mode = TRUE)
    res1.txt <- readLines(res1)
    unitizer:::read_line_set_vals(c("Y"))
    res2 <- testthat_translate_file(test.file, target.dir, prompt = "overwrite",
        interactive.mode = TRUE)
    res2.txt <- readLines(res2)
    unitizer:::read_line_set_vals(NULL)
})
dummy <- new("unitizerDummy")

all.equal(res1.txt, rds("translate_res1"))
all.equal(res1.txt, res2.txt)

# Can't do this twice in a row without prompting in non-interactive mode
# note test above does work because we use interactive mode to accept prompt

any(
  grepl(
    "already exists",
    capture.output(
      try(
        testthat_translate_file(
          test.file, target.dir, prompt = "always", interactive.mode = FALSE
      ) ),
      type='message'
) ) )
untz <- get_unitizer(file.path(target.dir, "translate2.unitizer"))
all.equal(untz@items.ref.calls.deparse, rds("translate_res2"))

lapply(unitizer:::as.list(untz@items.ref), function(x) x@data@value[[1L]])
unlink(target.dir, recursive = TRUE)

target.dir.base <- file.path(TMP.DIR, basename(tempfile()))
target.dir <- file.path(target.dir.base, "helper", "translate", "unitizer")

test.dir <- file.path("_helper", "ref-objs", "translate", "testthat")
target.dir <- file.path(target.dir.base, "_helper", "translate", "unitizer")

# - "translate a dir" ----------------------------------------------------------

unitizer:::capture_output(res2 <- testthat_translate_dir(test.dir, target.dir))
all.equal(lapply(res2, readLines), rds("translate_res3"))
untz <- get_unitizer(file.path(target.dir, "translate2.unitizer"))
all.equal(untz@items.ref.calls.deparse, rds("translate_res4"))

# Note not the same as when we did just the single file because the helper
# file is loaded so `fun0` and `fun1` are actually defined
lapply(unitizer:::as.list(untz@items.ref), function(x) x@data@value[[1L]])

# Can't do it again since there are files there
any(
  grepl(
    "safety feature to ensure files are not accidentally overwritten",
    capture.output(
      try(testthat_translate_dir(test.dir, target.dir)), type='message'
) ) )

