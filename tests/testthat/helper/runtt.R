# This used to be directly in `runtt.R`, but made it impossible to see any
# output from test failures.  So now we source this

# Install test packages, and set them up for removal, here in package name
# =dirname format

local({
  library(methods)
  library(unitizer)

  cat("setup packages\n")
  # Ensure same behavior interactive and non-interactive

  # Run tests

  test.filt <- paste(sep="|",
    "browse",
    "capture",
    "change",
    "demo",
    "error",

    "exec",
    "get",
    "global",
    "handledruns",
    "inpkg",
    "ischecks",
    "item",

    "list",
    "misc",

    "options",
    "parse",
    "prompt",
    "rename",

    "repairenvs",
    "random",
    "search",
    "section",
    "shim",

    "state",
    "text",
    "translate",
    "utz1",
    "utz2",
    "upgrade",
    "zzrunlast",
    "not_a_real_test_file"
  )
  # test.filt <- 'exec'
  test.res <- if(packageVersion('testthat') > "1.0.2") {
    test_dir(
      "testthat",
      # reporter="check",
      env=environment(),
      filter=test.filt,
      wrap=FALSE
    )
  } else {
    test_dir("testthat", reporter="check", env=environment(), filter=test.filt)
  }
  # Check for failures and throw error if they exist since test_dir does not
  # do so on its own

  # # comment out since with "check" reporter this is moot
  # with(
  #   as.data.frame(test.res), {
  #     fail <- sum(failed)
  #     err <- sum(error)
  #     if(fail != 0 || err) stop("Errors: ", err, " Failures: ", fail)
  # })
})

