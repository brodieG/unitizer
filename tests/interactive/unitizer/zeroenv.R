
unitizer_sect("Global Env Vars Not Visible", {
  if(!inherits(try(get(".unitizer.test.val", .GlobalEnv)), "try-error"))
    stop("`.unitizer.test.val` should not be defined")
  assign(".unitizer.test.val", 42, .GlobalEnv)
  .unitizer.test.val   # should cause an error
  rm(.unitizer.test.val, envir=.GlobalEnv)
})
unitizer_sect("Non-loaded libraries not visible", {
  library(unitizerdummypkg2)
  dummy_fun2()  # returns NULL
  dummy_fun1()  # error: not found
})
