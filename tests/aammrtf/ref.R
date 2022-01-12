# Copyright (C) 2022 Brodie Gaslam

# This file is part of "aammrtf - An Almost Most Minimal R Test Framework"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 or 3 of the License.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

## Generate Reference Object Accessor Functions
##
## Helper functions to simplify reading and writing reference files.  You will
## need to create the target directory, which by default is "aammrtf-ref-objs".
## The expectation is that this will be a subdirectory to "pkg/tests".
##
## @param name character(1) a name to use as a subfolder under `obj.dir`.
## @param obj.dir character(1L) directory to reference objects in.
## @return a list of reading ("rds", "txt"), and writing functions, ("rds_save",
##   "txt_save").
## @examples
## ## Load the functions into env so we can access them directly.
## list2env(make_file_funs("myfile"), environment())
## ## Test against stored RDS
## # rds_save(my_fun(), "my_fun_out")            # previously stored value
## all.equal(my_fun(), rds("my_fun_out"))

make_ref_obj_funs <- function(
  name,
  obj.dir=getOption("aammrtf.ref.objs", file.path("aammrtf-ref-objs")),
  env=parent.frame()
) {
  dir <- file.path(getwd(), obj.dir)
  if(!file_test('-d', dir))
    stop("`dir` (", dir, ") is not a directory or does not exist.")
  res <- list(
    rds=
      function(x) {
        old.dir <- setwd(dir); on.exit(setwd(old.dir))
        readRDS(file.path(dir, name, sprintf("%s.rds", x)))
      },
    rds_save=
      function(x, i) {
        old.dir <- setwd(dir); on.exit(setwd(old.dir))
        saveRDS(
          x,
          file.path(name, sprintf("%s.rds", i)), version=2
        )
      },
    txt=
      function(x) {
        old.dir <- setwd(dir); on.exit(setwd(old.dir))
        readLines(file.path(name, sprintf("%s.txt", x)))
      },
    txt_save=
      function(x, i) {
        old.dir <- setwd(dir); on.exit(setwd(old.dir))
        writeLines(
          x,
          file.path(NAME, sprintf("%s.txt", i))
        )
      }
  )
  if(
    any(
      c('rds', 'rds_save', 'txt', 'txt_save') %in% ls(env)
    )
  ) {
    warning('target objects already defined, not writing them to `env`')
  } else list2env(res, env)
  invisible(res)
}

