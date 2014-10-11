library(testthat)

try(detach("package:unitizer", unload=TRUE))
try(detach("package:unitizerdummypkg1", unload=TRUE))
try(detach("package:unitizerdummypkg2", unload=TRUE))

library(unitizer)

# Package added properly

search.path <- search()
unitizer:::unitizer_library(unitizerdummypkg1)
expect_equal(new.pack <- setdiff(search(), search.path), "package:unitizerdummypkg1")
expect_equal(new.pack, names(unitizer:::pack.env$objects.attached))

# Package added only once

unitizer:::unitizer_library(unitizerdummypkg1)
expect_equal(new.pack <- setdiff(search(), search.path), "package:unitizerdummypkg1")
expect_equal(new.pack, names(unitizer:::pack.env$objects.attached))

# Don't add packages that are already in search

library(unitizerdummypkg2)
search.path <- search()
unitizer:::unitizer_library(unitizerdummypkg2)
expect_equal(new.pack <- setdiff(search(), search.path), character())

# Check that require also works

try(detach("package:unitizerdummypkg2", unload=TRUE))

unitizer:::unitizer_require(unitizerdummypkg2)
search.path <- search()
expect_equal(new.pack <- setdiff(search(), search.path), "package:unitizerdummypkg2")
expect_equal(new.pack, names(unitizer:::pack.env$objects.attached))
expect_equal(
  c("package:unitizerdummypkg1", "package:unitizerdummypkg2"), 
  names(unitizer:::pack.env$objects.attached)
)

# Check search path trimming

try(detach("package:unitizer", unload=TRUE))
try(detach("package:unitizerdummypkg1", unload=TRUE))
try(detach("package:unitizerdummypkg2", unload=TRUE))

library(unitizer)
library(unitizerdummypkg1)  # at least one pack to explicitly remove

search.pre <- search()

testthat::expect_identical(unitizer:::search_path_trim(), NULL)  # note this actually removes testthat from search path...
testthat::expect_equal(
  search(), 
  c(
    ".GlobalEnv", "package:unitizer", "package:stats", "package:graphics",  
    "package:grDevices", "package:utils", "package:datasets", "package:methods",  
    "Autoloads", "package:base"
) )
testthat::expect_true(
  "package:unitizerdummypkg1" %in% 
  vapply(unitizer:::pack.env$objects.detached, `[[`, "", "name")
)

# Reset search path

unitizer:::search_path_restore()
expect_identical(search.pre, search())



