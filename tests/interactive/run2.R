library(testor)

runtests("tests/interactive/testor/sections.tests.R")
runtests("tests/interactive/testor/sections2.tests.R", "tests/interactive/testor/sections.tests.rds")
runtests("tests/interactive/testor/sections3.tests.R", "tests/interactive/testor/sections.tests.rds")