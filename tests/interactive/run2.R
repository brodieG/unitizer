library(unitizer)

unitize("tests/interactive/unitizer/sections.tests.R")
unitize("tests/interactive/unitizer/sections2.tests.R", "tests/interactive/unitizer/sections.tests.rds")
unitize("tests/interactive/unitizer/sections3.tests.R", "tests/interactive/unitizer/sections.tests.rds")