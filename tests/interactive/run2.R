library(unitizer)

unitize("tests/interactive/unitizer/sections.R")
unitize("tests/interactive/unitizer/sections2.R", "tests/interactive/unitizer/sections.unitizer")
unitize("tests/interactive/unitizer/sections3.R", "tests/interactive/unitizer/sections.unitizer")
