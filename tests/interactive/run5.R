# Need to do this way so we can make sure that dummypkg1 is not visible within
# the unitizer evaluation

library(unitizer)
library(unitizerdummypkg1)

unitize("tests/interactive/unitizer/zeroenv.R", search.path.clean=TRUE)

detach("package:unitizerdummypkg1", unload=TRUE)

unitize("tests/interactive/unitizer/zeroenv.fail.R", search.path.clean=TRUE)
