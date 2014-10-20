# Designed to cause errors on purpose

tracingState(FALSE)
library(unitizerdummypkg1)
tracingState(TRUE)  # turn back on because this persists
