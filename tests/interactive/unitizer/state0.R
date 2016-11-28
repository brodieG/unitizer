getOption("untz.state.test")
options(untz.state.test=25)
getOption("untz.state.test")
library(testthat)
search()
getwd()
xorb            # should be missing when running with clean env
