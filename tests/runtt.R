if(require(testthat)) {
  # used to be in here, but had to move to see error messages under travis

  source('testthat/helper/runtt.R', local=TRUE)
} else {
  warning("Cannot run tests without `testthat` available")
}

