if(require(testthat)) {
  # used to be in here, but had to move to see error messages under travis

  RNGversion("3.5.2");
  source('testthat/helper/runtt.R', local=TRUE)
  RNGversion(as.character(getRversion()))
} else {
  warning("Cannot run tests without `testthat` available")
}

