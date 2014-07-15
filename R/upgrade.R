#' Alter Older unitizer Versions So They Pass Validation in Latest Version
#' 
#' Sequentially applies all applicable patches
#' 
#' @param object an unitizer object
#' @param return an upgraded unitizer object

setMethod("upgrade", "unitizer", valueClass="unitizer",
  function(object, ...) {
    stop("No upgrades defined")
} )