#' Alter Older unitizer Versions So They Pass Validation in Latest Version
#' 
#' Sequentially applies all applicable patches
#' 
#' @export
#' @param object an unitizer object
#' @param ... other arguments
#' @return an upgraded unitizer object

setMethod("upgrade", "unitizer", valueClass="unitizer",
  function(object, ...) {
    stop("No upgrades defined")
} )