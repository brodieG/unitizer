#' Alter Older Testor Versions So They Pass Validation in Latest Version
#' 
#' Sequentially applies all applicable patches
#' 
#' @param object an testor object
#' @param return an upgraded testor object

setMethod("upgrade", "testor", valueClass="testor",
  function(object, ...) {
    if(object@version < "0.3.2") {
      changes <- new("testorChanges",
        failed=object@changes@failed,
        new=object@changes@new,
        removed=object@changes@removed,
        corrupted=object@changes@error   # this is the change
      )
      object@changes <- changes
    }
    object
} )