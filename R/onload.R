.onLoad <- function(x, y) {
  options(testor.show.output=FALSE)  
  options(testor.disable.capt=FALSE)  
}
.onUnload <- function(x) {
  options(testor.show.output=NULL) 
  options(testor.disable.capt=NULL)  
}

