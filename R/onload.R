.onLoad <- function(x, y) {
  options(testor.show.output=FALSE)         # Will display output/msg to stdout/stderr in addition to capturing it
  options(testor.disable.capt=FALSE)        # Will prevent capture
  options(testor.test.out.lines=c(50, 15))  # How many lines to display when showing test values, or truncate to if exceeds
  options(testor.test.msg.lines=c(10, 3))   # How many lines to display when showing test errors, or truncate to if exceeds
}
.onUnload <- function(x) {
  options(testor.show.output=NULL) 
  options(testor.disable.capt=NULL)  
  options(testor.test.out.lines=NULL)
  options(testor.test.msg.lines=NULL)  
}

