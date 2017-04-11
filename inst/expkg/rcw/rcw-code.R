# Fun to Rotate Matrix Clockwise

rcw <- function(mx) {
  if(!is.matrix(mx)) stop("Argument `mx` is not matrix.")
  nr <- dim(mx)[1]
  nc <- dim(mx)[2]
  res <- matrix(mx[0], nrow=nr, ncol=nc)

  for(i in seq_len(nrow(mx)))
    for(j in seq_len(ncol(mx)))
      res[j, nr - i + 1] <- mx[i, j]
  res
}








































# Changes and modifications to code above:
# 1. Fix nrow=nr to swith cols and rows
# 2. Switch to t.mx <- t(mx) approch, but forget stopifnot
# 3. Add the proper re-ordering

if(FALSE) {
  "
  rm -r ~/repos/rcw/*
  cp -r ~/repos/unitizer/inst/expkg/rcw/* ~/repos/rcw
  "

  rcw2 <- function(mx) {

  if(!is.matrix(mx)) stop("Argument `mx` is not matrix.")
  t.mx <- t(mx)
  t.mx[] <- t.mx[order(-col(t.mx))]  # preserves dims
  t.mx

  }
}
