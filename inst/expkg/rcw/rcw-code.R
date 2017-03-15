# Rotate Matrix Clockwise

rcw <- function(mx) {
  stopifnot(is.matrix(mx))
  nc <- ncol(mx)
  nr <- nrow(mx)
  res <- matrix(mx[0], nrow=nr, ncol=nc)

  for(i in seq_len(nrow(mx)))
    for(j in seq_len(ncol(mx)))
      res[j, nr - i + 1] <- mx[i, j]
  res
}




















rcw2 <- function(mx) {
  stopifnot(is.matrix(mx))
  nc <- ncol(mx)
  nr <- nrow(mx)
  res <- matrix(mx[0], nrow=nc, ncol=nr)

  for(i in seq_len(nrow(mx)))
    for(j in seq_len(ncol(mx)))
      res[j, nr - i + 1] <- mx[i, j]
  res
}

rcw3 <- function(mx) {
  stopifnot(is.matrix(mx))
  t.mx <- t(mx)
  "[<-"(t.mx, t.mx[order(-col(mx))])
  # matrix(t(mx)[order(-col(mx))], nrow=ncol(mx))
}
rcw4 <- function(mx) {
  stopifnot(is.matrix(mx))
  t.mx <- t(mx)
  matrix(t(mx)[order(-col(mx))], nrow=ncol(mx))
}
