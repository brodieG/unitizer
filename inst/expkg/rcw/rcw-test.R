# load function

source('rcw-code.R', local=TRUE)

# create matrix to test rotation

mx <- matrix(c(1:3, rep(0, 6)), 3)

rcw(mx)

# full rotation should get us to original

identical(mx, rcw(rcw(rcw(rcw(mx)))))

# non-square matrix

mx2 <- matrix(c(1:3, rep(0, 3)),  ncol=2)
rcw(mx2)

# Invalid input

rcw(1:10)
