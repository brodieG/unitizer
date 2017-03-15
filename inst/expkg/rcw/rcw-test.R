source('rcw-code.R')

mx <- matrix(1:9, 3)

rcw(mx)
rcw(rcw(mx))
rcw(rcw(rcw(mx)))
rcw(rcw(rcw(rcw(mx))))

mx2 <- matrix(1:6, 2)

rcw(mx2)

