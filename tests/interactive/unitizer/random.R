# For random tests

x <- 200
y <- 30
{
  message(sample(1:5))
  matrix(runif(1:30), ncol=5)
  warning(runif(1))
}

