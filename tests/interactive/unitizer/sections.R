# Unsectioned tests

matrix(1:12, nrow=3)

# Sectioned tests

unitizer_sect("basic tests", {
  1 + 3 + runif(1) / 1e8
  a <- 25
  print("heya")
  stop("wow")
} )
unitizer_sect("strict tests with identical", compare=identical, {
  1 + 2 + runif(1) / 1e8
  print("heya there")
  warning("wow")
} )
unitizer_sect(
  "I care about screen output and I am a super super duper long section title for testing purposes blah blah blah",
  compare=new("unitizerItemTestsFuns", output=all.equal),
  {
    x <- y <- list(1, letters[1:5], NULL)
    sqrt(2) + 3 ^ 5
    {
      print(paste0("heya there how's it going", sample(1:1000, 1)))
      NULL
    }
    p1 <- runif(5)
    p2 <- p1 * 3
    p3 <- 0 * p2 + 1:10
} )
unitizer_sect("Compare error", compare=sample, {
  6 + 2 + 1/10^16
  matrix(1:9, nrow=3)
  message("random wow")
  cause.trouble <- 42
} )
