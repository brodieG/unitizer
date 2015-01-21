# For random tests

x <- 200
y <- 30
unitizer_sect("blah", compare=unitizerItemTestsFuns(value=identical, output=all.equal),
{{
  #message(sample(1:5), appendLF = FALSE)
  warning(runif(1))
  if(runif(1) > .5) warning(sample(1:5))
  matrix(runif(1:60), ncol=2)
}})

