unitizer_sect("basic tests", {
  1 + 1 + 1/10^16
  print("heya")
  unitizer_sect("Nested", {
    3 + 5
    matrix(sample(1:9), nrow=3)
  } )
  stop("wow")
} )
unitizer_sect("strict tests", compare=identical, {
  1 + 2 + 1/10^16
  print("heya there")
  warning("wow")
} )
unitizer_sect("I care about screen output", compare=new("unitizerItemTestsFuns", output=all.equal), {
  sqrt(2) + 3 ^ 5
  print(paste0("heya there how's it going", sample(1:1000, 1)))
} )
unitizer_sect("Compare error", compare=sample, {
  6 + 2 + 1/10^16
  matrix(1:9, nrow=3)
  message("random wow")
} )
