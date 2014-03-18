# Random out of section test

1 + 1
runif(10) # truly Random

# This comment should be lost

testor_sect("first section", {
  matrix(1:9, 2)   # my first matrix
  # A more complex object
  structure(list(1, 2, 3), class="wow")  # with two comments
} )

testor_sect("second section", {
  # sampling for fun
  sample(1:10) 
  # Some screen output
  print("how do you do cowboy")
} )