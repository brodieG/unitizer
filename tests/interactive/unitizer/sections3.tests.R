
unitizer_sect("I care about screen output", compare=new("unitizerItemTestsFuns", output=all.equal), {
  sqrt(2) + 3 ^ 5
  print(paste0("heya there how's it going", sample(1:1000, 1)))
  {print(paste0("heya there how's it going", sample(1:1000, 1))); NULL}
} )

f <- factor(letters[1:3])
